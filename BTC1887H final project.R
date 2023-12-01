###################################################
##       BTC1877H Data Science in Health II      ##
##              Final Project: Code              ##
##  Leen Madani, Alanna Olteanu, Nishant Sarkar  ##
###################################################

# Loading required packages: 
library(readxl)
library(naniar)
library(mice)
library(ggplot2)
library(dplyr)
library(funModeling)
library(mice)
library(survival)
library(car)
library(funModeling)
library(glmnet)


##########################################
#                OVERVIEW                #      
##########################################

# Blah bloo bleee fill this in later
# Q1 Q2 

################### 1ST Q ASKS: What are the characteristics of patients that require transfusions, and what are the factors influencing the need and amount of transfusions?
### Need to consider variables that describe patient characteristics and factors that might influence the need for a transfusion
### Relevent sections include: 
# Orange (Patient Demographic Data)
# Green (Underlying Respiratory Diagnosis + Intraoperative Descriptions): patient's respiratory health status and details about their lung transplant surgery, which could influence the need for transfusions
# Yellow (Blood Product Transfusion Data)


################# 2nd Q ASKS: What is the impact of transfusion on patient outcomes, including mortality?
## Need to analyze the relationship between transfusion data and patient outcomes, so we need:
# Yellow (Blood Product Transfusion Data)
# Blue (Survival and ICU LOS Data): Variables related to patient survival (like mortality rates at different time points post-transplant) and the length of stay in the ICU..
# Red (Preoperative Bloodwork): Information such as hemoglobin levels, platelets, etc..




##########################################
#    DATA CLEANING AND PREPROCESSING     #      
##########################################

# Loading the data set 
data <- read_excel("transfusion data.xlsx")
View(data)

# Taking a deeper look at the dataset's structure to understand its components using glimpse() & str()
str(data)
glimpse(data) # 192 patient observations and 117 columns.
# 1st column is study id
#column 112 to 115 do not make sense; no variable name and all missing; should be removed later. 

# Using make.names() function to ensure all columnS have valid names. This function will replace 
# spaces and special characters with dots (.) and ensure names are unique and syntactically valid in R.
names(data) <- make.names(names(data), unique = TRUE)


### INVESTIGATING MISSING VALUES ###

# Examining how many values are missing in the dataset.
sum(is.na(data)) # 5557 missing values.
summary(data) # To  see the number of the missing observations for each variable. 

# Checking if missing values come in other forms (ex. 99, ?, etc.):
# Applying table function to each column
results_na <- lapply(data[,1:117], table) 

# Visualizing the missing data using the Naniar package.
missing_data <- vis_miss(data) 
# 24.7% missingness in the overall dataset. 

################### 1ST Q ASKS: What are the characteristics of patients that require transfusions, and what are the factors influencing the need and amount of transfusions?
### Need to consider variables that describe patient characteristics and factors that might influence the need for a transfusion
### Relevant sections include: 
# Orange (Patient Demographic Data).
# Green (Underlying Respiratory Diagnosis + Intraoperative Descriptions): patient's respiratory health status 
# and details about their lung transplant surgery, which could influence the need for transfusions.
# Yellow (Blood Product Transfusion Data).


################# 2nd Q ASKS: What is the impact of transfusion on patient outcomes, including mortality?
## Need to analyze the relationship between transfusion data and patient outcomes, so we need:
# Yellow (Blood Product Transfusion Data)
# Blue (Survival and ICU LOS Data): Variables related to patient survival (like mortality rates at different 
# time points post-transplant) and the length of stay in the ICU.
# Red (Preoperative Bloodwork): Information such as hemoglobin levels, platelets, etc.


# Checking missingness for each variable. 
missing_before_filter <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
missing_before_filter

# for time-dependent variables, such as date of ICU discharge, missinginess might be informative in itself and needs careful consideration

# Eliminating columns with 30% or more missinginess:
# Identify these columns with less than 30% missingness
columns_to_keep <- names(which(missing_before_filter < 30))

# Selecting these columns from the original dataset (will also remove columns 112-115, which are erroneous)
filter70_data <- data %>% 
  select(all_of(columns_to_keep))

# 80 variables remaining (out of 117).

# Reassessing missingness: 
vis_miss(filter70_data) # Now we have 0.8% missingness instead of 24.7%.
missing_after <- as.data.frame(sapply(filter70_data, function(x) sum(is.na(x)) / length(x) * 100))
colnames(missing_after) <- "Percent_Missing" 
missing_after # Double checking to see missigness percentage in every variable.

# Missing values will be imputed in a later step.


### DATA RESTRUCTURING ###

# All variables should be properly encoded to reflect their nature.
# Variables like Type, Gender (male), COPD, etc., which are currently chars, should be converted to factors.
# Date.of.Extubation is encoded as a character (but should be dttm). 

###### All variables encoded as chr but should be factor: 
# Type, Gender,COPD, alpha1-Antitrypsin Deficiency, Cystic Fibrosis, Idiopathic Pulmonary Hypertension, 
# Interstitial Lung Disease, Pulm_Other, Coronary Artery Disease, Hypertension, Diabetes (insulin), Diabetes (diet/OHGs), 
# GERD/PUD, Renal Failure, Stroke/CVA, Liver Disease, Thyroid Disease, First Lung Transplant, Redo Lung Transplant, ExVIVO Lung Perfusion, 
# Preoperative ECLS, Intraoperative ECLS, ECLS_ECMO, ECLS_CPB, Protamine (Y=1 N=0), Tranexamic Acid Used, Need for reoperation for bleeding within 24h, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN

# Intraoperative.ECLS encoded as lgl but should be factor. 

# Intra_Albumin.5...mL. is chr but should be dbl (not factor)
table(filter70_data$Intra_Albumin.5...mL.)
# Also, this variable has a strange value of 0+AQ7. We will assume this should be 0.

# Date.of.Extubation column was processed incorrectly when loaded into R (dissimilar to other date/time variables).
# This should be fixed. 
filter70_data$Date.of.Extubation <- as.POSIXct(as.numeric(filter70_data$Date.of.Extubation) * (60*60*24),
                                               origin="1899-12-30", tz="UTC")

#Massive transfusion is dbl but represented as 0s and 1s, so its a binary indicator and should be fixed. 


######### Restructuring variables according to these required changes: 
filter70_data <- filter70_data %>%
  # Convert all character variables to factors, except for Intra_Albumin.5...mL.
  mutate_if(~ is.character(.) && !identical(colnames(.), "Intra_Albumin.5...mL."), as.factor) %>%
  
  # Convert Intraoperative.ECLS to factor
  mutate(Intraoperative.ECLS = as.factor(Intraoperative.ECLS)) %>%
  
  # Handle the odd value in Intra_Albumin.5...mL. and convert to numeric
  mutate(Intra_Albumin.5...mL. = ifelse(Intra_Albumin.5...mL. == "0+AQ7", "0", Intra_Albumin.5...mL.),
         Intra_Albumin.5...mL. = as.double(Intra_Albumin.5...mL.)) %>%
  # Encode massive transfusion as factor 
  mutate(Massive.Transfusion = as.factor(Massive.Transfusion))

# Double checking. 
glimpse(filter70_data)

# Analyzing each variable with missing data:

###### 1) DCD.vs.DBD which has 7.27% missingness
# this variable relates to the donor itself (not patient); different types of organ donors used in lung transplantation
table(filter70_data$DCD.vs.DBD)
# DBD: Donation after brain death involves donation of organs after the patient meets criteria for death by neurological criteria.
# DCD: Donation after cardiac death involves donation of organs after irreversible cessation of circulatory and respiratory function
# NDD: Neurological Determination of Death or brain death basically 
# FALSE: ??? (2 observations has FALSE)

# Changing the 2 FALSE encoded values to NA so they can be imputed downstream.
filter70_data <- filter70_data %>% 
  mutate(DCD.vs.DBD = ifelse(DCD.vs.DBD == "FALSE", NA, DCD.vs.DBD))

table(filter70_data$DCD.vs.DBD) # we have 119 DBD, 45 DCD, 12 NDD

########## LAS.score 6.25%
# it is a lung allocation score; used with blood type and the distance between the candidate and the donor hospital to determine priority for receiving a lung transplant
# it is important because if we have a higher score, then this means the patient is in urgent need for an organ and probably impacts their need for blood transfusion.

######### Pre_PTT (partial thromboplastin time) 0.52%
# time it takes for a clot to form in a blood sample
# imp as could influence need of transfusion

####### Blood.Loss 1.042 % missing 
table(filter70_data$Blood.Loss) # looks good in terms of legitimate values and its a necessary variable
# same for urine output and fluid balance

####### ICU.Discharge.Date.Time 0.52% & Duration.of.ICU.Stay..days. 0.52%
table(filter70_data$ICU.Discharge.Date.Time)
table(filter70_data$Duration.of.ICU.Stay..days.) # looks fine 
# also missingness in discharge time (which is one observation) is most likely MNAR because patient might have died, so no discharge 

#####PostImmediate_PTT 1.042%, PostImmediate_Creatinine 1.56%
# these can be used to assess patient need for transfusion 


### OTHER DATA CLEANING ###

##### Some variables are represented twice in the dataset, with different missingness:

# ICU.Admission.Date.Time 0.0000000
# ICU.Discharge.Date.Time 0.5208333
# Duration.of.ICU.Stay..days. 0.5208333
# Date.of.Extubation 0.0000000
# Duration.of.Ventilation 30.2083333

# ICU.Admit.Date.Time 16.6666667
# ICU.Discharge.Date.Time.1 17.7083333
# Duration.of.ICU.stay..days. 1.0416667
# Extubation.Date 51.0416667
# Duration.of.Mechanical.Ventilation..days. 34.3750000


### Conducting a side by side comparison to see what the differences are. 
comparison_data <- filter70_data %>%
  select(
    ICU.Admission.Date.Time,
    ICU.Admit.Date.Time,
    ICU.Discharge.Date.Time,
    ICU.Discharge.Date.Time.1,
    Duration.of.ICU.Stay..days.,
    Duration.of.ICU.stay..days.) 

# The latter set will be removed as they have more missing values, but otherwise seem to be the same.
filter70_data <- filter70_data %>%
  select(
    -ICU.Admit.Date.Time,
    -ICU.Discharge.Date.Time.1,
    -Duration.of.ICU.stay..days.)

glimpse(filter70_data) # 77 variables remaining. 


##### It appears that some variables have negative values; these should be fixed where necessary.
# Check for negative values in each column
negative_counts <- sapply(filter70_data, function(x) sum(x < 0, na.rm = TRUE))

# Display columns with negative values
negative_counts[negative_counts > 0]

####### PostImmediate_PTT,PostDay1_PTT have negative values which are not clinically meaningful 
# lets make these negative values as NA 
filter70_data <- filter70_data %>%
  mutate(
    PostImmediate_PTT = ifelse(PostImmediate_PTT < 0, NA, PostImmediate_PTT),
    PostDay1_PTT = ifelse(PostDay1_PTT < 0, NA, PostDay1_PTT)
  )


### DATA PREPROCESSING COMPLETED ###

glimpse(filter70_data)
colnames(filter70_data)

