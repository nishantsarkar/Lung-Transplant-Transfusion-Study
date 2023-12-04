###################################################
##       BTC1877H Data Science in Health II      ##
##              Final Project: Code              ##
##  Leen Madani, Alanna Olteanu, Nishant Sarkar  ##
###################################################

# Loading required packages: 
library(readxl)
library(naniar)
library(ggplot2)
library(dplyr)
library(funModeling)
library(mice)
library(survival)
library(car)
library(funModeling)
library(glmnet)
library(survminer)


##########################################
#                OVERVIEW                #      
##########################################

# This code contains the complete processing and analysis of the "transfusion data" dataset, 
# for the Team Project in BTC1877H Data Science in Health II. 

################### 1ST Q ASKS: What are the characteristics of patients that require transfusions, and what are the factors influencing the need and amount of transfusions?
### Need to consider variables that describe patient characteristics and factors that might influence the need for a transfusion
### Relevant sections in the dataset include: 
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

#################################
#  IMPUTATION AND COLLINEARITY  #                   
#################################

# First, initial variable selection was done on the basis of a literature review.
# Please see attached report for more information.

# First data frame contains "Pre" variables for patients BEFORE surgery, to answer QUESTION 1. 
View(filter70_data)
Pre_df <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", 
                          "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", 
                          "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", 
                          "First.Lung.Transplant", "Redo.Lung.Transplant", "ExVIVO.Lung.Perfusion", 
                          "Total.24hr.RBC", "Pre_Hb","Pre_Hct", "Pre_Platelets", "Pre_INR", "ECLS_ECMO", 
                          "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", 
                          "Blood.Loss", "Massive.Transfusion", "RBC.72hr.Total", "FFP.72hr.Total", 
                          "Plt.72hr.Total", "Cryo.72hr.Total")]
View(Pre_df)

# Second data frame contains "Post" variables AFTER surgery, to answer QUESTION 2. 
Post_df <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", 
                           "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", 
                           "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", 
                           "ExVIVO.Lung.Perfusion","ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "RBC.72hr.Total", "FFP.72hr.Total", 
                           "Plt.72hr.Total", "Cryo.72hr.Total", "HOSPITAL_LOS", "Duration.of.ICU.Stay..days.", "Massive.Transfusion", "Total.24hr.RBC", "Need.for.reoperation.for.bleeding.within.24h")]


## Why were these variables selected? 
# The "intra" measurements are removed because they are taken into consideration when measurement the Total amount later 
# Except for crystalloid and albumin, they are not included as part of FFP, Platelets, or RBC 
# However, we are not interested in them really: Intra_crystalloid is like a saline solution and is given as first-line to all these patients in general to prevent dehydration so won't consider it
# Intra_albumin is also given for all patients during the surgery so won't assesss their impact on patient outcomes because all are given
# "Intravenous albumin is commonly administered among noncardiac surgeries with significant inter-institutional variability" 
# Massive Transfusion Protocol (MTP): In the context of massive transfusions, the first 24 hours are critical, and there may be protocols that require reporting or analyzing this data separately.
# Why is Total.24hr.RBC higher than RBC.72hr.Total? From reviwing the literature and the dataset: 
# there might have been a massive transfusion event within the first 24 hours after the 72 hr that required more RBCs than the following days.
#  or the totals might be aggregated differently. The "total24RBC" might aggregate data from multiple sources or points, whereas the "72hr Total" might not.


# Combining the two "Transplant Type" variables into one to aid downstream analysis. 
Pre_df <- Pre_df %>% 
  mutate(Transplant_Type = ifelse(Redo.Lung.Transplant == TRUE, "SECOND", "FIRST")) %>%
  select(-First.Lung.Transplant, -Redo.Lung.Transplant)

### IMPUTATION ###

# Imputation for the rest of columns
vis_miss(Pre_df) # Missing under 0.1%
vis_miss(Post_df) # Missing under 0.2%

# Performing Stochastic Imputation
# Stochastic imputation was done due to machine learning steps downstream; see report for justification.
Pre_df <- mice(Pre_df, m = 1, method = 'pmm', seed = 123)
Pre_df <- complete(Pre_df, 1)
vis_miss(Pre_df) # No more missing values

Post_df <- mice(Post_df, m = 1, method = 'pmm', seed = 123)
Post_df <- complete(Post_df, 1)
vis_miss(Post_df) # No more missing values

# With the imputed data, defining a new variable denoting if the patient had any kind of transfusion.
Pre_df <- Pre_df %>%
  mutate(Had.Transfusion = rowSums(select(., Intra_Packed.Cells, Total.24hr.RBC, 
                                          RBC.72hr.Total, FFP.72hr.Total, 
                                          Plt.72hr.Total, Cryo.72hr.Total) > 0) > 0)


### INVESTIGATING COLLINEARITY ###

# Highly collinear variables should be removed before downstream analysis with regression models.  

# Fitting a linear model with '24hr RBC' as the dependent variable, for Pre_df. 
model <- lm(Total.24hr.RBC ~ ., data=Pre_df)
# Calculating Variance Inflation Factor (VIF)
vif_results <- vif(model)
# Variables with high collinearity are those with VIF > 5. This includes:
#   - Pre_Hb
#   - Pre_Hct
#   - Intra_Packed.Cells
#   - Blood.Loss
#   - RBC.72hr.Total
#   - Plt.72hr.Total 

# The latter two will not be used in downstream analysis and can be safely removed.
# Intra_Packed.Cells can be removed as it will be highly collinear with amount of RBCs received.
# Blood.Loss can be removed as it is not a very useful early predictor of how much transfusion a patient will need.
# Hematocrit will be removed to limit collinearity with hemoglobin.

Pre_df_1 <- Pre_df %>%
  select(-Pre_Hct, -Intra_Packed.Cells, -Blood.Loss, -RBC.72hr.Total, 
         -Plt.72hr.Total, -FFP.72hr.Total, -Cryo.72hr.Total)
# Checking VIF again: Fitting a linear model with '24hr RBC' as the dependent variable
model_1.2 <- lm(Total.24hr.RBC ~ ., data=Pre_df)
# Calculating Variance Inflation Factor (VIF)
vif_results_1.2 <- vif(model_1.2)
# Displaying VIF results
print(vif_results_1.2)
# Multicollinearity is no longer a big issue in Pre_df with these removals.
Pre_df <- Pre_df_1


# Fitting a linear model with '24hr RBC' as the dependent variable, for Post_df. 
model2 <- lm(Total.24hr.RBC ~ ., data=Post_df)
# Calculating Variance Inflation Factor (VIF)
vif_results <- vif(model2)
# Variables with high collinearity are those with VIF > 5. This includes:
# Duration.of.ICU.Stay..days. - this will be removed because it is highly correlaed with HOSPITAL_LOS
# Plt.72hr.Total - this will be removed after creating a transfusion variable which takes into consideration whether patient had any type of transufsion
# RBC.72hr.Total - this will be removed after creating a transfusion variable which takes into consideration whether patient had any type of transufsion




###############################
#  EXPLORATORY DATA ANALYSIS  #                   
###############################

# Question 1 (First part): What are the characteristics of patients receiving transfusions?
# These characteristics can be isolated using EDA. 

# Performing EDA
# Creating data frame for continuous and categorical variables
Merged_Frame <- merge(Pre_df, Post_df)
View(Merged_Frame)

Continuous_Variables <- Merged_Frame [c("Age", "BMI", "Pre_Hb", "Pre_Platelets", "Pre_INR", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Duration.of.ICU.Stay..days.", "PostDay1_Hb", "PostDay1_Hct", "PostDay1_Platelets", "PostDay1_INR", "Total.24hr.RBC")]
Categorical_Variables <- Merged_Frame [c("Type", "Gender..male.", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "Transplant_Type", "ExVIVO.Lung.Perfusion", "ECLS_ECMO", "ECLS_CPB", "Massive.Transfusion", "Minimum_Alive_Days")]

# Using funmodeling for Continuous Variables 
basic_eda <- function(Continuous_Variables)
{
  glimpse(Continuous_Variables) # Gives information on the data such as number of rows, columns, values in the data frame, and type of data
  print(status(Continuous_Variables)) # Generates a table with information on the data such as number of zeros and NAs
  freq(Continuous_Variables)
  print(profiling_num(Continuous_Variables)) # Generates a table with information on mean, std_dev, variance, skewness of the distribution, kurotsis, IQR, range_98, and range_80  
  plot_num(Continuous_Variables) # Generates plots for each variable and its data
  describe(Continuous_Variables) # Generates an extensive summary that includes count, mean, standard deviation, minimum, maximum, and various percentiles for each numeric variable
}

basic_eda(Continuous_Variables)

# Using funmodeling for Categorical variables 
basic_eda <- function(Categorical_Variables)
{
  glimpse(Categorical_Variables) # Gives information on the data such as number of rows, columns, values in the data frame, and type of data
  print(status(Categorical_Variables)) # Generates a table with information on the data such as number of zeros and NAs
  freq(Categorical_Variables)
  print(profiling_num(Categorical_Variables)) # Generates a table with information on mean, std_dev, variance, skewness of the distribution, kurotsis, IQR, range_98, and range_80  
  plot_num(Categorical_Variables) # Generates plots for each variable and its data
  describe(Categorical_Variables) # Generates an extensive summary that includes count, mean, standard deviation, minimum, maximum, and various percentiles for each numeric variable
}

basic_eda(Categorical_Variables)

# Creating histograms for continuous variables
for (var in names(Continuous_Variables)) {
  p <- ggplot(Continuous_Variables, aes_string(x = var)) + 
    geom_histogram(bins = 30, fill = "pink", color = "black") +
    theme_minimal() +
    ggtitle(paste("Histogram of", var))
  print(p)
}

# Do a bar plot for Intra_Albumin (Look to group)

# Creating bar plots for categorical variables
for (var in names(Categorical_Variables)) {
  p <- ggplot(Categorical_Variables, aes_string(x = var)) + 
    geom_bar(fill = "purple", color = "black") +
    theme_minimal() +
    ggtitle(paste("Bar Plot of", var))
  print(p)
}

# Answering: What are the characteristics of patients that require transfusions?
# Histogram showing amount of transfusion at 24hrs per patient 
ggplot(Merged_Frame, aes(x = Total.24hr.RBC)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  geom_vline(xintercept = 10, color = "red", linetype = "dashed") +
  ggtitle("Histogram of Total RBC Units Transfused")
# Dashed line shows threshold of Massive Transfusion

# Showing blood transfusion per gender
ggplot(Merged_Frame, aes(x = Gender..male., y = Total.24hr.RBC)) +
  geom_boxplot() +
  ggtitle("Box Plot of Total RBC Units by Gender")

# Showing blood transfusion per age 
ggplot(Merged_Frame, aes(x = Age, y = Total.24hr.RBC)) +
  geom_point() +
  ggtitle("Age vs. Total RBC Units Transfused")

# Doing a correlation heat map to demonstrate correlation between variables 
library(corrplot)
continuous_data <- Merged_Frame[, sapply(Merged_Frame, is.numeric)]
corr_matrix <- cor(continuous_data)
corrplot(corr_matrix, method = "color")

# Heat map for categorical variables 
library(reshape2)
categorical_data <- Merged_Frame[, sapply(Merged_Frame, is.factor)]
heatmap_data <- as.matrix(cor(sapply(categorical_data, as.numeric)))
heatmap(heatmap_data)

# Box Plot of Total RBC Units by Transplant Type 
ggplot(Merged_Frame, aes(x = Transplant_Type, y = Total.24hr.RBC)) +
  geom_boxplot() +
  ggtitle("Total RBC Units by Transplant Type")

#  Plot of BMI vs. Total RBC Units
ggplot(Merged_Frame, aes(x = BMI, y = Total.24hr.RBC)) +
  geom_point(trim = FALSE, fill = "lightblue") +
  ggtitle("Scatter Plot of BMI vs. Total RBC Units Transfused")

# Bar Plot of average of RBC units by transfusion type
ggplot(Merged_Frame, aes(x = Transplant_Type, y = Total.24hr.RBC)) +
  geom_bar(stat = "summary", fun = "mean", fill = "purple") +
  ggtitle("Average Blood Loss by Transplant Type")

# Boxplot for Pre_Hb levels by Massive Transfusion 
ggplot(Merged_Frame, aes(x = factor(Massive.Transfusion), y = Pre_Hb)) +
  geom_boxplot() +
  ggtitle("Pre_Hb Levels by Massive Transfusion Requirement")

# Analyzing the relationship between preoperative hemoglobin (Pre_Hb) levels and the total 24-hour red blood cell (RBC) transfusion amount (Total.24hr.RBC)
ggplot(Merged_Frame, aes(x = Pre_Hb, y = Total.24hr.RBC)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + # Adds a linear regression line
  ggtitle("Preoperative Hemoglobin vs. Total 24hr RBC Transfused") +
  xlab("Preoperative Hemoglobin (g/dL)") +
  ylab("Total 24hr RBC Transfused (units)")

# Scatter Plot for Pre_INR vs. Total 24-hour RBC
ggplot(Merged_Frame, aes(x = Pre_INR, y = Total.24hr.RBC)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Preoperative INR vs. Total 24hr RBC Transfused") +
  xlab("Preoperative INR") +
  ylab("Total 24hr RBC Transfused (units)")

# Bar Plot for ECLS_ECMO vs. Total 24-hour RBC
ggplot(Merged_Frame, aes(x = ECLS_ECMO, y = Total.24hr.RBC)) +
  geom_bar(stat = "summary", fun = "mean", fill = "cyan") +
  ggtitle("Average Total 24hr RBC Transfused by ECLS_ECMO Status") +
  xlab("ECLS_ECMO Status") +
  ylab("Average Total 24hr RBC Transfused (units)")

# Bar Plot for Liver Disease vs. Total 24-hour RBC
ggplot(Merged_Frame, aes(x = Liver.Disease, y = Total.24hr.RBC)) +
  geom_bar(stat = "summary", fun = "mean", fill = "green") +
  ggtitle("Average Total 24hr RBC Transfused by Liver Disease Status") +
  xlab("Liver Disease Status") +
  ylab("Average Total 24hr RBC Transfused (units)")


# Please see report for analysis of these plots.
# Results indicate that some predictors, including Pre_Hb and Liver.Disease, may re-emerge as
# important predictors following more rigorous statistical analysis. 



############################################
#  VARIABLE SELECTION: BOOTSTRAPPED LASSO  #                   
############################################

# Question 1 (Part 2): What are the factors influencing the need and amount of transfusions?

# To find the factors, bootstrapped lasso can be used.
# This involves repeatedly training a Lasso regression model or classifier on samples from the dataset, with replacement.
# Please see report for more information and justification. 
# The most common predictors that are found in the Lasso models can be fit into linear or logistic regression models,
# to better understand the impact each predictor has. 

# Models created and bootstrapped:
#  - Lasso regression with "Total.24hr.RBC" as outcome (to investigate AMOUNT of transfusion)
#  - Lasso classifier with "Massive.Transfusion" as outcome (to investigate need for MASSIVE transfusion)
#  - Lasso classifier with "Had.Transfusion" as outcome (to investigate OVERALL NEED for transfusion)

# Setting the total number of bootstraps (repeats) that will be done for all models.  
num_bootstraps <- 2000


### LASSO REGRESSION WITH 24 HR RBC TRANSFUSION AS OUTCOME ###

# Creating a dataset without Massive.Transfusion and Had.Transfusion for this model
Pre_df_Lasso1 <- Pre_df %>%
  select(-Had.Transfusion, -Massive.Transfusion)

# Initializing a list to contain all the variables in the 'optimal' regression models. 
selected_variables <- list()

# Creating a model matrix with the feature values, for the response variable denoting RBCs received at 24 hours. 
# The first column is excluded since it corresponds to the intercept.
x <- model.matrix(Total.24hr.RBC ~. , Pre_df_Lasso1)[,-1]
# Creating a vector with response values
y <- Pre_df_Lasso1$Total.24hr.RBC

# Main loop to bootstrap Lasso regression and store optimal predictors for each iteration.
set.seed(123)
for(i in 1:num_bootstraps) {
  # Bootstrap sample
  boot_indices <- sample(1:nrow(Pre_df_Lasso1), replace = TRUE)
  boot_x <- x[boot_indices,]
  boot_y <- y[boot_indices]
  
  # Lasso regression. cv.glmnet scales input by default.
  cv.lasso <- cv.glmnet(boot_x, boot_y, alpha = 1, family="gaussian")
  optimal_lambda <- cv.lasso$lambda.min
  
  # Extracting coefficients at the optimal lambda
  lasso_coefs <- coef(cv.lasso, s = optimal_lambda)
  non_zero_coefs <- lasso_coefs[lasso_coefs[, 1] != 0, , drop = FALSE]
  selected_vars_names <- row.names(non_zero_coefs)[-1] # Excluding intercept
  
  # Storing names of selected variables
  selected_variables[[i]] <- selected_vars_names
}

# Analyzing the frequency of selection for each variable
all_selected_vars <- unlist(selected_variables)
variable_selection_freq <- table(all_selected_vars) / num_bootstraps

# Convert the table to a dataframe for easier handling, and sorting by frequency
variable_selection_df <- as.data.frame(variable_selection_freq)
names(variable_selection_df) <- c("Variable", "Frequency")
variable_selection_df <- variable_selection_df[order(-variable_selection_df$Frequency),]

# Finding variables with frequency greater than 80%
selected_predictors <- subset(variable_selection_df, Frequency > 0.80)
print(selected_predictors)

# The following predictors for Total.24hr.RBC had a frequency of being in the final model > 80% (in order of frequency):
#  - Pre_Hb
#  - Transplant_Type
#  - ECLS_ECMO
#  - Intra_Albumin.5...mL.
#  - Gender..male.
#  - Intra_Crystalloid..mL
#  - ECLS_CPB
#  - Pre_Platelets
#  - ExVIVO.Lung.Perfusion

# Creating a frequency plot to visualize the frequency at which each predictor was chosen.
lasso1plot <- ggplot(variable_selection_df, aes(x = reorder(Variable, Frequency), y = Frequency)) + 
                geom_bar(stat = "identity") +
                geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 0.75) +
                coord_flip() + 
                theme_minimal() + 
                labs(x = "Predictors", y = "Frequency of Inclusion in Best Model", title = "Frequency of Predictors: Total.24hr.RBC Model")


# The problem with the above approach is that it does not result in a meaningful single set
# of model weights that can be used to assess the relative importance of the "Best" predictors,
# since only each predictor's frequency of inclusion in the best model is known. 

# To circumvent this problem, the predictors with frequencies > 80% can be fit into a regression
# model with Total.24hr.RBC as outcome, and the beta coefficients can be assessed. Note that the
# resulting p-values cannot be interpreted normally as candidate predictors were chosen based
# on the data-driven bootstrapping process, which can introduce bias; only the beta coefficients
# are particularly informative. 

# Fitting a linear regression model with the most frequent predictors: 
linreg1 <- lm(Total.24hr.RBC ~ Pre_Hb + Transplant_Type + ECLS_ECMO + 
             Intra_Albumin.5...mL. + Gender..male. + Intra_Crystalloid..mL. +
             ECLS_CPB + Pre_Platelets + ExVIVO.Lung.Perfusion, 
             data = Pre_df)
summary(linreg1)
# Please see report for analysis of results. 




### LASSO CLASSIFIER WITH MASSIVE TRANSFUSION AS OUTCOME ###

# Creating a dataset without Had.Transfusion and Total.24hr.RBC for this model
Pre_df_Lasso2 <- Pre_df %>%
  select(-Had.Transfusion, -Total.24hr.RBC)

# Initializing a list to contain all the variables in the 'optimal' classification models. 
class_selected_variables <- list()

# Creating a model matrix with the feature values, for the response variable Massive Transfusion
x2 <- model.matrix(Massive.Transfusion ~. , Pre_df_Lasso2)[,-1]
# Creating a vector with response values
y2 <- Pre_df_Lasso2$Massive.Transfusion

# Main loop to bootstrap Lasso classifier and store optimal predictors for each iteration.
set.seed(123)
for(i in 1:num_bootstraps) {
  # Separate the data into two groups based on the value of 'Massive.Transfusion'.
  # This helps in stratified sampling to address class imbalance.
  indices_0 <- which(y2 == 0)
  indices_1 <- which(y2 == 1)
  
  # Sample separately from each group to ensure representation of both classes in each sample.
  # For the majority class (0), we sample the usual number minus the count of minority class.
  sample_0 <- sample(indices_0, size = nrow(Pre_df_Lasso2) - 9, replace = TRUE)
  # For the minority class (Massive Transfusions), we always sample 9 instances to ensure their presence.
  # (as there are 9 instances in the original dataset)
  sample_1 <- sample(indices_1, size = 9, replace = TRUE)
  
  # Combine the samples and creating an overall bootstrap sample
  boot_indices <- c(sample_0, sample_1)
  boot_x2 <- x2[boot_indices,]
  boot_y2 <- y2[boot_indices]
  
  # Lasso Classification: 
  # Fitting a Lasso model on the bootstrap sample. cv.glmnet automatically scales the input.
  cv.lasso2 <- cv.glmnet(boot_x2, boot_y2, alpha = 1, family="binomial")
  optimal_lambda2 <- cv.lasso2$lambda.min
  
  # Extracting coefficients at the optimal lambda
  # Coefficients not equal to zero indicate selected variables
  lasso_coefs2 <- coef(cv.lasso2, s = optimal_lambda2)
  non_zero_coefs2 <- lasso_coefs2[lasso_coefs2[, 1] != 0, , drop = FALSE]
  selected_vars_names2 <- row.names(non_zero_coefs2)[-1] # Excluding intercept
  
  # Storing names of selected variables
  class_selected_variables[[i]] <- selected_vars_names2
}

# Analyzing the frequency of selection for each variable
class_selected_vars_final <- unlist(class_selected_variables)
class_selection_freq <- table(class_selected_vars_final) / num_bootstraps

# Convert the table to a dataframe for easier handling, and sorting by frequency
class_selection_df <- as.data.frame(class_selection_freq)
names(class_selection_df) <- c("Variable", "Frequency")
class_selection_df <- class_selection_df[order(-class_selection_df$Frequency),]

# Finding variables with frequency greater than 80%
class_final_predictors <- subset(class_selection_df, Frequency > 0.80)
print(class_final_predictors)

# The following predictors for Massive.Transfusion had a frequency of being in the final model > 80% (in order of frequency):
#  - Pre_Hb
#  - Transplant_Type
#  - Pre_Platelets

# Creating a frequency plot to visualize the frequency at which each predictor was chosen.
lasso2plot <- ggplot(class_selection_df, aes(x = reorder(Variable, Frequency), y = Frequency)) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 0.75) +
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Predictors", y = "Frequency of Inclusion in Best Model", title = "Frequency of Predictors: Massive.Transfusion Model")


# As above, fitting a logistic regression model with the most frequent predictors: 
logreg2 <- glm(Massive.Transfusion ~ Pre_Hb + Transplant_Type + Pre_Platelets, 
               family = binomial, 
               data = Pre_df)
summary(logreg2)
# Please see report for analysis of results. 




### LASSO CLASSIFIER WITH HAD.TRANSFUSION AS OUTCOME ###

# Creating a dataset without Massive.Transfusion and Total.24hr.RBC for this model
Pre_df_Lasso3 <- Pre_df %>%
  select(-Massive.Transfusion, -Total.24hr.RBC) %>%
  mutate(Had.Transfusion = as.factor(Had.Transfusion))

# Initializing a list to contain all the variables in the 'optimal' classification models. 
trans_selected_variables <- list()

# Creating a model matrix with the feature values, for the response variable Had.Transfusion
x3 <- model.matrix(Had.Transfusion ~. , Pre_df_Lasso3)[,-1]
# Creating a vector with response values
y3 <- Pre_df_Lasso3$Had.Transfusion

# Main loop 
set.seed(123)
for(i in 1:num_bootstraps) {
  boot_indices3 <- sample(1:nrow(Pre_df_Lasso3), replace = TRUE)
  boot_x3 <- x3[boot_indices3,]
  boot_y3 <- y3[boot_indices3]
  
  # Fitting a Lasso model on the bootstrap sample
  cv.lasso3 <- cv.glmnet(boot_x3, boot_y3, alpha = 1, family="binomial")
  optimal_lambda3 <- cv.lasso3$lambda.min
  
  # Extracting coefficients at the optimal lambda
  # Coefficients not equal to zero indicate selected variables
  lasso_coefs3 <- coef(cv.lasso3, s = optimal_lambda3)
  non_zero_coefs3 <- lasso_coefs3[lasso_coefs3[, 1] != 0, , drop = FALSE]
  selected_vars_names3 <- row.names(non_zero_coefs3)[-1] # Excluding intercept
  
  # Storing names of selected variables
  trans_selected_variables[[i]] <- selected_vars_names3
}

# Analyzing the frequency of selection for each variable
trans_selected_vars_final <- unlist(trans_selected_variables)
trans_selection_freq <- table(trans_selected_vars_final) / num_bootstraps

# Convert the table to a dataframe for easier handling, and sorting by frequency
trans_selection_df <- as.data.frame(trans_selection_freq)
names(trans_selection_df) <- c("Variable", "Frequency")
trans_selection_df <- trans_selection_df[order(-trans_selection_df$Frequency),]

# Finding variables with frequency greater than 80%
trans_final_predictors <- subset(trans_selection_df, Frequency > 0.80)
print(trans_final_predictors)

# The following predictors for Had.Transfusion had a frequency of being in the final model > 80% (in order of frequency):
#  - Pre_Hb
#  - ECLS_ECMO
#  - Pulm_Other
#  - Intra_Albumin.5...mL.
#  - Intra_Crystalloid..mL
#  - Type (Single Left Lung)
#  - Pre_Platelets
#  - BMI
#  - Gender..male
#  - Renal.Failure
#  - Hypertension
#  - ExVIVO.Lung.Perfusion
#  - Coronary.Artery.Disease
#  - ECLS_CPB
#  - Type (Single Right Lung)
#  - Liver.Disease
#  - Transplant_Type

# Creating a frequency plot to visualize the frequency at which each predictor was chosen.
lasso3plot <- ggplot(trans_selection_df, aes(x = reorder(Variable, Frequency), y = Frequency)) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 0.75) +
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Predictors", y = "Frequency of Inclusion in Best Model", title = "Frequency of Predictors: Had.Transfusion Model")


# As above, fitting a logistic regression model with the most frequent predictors: 
logreg3 <- glm(Had.Transfusion ~ Pre_Hb + ECLS_ECMO + Pulm_Other + Intra_Albumin.5...mL. + 
                 Intra_Crystalloid..mL. + Type + Pre_Platelets + BMI + Gender..male. + 
                 Renal.Failure + Hypertension + ExVIVO.Lung.Perfusion + Coronary.Artery.Disease + 
                 ECLS_CPB + Liver.Disease + Transplant_Type, family = binomial, data = Pre_df)
summary(logreg3)
# Please see report for analysis of results. 



############################################
#            SURVIVAL ANALYSIS             #                   
############################################

# Question 2: What is the impact of transfusion on patient outcomes, including mortality?
                 

### INVESTIGATING COLLINEARITY AMONG OUR VARIABLES ###

###### Lets see which of the continuous patient outcomes are highly correlated first so we can either to improve our models moving forward 
# Select only the continuous (numeric) columns from Post_df because corr plot only works for visualizing continuous variables
# Either way those measurements we are interested in for q2 are all numerical until now
numeric_cols <- Post_df %>%
  select_if(is.numeric)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_cols)
cor_matrix

# Visualize the correlation matrix
corrplot::corrplot(cor_matrix, method = "color")


#####Strong Positive Relationships (Correlation close to 1):
# Blood transfusions like plt and rbc are highly correlated too becuase one might imply we need the other but 
# for the sake of our research its important to have both so we will keep them
# Hospital_LOS and Duration of ICU have strong correlation of 0.8

#####Strong Negative Relationships (Correlation close to -1):
# PostDay1_PT and PostDay1_PTT have a strong negative correlation of approximately -0.43.
# PostDay1_Creatinine and PostDay1_PTT have a moderate negative correlation of approximately -0.25



### PREPARING POST_DF FOR Q2 ###


# We will base our primary analysis by using RBC.Total.24hr because the 72 total span variables is based off of columns that have more than 80 missingess.
# However, to take into consideration the other types of transfusion, we will create a variable called Had.Transfusion which sees how any type of transfusin, 
# whether its blood, platelet, or plasma has any impact on patient outcomes 


# Remove strongly correlated patient outcomes  or else they will confuse our regression models 
#  Different transfusion type binary categories will be made based on literature evidence and individual components will be removed 
# For example, FFP and Cryo are both plasma components and fall under plasma transfusion 
Post_df_1 <- Post_df %>%
  mutate(
    Plasma_Transfusion = as.factor(ifelse(FFP.72hr.Total > 0 | Cryo.72hr.Total > 0 , TRUE, FALSE)),
    Blood_Transfusion = as.factor(ifelse(RBC.72hr.Total > 0,  TRUE, FALSE)),
    Platelet_Transfusion = as.factor(ifelse(Plt.72hr.Total > 0, TRUE, FALSE))) %>% 
  mutate(
    Had.Transfusion = as.factor(Plasma_Transfusion == TRUE | Blood_Transfusion == TRUE | Platelet_Transfusion == TRUE))%>% 
  select(-c(Duration.of.ICU.Stay..days., FFP.72hr.Total, Cryo.72hr.Total, RBC.72hr.Total, Plt.72hr.Total))


######### Now do the correlation matrix again; keep in mind it won't show the new transufsion categories because theyre factors
numeric_cols1 <- Post_df_1 %>%
  select_if(is.numeric)

# Calculate the correlation matrix
cor_matrix1 <- cor(numeric_cols1)
cor_matrix1

# Visualize the correlation matrix
corrplot::corrplot(cor_matrix1, method = "color")

# Now everything seems to be fine and no extreme correlation observed (no > 0.8)


###### Let's visualize the transfusion types and calculate the proportion of patients hwo had it: 



# Reshape data for plotting
long_format <- Post_df_1 %>%
  select(Plasma_Transfusion, Blood_Transfusion, Platelet_Transfusion, Had.Transfusion) %>%
  pivot_longer(cols = everything(), names_to = "Transfusion_Type", values_to = "Status")

# Plotting
ggplot(long_format, aes(x = Transfusion_Type, fill = Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Transfusion Status Distribution",
       x = "Type of Transfusion",
       y = "Count",
       fill = "Status") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Calculating frequencies and percentages
frequency_table <- long_format %>%
  group_by(Transfusion_Type, Status) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round((Frequency / sum(Frequency)) * 100, 1)) %>%
  arrange(Transfusion_Type, Status)

# Displaying the frequency table
print(frequency_table)



### ASSESSING MORTALITY ###

##### To assess mortality: use the death date to make a response variable is binary, such as (1 for death and 0 for survival) and use the had transfusion variable to assess relationship of transfusion on likelihood of death
# Replace empty values with NA in DEATH_DATE for it be processed easily
data$DEATH_DATE[data$DEATH_DATE == ""] <- NA

# Convert DEATH_DATE to Date format
data$DEATH_DATE <- as.Date(data$DEATH_DATE, format = "%d-%b-%Y")


# Add these variables to Post_df_1
Post_df_1$DEATH_DATE <- data$DEATH_DATE 

# Create a binary death variable as a factor
Post_df_1$Death_Binary <- factor(ifelse(!is.na(Post_df_1$DEATH_DATE), "1", "0"))

# View the levels of the Death_Binary variable
levels(Post_df_1$Death_Binary)



### ASSESSING IMPACT OF TRANFUSIONS ON PATIENT OUTCOMES ###

##### Had.Transfusion in General
##### We can assess if transfusion binary variabe impacts all post measurement patient outcomes , hospital LOS, death, etc..

glimpse(Post_df_1)

# List of actual outcomes that are continuous and categorical 
outcomes_cont <- c("HOSPITAL_LOS")

outcomes_categ <- c("ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "Death_Binary", "Need.for.reoperation.for.bleeding.within.24h")

# Initialize a list to store regression models and summaries
regression_models <- list()

# Loop through each continuous outcome for linear regression
for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~  Had.Transfusion"), data = Post_df_1)
  regression_models[[paste(outcome, "_linear")]] <- summary(linear_model)
}

# Loop through each binary outcome for logistic regression
for (outcome in outcomes_categ) {
  logistic_model <- glm(paste(outcome, "~ Had.Transfusion"), 
                        data = Post_df_1, family = binomial)
  regression_models[[paste(outcome, "_logistic")]] <- summary(logistic_model)
}

# View the summaries for all regression models
for (model_summary in regression_models) {
  print(model_summary)
}



####### For Total.24hr.RBC
# Initialize a list to store regression models and summaries
regression_models <- list()

# Loop through each continuous outcome for linear regression
for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~ Total.24hr.RBC"), data = Post_df_1)
  regression_models[[paste(outcome, "_linear")]] <- summary(linear_model)
}
glimpse(Post_df_1)
# Loop through each binary outcome for logistic regression
for (outcome in outcomes_categ) {
  logistic_model <- glm(paste(outcome, "~ Total.24hr.RBC"), 
                        data = Post_df_1, family = binomial)
  regression_models[[paste(outcome, "_logistic")]] <- summary(logistic_model)
}

# View the summaries for all regression models
for (model_summary in regression_models) {
  print(model_summary)
}

### ENsuring assumptions are right
for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~ Total.24hr.RBC"), data = Post_df_1)
  plot(linear_model$fitted.values, residuals(linear_model), main=paste("Residuals vs Fitted for", outcome), xlab="Fitted Values", ylab="Residuals")
  abline(h=0, col="red")
}

for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~ Total.24hr.RBC"), data = Post_df_1)
  qqnorm(residuals(linear_model))
  qqline(residuals(linear_model), col = "red")
  title()
}


### For categorical outcomes
for (outcome in outcomes_categ) {
  logistic_model <- glm(paste(outcome, "~ Total.24hr.RBC"), 
                        data = Post_df_1, family = binomial)
  print(broom::glance(logistic_model))
}

# Results 
# For ALIVE_30DAYS_YN: The model has a relatively high null deviance, suggesting that the predictor (Total.24hr.RBC) may not explain a significant portion of the variance in the outcome.

# For ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, and Death_Binary: Similar interpretation as ALIVE_30DAYS_YN, with the null and residual deviances indicating the model's explanatory power.

# For Need.for.reoperation.for.bleeding.within.24h: This model also shows similar characteristics, with null and residual deviances indicating the extent of variance explained.


#########For Massive.Transfusion 
# Initialize a list to store regression models and summaries
regression_models <- list()

# Loop through each continuous outcome for linear regression
for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~ Massive.Transfusion"), data = Post_df_1)
  regression_models[[paste(outcome, "_linear")]] <- summary(linear_model)
}
glimpse(Post_df_1)
# Loop through each binary outcome for logistic regression
for (outcome in outcomes_categ) {
  logistic_model <- glm(paste(outcome, "~ Massive.Transfusion"), 
                        data = Post_df_1, family = binomial)
  regression_models[[paste(outcome, "_logistic")]] <- summary(logistic_model)
}

# View the summaries for all regression models
for (model_summary in regression_models) {
  print(model_summary)
}



# Checking assumptions: 
### FOr categorical outcomes
for (outcome in outcomes_categ) {
  logistic_model <- glm(paste(outcome, "~ Massive.Transfusion"), 
                        data = Post_df_1, family = binomial)
  print(broom::glance(logistic_model))
}


### Ensuring assumptions are right
for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~ Massive.Transfusion"), data = Post_df_1)
  plot(linear_model$fitted.values, residuals(linear_model), main=paste("Residuals vs Fitted for", outcome), xlab="Fitted Values", ylab="Residuals")
  abline(h=0, col="red")
}

for (outcome in outcomes_cont) {
  linear_model <- lm(paste(outcome, "~ Massive.Transfusion"), data = Post_df_1)
  qqnorm(residuals(linear_model))
  qqline(residuals(linear_model), col = "red")
  title()
}




### ASSESSING IMPACT OF TRANSFUSION ON MORTALITY USING SURVIVAL ANALYSIS ###

## For survival analysis and having time to event, we will utilize the ICU admission and discharge dates 
## Let's add icu admission, and icu discharge variables which will be helpful in our downstream analysis for assessing mortality outcome 

# Convert ICU.Admission.Date.Time to Date format
data$ICU.Admission.Date.Time <- as.Date(data$ICU.Admission.Date.Time)

# Convert ICU.Discharge.Date.Time to Date format
data$ICU.Discharge.Date.Time <- as.Date(data$ICU.Discharge.Date.Time)

# Merge ICU admission and discharge times into Post_df_1
Post_df_1$ICU.Admission.Date.Time <- data$ICU.Admission.Date.Time
Post_df_1$ICU.Discharge.Date.Time <- data$ICU.Discharge.Date.Time

# Calculate Survival Time
Post_df_1$Survival_Time <- ifelse(!is.na(Post_df_1$DEATH_DATE),
                                  as.numeric(difftime(Post_df_1$DEATH_DATE, Post_df_1$ICU.Admission.Date.Time, units = "days")),
                                  as.numeric(difftime(Post_df_1$ICU.Discharge.Date.Time, Post_df_1$ICU.Admission.Date.Time, units = "days")))

# Create Status Variable
Post_df_1$Status <- ifelse(!is.na(Post_df_1$DEATH_DATE), 1, 0)

#################################### Had transfusion is optional ####################
# Let's fit a survival model with all predcitors first. Iteration adjusted because model is complex and the transfusion variables can be very corelated
summary(coxph(Surv(Survival_Time, Status) ~ Had.Transfusion + Type + Gender..male. + Age + BMI + COPD + Cystic.Fibrosis + Interstitial.Lung.Disease + Pulm_Other + Coronary.Artery.Disease + 
                Hypertension + Renal.Failure + Stroke.CVA + Liver.Disease, 
              data = Post_df_1))

# check hazard assumption and see if they converge visually using log log survival plots
plot(survfit(coxph(Surv(Survival_Time, Status) ~ Had.Transfusion +Type + Gender..male. + Age + BMI + COPD + Cystic.Fibrosis + Interstitial.Lung.Disease + Pulm_Other + Coronary.Artery.Disease + 
                     Hypertension + Renal.Failure + Stroke.CVA + Liver.Disease, 
                   data = Post_df_1)), fun='cloglog') # they look proportional and do not cross, good sign
# does not look like the best might converge on the top 
cox.zph(coxph(Surv(Survival_Time, Status) ~ Had.Transfusion +Type + Gender..male. + Age + BMI + COPD + Cystic.Fibrosis + Interstitial.Lung.Disease + Pulm_Other + Coronary.Artery.Disease + 
                Hypertension + Renal.Failure + Stroke.CVA + Liver.Disease, 
              data = Post_df_1))

# Loglik converged before variable  2 ; coefficient may be infinite : most likely because of how related plaelet and blood transufsion are 



### Because massive transufison did not show any signifncant relationship based on the regression, it won't be included in the report for survival analysis 
# Fit a survival model using Massive transfusion  
cox_model2 <- coxph(Surv(Survival_Time, Status)~  Massive.Transfusion+Type + Gender..male. +  Age + BMI + COPD + Cystic.Fibrosis + Interstitial.Lung.Disease + Pulm_Other + Coronary.Artery.Disease + 
                      Hypertension + Renal.Failure + Stroke.CVA + Liver.Disease, 
                    data = Post_df_1)
summary(cox_model2)
# Check proportional hazards assumption
cox.zph(cox_model2)
plot(survfit(cox_model2), fun='cloglog') 


# Fit a survival model using Total.24hr.RBC including other potential covariates
cox_model3 <- coxph(Surv(Survival_Time, Status)~ Total.24hr.RBC + Type +Gender..male. + Age + BMI + COPD + Cystic.Fibrosis + Coronary.Artery.Disease + 
                      Hypertension + Renal.Failure + Stroke.CVA + Liver.Disease,
                    data = Post_df_1)
summary(cox_model3)
# Check proportional hazards assumption
cox.zph(cox_model3)
plot(survfit(cox_model3), fun='cloglog')


# Grouping Total.24hr.RBC into categories into regular transfusin, massive (10-20 units), and ultra-massive 20+ units 
Post_df_1$RBC_Transfusion <- cut(Post_df_1$Total.24hr.RBC, 
                                 breaks = c(0, 10, 20, Inf), 
                                 labels = c("0-10 units (Regular)", "10-20 units (Massive)", "20+ units (Ultra-massive)"), 
                                 include.lowest = TRUE)

# Create Kaplan-Meier survival curves
fit <- survfit(Surv(Survival_Time, Status) ~ RBC_Transfusion, data = Post_df_1)
ggsurvplot(fit, data = Post_df_1, pval = TRUE, 
           palette = c("#00BA38", "#F8766D", "#619CFF"),
           xlab = "Time (days)", ylab = "Survival probability")


# Please see report for insights into these results. 

### ADDITIONAL INVESTIGATION: NOT INCLUDED IN REPORT ###

# Load library for plotting
library(survminer)
# Create Kaplan-Meier survival curves
ggsurvplot(survfit(Surv(Survival_Time, Status) ~ Had.Transfusion, data = Post_df_1), data = Post_df_1)

# Create Kaplan-Meier survival curves
ggsurvplot(survfit(Surv(Survival_Time, Status) ~ Massive.Transfusion, data = Post_df_1), data = Post_df_1)

glimpse(Post_df_1)

###### Analyze the survival at 30 days, 90 days, and 12 months in relation to whether patients had a transfusion OPTIONALLLL######### 
library(scales)

# Reshape the data and adjust labels for Had.Transfusion plot
long_Had.transfusion <- Post_df_1 %>%
  select(ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, Had.Transfusion) %>%
  pivot_longer(cols = starts_with("ALIVE"), names_to = "Time_Point", values_to = "Status") %>%
  mutate(Time_Point = case_when(
    Time_Point == "ALIVE_30DAYS_YN" ~ "30 Days",
    Time_Point == "ALIVE_90DAYS_YN" ~ "90 Days",
    Time_Point == "ALIVE_12MTHS_YN" ~ "12 Months",
    TRUE ~ Time_Point
  ),
  Status = ifelse(Status == "Y", "Alive", "Dead"),
  Time_Point = factor(Time_Point, levels = c("30 Days", "90 Days", "12 Months"))) # Order the levels

# Create a summary plot for Had.Transfusion
ggplot(long_Had.transfusion, aes(x = Time_Point, fill = Status)) +
  geom_bar(position = "dodge") +
  geom_text(aes(label = paste0(round((..count..)/sum(..count..) * 100, 1), "%")), stat = "count", position = position_dodge(width = 0.9), vjust = -0.25) +
  facet_wrap(~ Had.Transfusion) +
  labs(title = "Survival Status at Different Time Points For Any Type of Transfusion",
       x = "Time Point",
       y = "Count",
       fill = "Survival Status") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Interpret: 
# for example: "4% Dead at 12mths" for patients who did not have a massive transfusion, it means that 4% of patients in the non-transfusion group were recorded as dead 12mths post-surgery.
# NOT REALLY SURE HOW TO INERPRET


# Repeat the same steps for Massive.Transfusion
long_MT <- Post_df_1 %>%
  select(ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, Massive.Transfusion) %>%
  pivot_longer(cols = starts_with("ALIVE"), names_to = "Time_Point", values_to = "Status") %>%
  mutate(Time_Point = case_when(
    Time_Point == "ALIVE_30DAYS_YN" ~ "30 Days",
    Time_Point == "ALIVE_90DAYS_YN" ~ "90 Days",
    Time_Point == "ALIVE_12MTHS_YN" ~ "12 Months",
    TRUE ~ Time_Point
  ),
  Status = ifelse(Status == "Y", "Alive", "Dead"),
  Time_Point = factor(Time_Point, levels = c("30 Days", "90 Days", "12 Months"))) # Order the levels

# Create a summary plot for Massive.Transfusion
ggplot(long_MT, aes(x = Time_Point, fill = Status)) +
  geom_bar(position = "dodge") +
  geom_text(aes(label = paste0(round((..count..)/sum(..count..) * 100, 1), "%")), stat = "count", position = position_dodge(width = 0.9), vjust = -0.2) +
  facet_wrap(~ Massive.Transfusion) +
  labs(title = "Survival Status at Different Time Points (Massive Transfusion)",
       x = "Time Point",
       y = "Count",
       fill = "Survival Status") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
