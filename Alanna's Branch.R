# Data Science in Health II
# Team Project 

# Loading the necessary data sets
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

# Loading the data set 
data <- read_excel("transfusion data.xlsx")
View(data)

# Take a deeper look at the dataset's structure to understand its components using glimpse() & str()
str(data)
glimpse(data) # 192 patiens observations and 117 columns
# 1st column is study id
#column 112 to 115 do not make sense; no variable name just a number and all NA values so will remove them later

#make.names function to ensure all column names are valid R names. This function will replace spaces and special characters with dots (.) and ensure names are unique and syntactically valid in R.
names(data) <- make.names(names(data), unique = TRUE)

any(is.na(data)) # check if there is  any missing values portrayed as NA

# Now let's see how many NAs in our dataset
sum(is.na(data))

summary(data) # can see the number of the missing observations for each variable (5557 NAs)


# However, NAs are sometimes present in a different format, like empty space, dot, 99, etc.. Let's investigate.
# Apply table function to each column
results_na <- lapply(data[,1:117], table) # not ideal because we have many columns; hectic to go through each

# We can visualize our missing data using nania package
missing_data <- vis_miss(data) # 24.7% missingness in all the dataset
missing_data
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


# Check missingness percentage for each predictor
missing_before_filter <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
missing_before_filter

# for time-dependent variables, such as date of icu dishcagre or duration, missinginess  might be informative in itself and needs careful consideration

# Let's eliminate columns with 30% or more missinginess
# Identify these columns with less than 30% missingness
columns_to_keep <- names(which(missing_before_filter < 30))

# Selecting these columns from the original dataset and this will also remove the columns 112 to 115 that have no values
filter70_data <- data %>% 
  select(all_of(columns_to_keep))

# now we have 80 variables (out of 117)

vis_miss(filter70_data) # now we have 0.8% missingness instead of 24.7%
missing_after <- as.data.frame(sapply(filter70_data, function(x) sum(is.na(x)) / length(x) * 100))
colnames(missing_after) <- "Percent_Missing" # double check to see missigness percentage in every variable


# lets try and do MI on the others 
# load the mice package
glimpse(filter70_data)

# before, encode categorical factors as factors before imputing to use approrpiate method 
# variables like Type, Gender (male), COPD, etc., which are character types, should be converted to factors.
# be aware of Date.of.Extubation which is encoded as character (but should be dttm) 

###### All variables encoded as chr but should be factor: 
# Type, Gender,COPD, alpha1-Antitrypsin Deficiency, Cystic Fibrosis, Idiopathic Pulmonary Hypertension, 
# Interstitial Lung Disease, Pulm_Other, Coronary Artery Disease, Hypertension, Diabetes (insulin), Diabetes (diet/OHGs), 
# GERD/PUD, Renal Failure, Stroke/CVA, Liver Disease, Thyroid Disease, First Lung Transplant, Redo Lung Transplant, ExVIVO Lung Perfusion, 
# Preoperative ECLS, Intraoperative ECLS, ECLS_ECMO, ECLS_CPB, Protamine (Y=1 N=0), Tranexamic Acid Used, Need for reoperation for bleeding within 24h, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN


# Intraoperative.ECLS encoded as lgl but should be factor 
# Protamine..Y.1.N.0. ecnoded as dbl but should be factor and it should take values of 0s and 1s where 0 means it was NOT administered vs 1 meaning opposite
# however, 3 values are 25, 150, and 400 so lets let's change these values to NA 
table(filter70_data$Protamine..Y.1.N.0.)
# and encode protamine as factor

# Intra_Albumin.5...mL. is chr but should be dbl (not factor)
table(filter70_data$Intra_Albumin.5...mL.)
# has a weird value of 0+AQ7.. change to 0

# Date.of.Extubation column was processed weirdly when loaded into R (not proceessed as the other date and time as the other date/time variable)
filter70_data$Date.of.Extubation <- as.POSIXct(as.numeric(filter70_data$Date.of.Extubation) * (60*60*24),
                                               origin="1899-12-30", tz="UTC")
glimpse(filter70_data)
#Massive transfusion is dbl but represented as 0s and 1s, so its a binary indicator

#### Ensure dates are in dttm 
# OR Date, ICU Admission Date/Time, ICU Discharge Date/Time, Date of Extubation, ICU Admit Date-Time, ICU Discharge Date-Time, Extubation Date

######### Now, lets incorporate all these changes: 
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

#double check 
glimpse(filter70_data)

# Imputation errors are resulting when running MI and this could be due to: 
# 1) High Collinearity: If data have variables that are highly correlated with each other, it can cause multicollinearity problems in regression models.
# can check using VIF
# library(car)
# create linear model fitted in ourdataset
# vif_values <- vif(lm_model)
# high_collinearity <- names(vif_values[vif_values > 5])  # VIF > 5 is a common threshold

# 2) Sparse Data or Categories with Very Few Observations: For categorical variables, having categories with very few observations can lead to singularities.
# use table function for each variable seperately or use sapply: 

# 3) Very Low Variance in a Variable: If a variable has very low variance (i.e., most of its values are the same), it can lead to issues in the regression model used for imputation
### Can check using this line of code: 
#variances <- sapply(filter70_data, var, na.rm = TRUE)
#low_variance <- names(variances[variances < some_threshold])  # Set some_threshold appropriately



# Let's analyze each variable with missing values so MI runs properly 

###### 1) DCD.vs.DBD which has 7.27% missingness
# this variable relates to the donor itself (not patient); different types of organ donors used in lung transplantation
table(filter70_data$DCD.vs.DBD)
# DBD: Donation after brain death involves donation of organs after the patient meets criteria for death by neurological criteria.
# DCD: Donation after cardiac death involves donation of organs after irreversible cessation of circulatory and respiratory function
# NDD: Neurological Determination of Death or brain death basically 
# FALSE: ??? (2 observations has FALSE)

# Let's change the 2 FALSE encoded values to NA so it is easier to perform any sort of analysis on this predictor if we want to use it later no
filter70_data <- filter70_data %>% 
  mutate(DCD.vs.DBD = ifelse(DCD.vs.DBD == "FALSE", NA, DCD.vs.DBD))

table(filter70_data$DCD.vs.DBD) # we have 119 DBD, 45 DCD, 12 NDD

# remove the DCD predictor if we decide its unnecessary 
filterDCD_data <- filter70_data %>% 
  select(-DCD.vs.DBD)

########## LAS.score 6.25%
# it is a lung allocation score; used with blood type and the distance between the candidate and the donor hospital to determine priority for receiving a lung transplant
# it is important because if we have a higher score, then this means the patient is in urgent need for an organ and probably impacts its need for blood transufions 
# since its scores(numeric), use pmm method for when doing multiple imputation


######### Pre_PTT (partial thromboplastin time) 0.52%
# time it takes for a clot to form in a blood sample
# imp as could influence need of transfusion

####### Blood.Loss 1.042 % missing 
table(filter70_data$Blood.Loss) # looks good in terms of legit values and its a necessary predictor
# same for urine output and fluid balance

####### ICU.Discharge.Date.Time 0.52% & Duration.of.ICU.Stay..days. 0.52%
table(filter70_data$ICU.Discharge.Date.Time)
table(filter70_data$Duration.of.ICU.Stay..days.) # looks fine 
# also missingness in discharge time (which is one observation) is most likely MNAR because patient might have died, so no discharge 

##### these variables are repeated twice in the dataset!

#ICU.Admission.Date.Time 0.0000000
#ICU.Discharge.Date.Time 0.5208333
#Duration.of.ICU.Stay..days. 0.5208333
#Date.of.Extubation 0.0000000
#Duration.of.Ventilation 30.2083333

# ICU.Admit.Date.Time 16.6666667
# ICU.Discharge.Date.Time.1 17.7083333
# Duration.of.ICU.stay..days. 1.0416667
# Extubation.Date 51.0416667
# Duration.of.Mechanical.Ventilation..days. 34.3750000


### lets do a side by side comparison to see where difference is
comparison_data <- filter70_data %>%
  select(
    ICU.Admission.Date.Time,
    ICU.Admit.Date.Time,
    ICU.Discharge.Date.Time,
    ICU.Discharge.Date.Time.1,
    Duration.of.ICU.Stay..days.,
    Duration.of.ICU.stay..days.) 
# let's remove the latter ones as they have more missingess compared to the former ones

filter70_data <- filter70_data %>%
  select(
    -ICU.Admit.Date.Time,
    -ICU.Discharge.Date.Time.1,
    -Duration.of.ICU.stay..days.)

glimpse(filter70_data) # 77 variables for now

#####PostImmediate_PTT 1.042%, PostImmediate_Fibrinogen 65.63%, PostImmediate_Creatinine 1.56%
# these can be used to assess patient need for transfusion 
table(filter70_data$PostImmediate_PTT)
table(filter70_data$PostImmediate_Fibrinogen)
table(filter70_data$Pre_Creatinine)
#### to be aware of: PTT and fibrogen have negative values, which I think could indicitate very very low levels but to having negative levels is not plausible

######### PostDay Measurements (4)
## In PostDay1_PTT, we have one negative value
## can be used in patient need for transfusion

######## RBC.0.24hrs 68.75%
table(filter70_data$RBC.0.24hrs) # looks good in terms of not having abnormal values 
# however, RBC 24-48hrs	RBC 48-72hrs had more than 70% missingness so they were reomved
# we only have 0-24 and 72 hour total as of now. 
# since there's a lot of missingness in 0-24hrs, we can decide to select only the RBC count at the  72hr total timeframe. 
filter70_data <- filter70_data %>%
  select(-RBC.0.24hrs)

##### Let's check for negative values across the dataset
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

glimpse(filter70_data)

colnames(filter70_data)

######ALANNA###########
# Creation of new data frame with selected variables based on literature review
# First data frame contains "Pre" variables to answer first question
View(filter70_data)
Pre_df <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "First.Lung.Transplant", "Redo.Lung.Transplant", "ExVIVO.Lung.Perfusion", "Pre_Hb","Pre_Hct", "Pre_Platelets", "Pre_INR", "ECLS_ECMO", "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Massive.Transfusion" )]
View(Pre_df)
# Second data frame contains "Post" variables to answer the second question
Post_df <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "ExVIVO.Lung.Perfusion", "Duration.of.ICU.Stay..days.","ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "PostDay1_Hb", "PostDay1_Hct", "PostDay1_Platelets", "PostDay1_INR", "Total.24hr.RBC", "ECLS_ECMO", "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Massive.Transfusion")]
View(Post_df)

Pre_df <- Pre_df %>% 
  mutate(Transplant_Type = ifelse(Redo.Lung.Transplant == TRUE, "SECOND", "FIRST")) %>%
  select(-First.Lung.Transplant, -Redo.Lung.Transplant)

# now do the same for Alive in 30, 90, 12 months variable 
Post_df <- Post_df %>%
  mutate(Minimum_Alive_Days = case_when(
    ALIVE_12MTHS_YN == "Y" ~ 365,
    ALIVE_90DAYS_YN == "Y" ~ 90,
    ALIVE_30DAYS_YN == "Y" ~ 30,
    TRUE ~ 0
  )) %>%
  select(-ALIVE_30DAYS_YN, -ALIVE_90DAYS_YN, -ALIVE_12MTHS_YN)

# IMPUTING THE DATA
# Imputation for the rest of columns
vis_miss(Pre_df) # Missing under 0.1%
vis_miss(Post_df) # Missing under 0.1%

# Performing Stochastic Imputation
  # Only doing one imputation rather than doing multiple imputations 
Pre_df <- mice(Pre_df, m = 1, method = 'pmm', seed = 123)
Pre_df <- complete(Pre_df, 1)
vis_miss(Pre_df) # No more NA Values

# Only doing one imputation rather than doing multiple imputations
Post_df <- mice(Post_df, m = 1, method = 'pmm', seed = 123)
Post_df <- complete(Post_df, 1)
vis_miss(Post_df) # No more NA Values

# CHECKING FOR COLLINEARITY
# Fitting a linear model with 'Blood.Loss' as the dependent variable
model <- lm(Blood.Loss ~ ., data=Pre_df)
# Calculating Variance Inflation Factor (VIF)
vif_results <- vif(model)
# Identifying variables with high collinearity
high_vif <- vif_results[vif_results > 5]  # You can also use 10 as a threshold
print(high_vif)
# Pre_Hb and Pre_Hct are highly collinear

Pre_df_1 <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "First.Lung.Transplant", "Redo.Lung.Transplant", "ExVIVO.Lung.Perfusion", "Pre_Hb", "Pre_Platelets", "Pre_INR", "ECLS_ECMO", "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Massive.Transfusion" )]
# Fitting a linear model with 'Blood.Loss' as the dependent variable
model_1.2 <- lm(Blood.Loss ~ ., data=Pre_df_1)
# Calculating Variance Inflation Factor (VIF)
vif_results_1.2 <- vif(model_1.2)
# Displaying VIF results
print(vif_results_1.2)
# When removing Pre_Hct, no more collinearity is present

# Fitting a linear model with 'Blood.Loss' as the dependent variable
model_2 <- lm(Blood.Loss ~ ., data=Post_df)
# Calculating Variance Inflation Factor (VIF)
vif_results_2 <- vif(model_2)
# Displaying VIF results
print(vif_results_2)
# Total.24hr.RBC and Intra_Packed.Cells are highly collinear

Post_df_1 <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "ExVIVO.Lung.Perfusion", "Duration.of.ICU.Stay..days.","ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "PostDay1_Hb", "PostDay1_Hct", "PostDay1_Platelets", "PostDay1_INR", "Total.24hr.RBC", "ECLS_ECMO", "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Blood.Loss")]
# Fitting a linear model with 'Blood.Loss' as the dependent variable
model_2.1 <- lm(Blood.Loss ~ ., data=Post_df_1)
# Calculating Variance Inflation Factor (VIF)
vif_results_2.1 <- vif(model_2.1)
# Displaying VIF results
print(vif_results_2.1)
# No more collinearity when removing Intra.Packed.Cells and Massive.Transfusion

# Question 1
# Performing EDA
# Creating data frame for continuous and categorical variables
Merged_Frame <- merge(Pre_df, Post_df)
View(Merged_Frame)

Continuous_Variables <- Merged_Frame [c("Age", "BMI", "Pre_Hb","Pre_Hct", "Pre_Platelets", "Pre_INR", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Duration.of.ICU.Stay..days.", "PostDay1_Hb", "PostDay1_Hct", "PostDay1_Platelets", "PostDay1_INR", "Total.24hr.RBC")]
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
