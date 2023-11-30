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
Pre_df <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "Redo.Lung.Transplant", "ExVIVO.Lung.Perfusion", "Pre_Hb","Pre_Hct", "Pre_Platelets", "Pre_INR", "ECLS_ECMO", "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Massive.Transfusion", "Total.24hr.RBC" )]
View(Pre_df)
# Second data frame contains "Post" variables to answer the second question
Post_df <- filter70_data[c("Type", "Gender..male.", "Age", "BMI", "COPD", "Cystic.Fibrosis", "Interstitial.Lung.Disease", "Pulm_Other", "Coronary.Artery.Disease", "Hypertension", "Renal.Failure", "Stroke.CVA", "Liver.Disease", "Redo.Lung.Transplant", "ExVIVO.Lung.Perfusion", "Duration.of.ICU.Stay..days.","ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN", "PostDay1_Hb", "PostDay1_Hct", "PostDay1_Platelets", "PostDay1_INR", "Total.24hr.RBC", "ECLS_ECMO", "ECLS_CPB", "Intra_Albumin.5...mL.", "Intra_Crystalloid..mL.", "Intra_Packed.Cells", "Blood.Loss", "Massive.Transfusion")]
View(Post_df)

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

# Question 1
# Performing EDA
basic_eda <- function(Pre_df)
{
  glimpse(Pre_df) # Gives information on the data such as number of rows, columns, values in the data frame, and type of data
  print(status(Pre_df)) # Generates a table with information on the data such as number of zeros and NAs
  freq(Pre_df)
  print(profiling_num(Pre_df)) # Generates a table with information on mean, std_dev, variance, skewness of the distribution, kurotsis, IQR, range_98, and range_80  
  plot_num(Pre_df) # Generates plots for each variable and its data
  describe(Pre_df) # Generates an extensive summary that includes count, mean, standard deviation, minimum, maximum, and various percentiles for each numeric variable
}

basic_eda(Pre_df)















































































































































































































































































































# BOOTSTRAPPED LASSO TEST - NISHANT
library(glmnet)

# Creating subset of pre-data to be used as predictors in this section.
# Pre_df2 <- Pre_df %>% (....fill this in later....)
# Data frame without Massive Transfusion for regression part
# Data frame without 24hr RBC for Classification part

set.seed(123) # For reproducibility; Please ensure this is ran with the rest of the code


###################################################################
# FINDING PREDICTORS FOR AMOUNT OF RBCs TRANSFUSED DURING SURGERY #
###################################################################

# Setting the total number of bootstraps (repeats) that will be done. 
num_bootstraps <- 1000
# Initializing a list to contain all the variables in the 'optimal' regression models. 
selected_variables <- list()

# Creating a model matrix with the feature values, for the response variable denoting RBCs received at 24 hours. The first column is excluded since it corresponds to the intercept.
x <- model.matrix(Total.24hr.RBC ~. , Pre_df)[,-1]
# Creating a vector with response values
y <- Pre_df$Total.24hr.RBC

# Main loop to bootstrap Lasso regression and store optimal predictors for each iteration.
for(i in 1:num_bootstraps) {
  # Bootstrap sample
  boot_indices <- sample(1:nrow(Pre_df), replace = TRUE)
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

# Finding variables with frequency greater than 75%
selected_predictors <- subset(variable_selection_df, Frequency > 0.75)
print(selected_predictors)


# INSERT FREQUENCY PLOT HERE

# INSERT LINEAR REGRESSION SECTION HERE - WILL DO ONCE WE COMBINE THE CODE



####################################################
# FINDING PREDICTORS FOR MASSIVE BLOOD TRANSFUSION #
####################################################

# Initializing a list to contain all the variables in the 'optimal' classification models. 
class_selected_variables <- list()

# Creating a model matrix with the feature values, for the response variable Massive Transfusion
x2 <- model.matrix(Massive.Transfusion ~. , Pre_df)[,-1]
# Creating a vector with response values
y2 <- Pre_df$Massive.Transfusion

# Main loop to bootstrap Lasso classifier and store optimal predictors for each iteration.
set.seed(123)
for(i in 1:num_bootstraps) {
  # Separate the data into two groups based on the value of 'Massive.Transfusion'.
  # This helps in stratified sampling to address class imbalance.
  indices_0 <- which(y2 == 0)
  indices_1 <- which(y2 == 1)
  
  # Sample separately from each group to ensure representation of both classes in each sample.
  # For the majority class (0), we sample the usual number minus the count of minority class.
  sample_0 <- sample(indices_0, size = nrow(Pre_df) - 9, replace = TRUE)
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

# Finding variables with frequency greater than 75%
class_final_predictors <- subset(class_selection_df, Frequency > 0.75)
print(class_final_predictors)


# INSERT FREQUENCY PLOT HERE

# INSERT LINEAR REGRESSION SECTION HERE - WILL DO ONCE WE COMBINE THE CODE
