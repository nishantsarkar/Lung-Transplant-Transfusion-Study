# Data Science in Health II
# Team Project 

# Loading the necessary packages
library(readxl)
library(ggplot2)
library(dplyr)
library(funModeling)
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

summary(data) # can see the number of the missing observations for each variable 


# However, NAs are sometimes present in a different format, like empty space, dot, 99, etc.. Let's investigate.
# Apply table function to each column
results_na <- lapply(data[,1:117], table) # not ideal because we have many columns; hectic to go through each

# We can visualize our missing data using nania package
#install.packages("naniar")
library(naniar)
missing_data <- vis_miss(data) # 24.7% missingness in all the dataset

# if large enough dataset with multiple variables, consider multiple imputation if the missingness is random (MCAR OR MAR). 
# however, if certain variables have a very high percentage of missing data, consider excluding them from your analysis or use domain knowledge to impute them more accurately.


# check missingness for each predictor
missing_percentages <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
missing_percentages


# Duration of Mechanical Ventilation with 34.38% missing --> time-dependent variable so missinginess  might be informative in itself and needs careful consideration

# Let's eliminate columns with 70% or more missinginess
# Identify these columns with less than 70% missingness
columns_to_keep <- names(which(missing_percentages < 70))

# Selecting these columns from the original dataset and this will remove the columns 112 to 115 that have no values
filter70_data <- data %>% 
  select(all_of(columns_to_keep))
  
# now we have 90 variables (out of 117)

vis_miss(filter70_data) # now we have 6.8% missingness
sapply(filter70_data, function(x) sum(is.na(x)) / length(x) * 100) # double check to see missigness percentage in every variable
# some are on the verge of 70% 
# Lung1_Clot.Time  69.27, Lung1_A10.EXTEM 69.27, and Lung1_Max.Lysis 69.27


# lets try and do MI on the others 
# load the mice package
library(mice)

glimpse(filter70_data)

# before, encode categorical factors as factors before imputing to use approrpiate method 
# variables like Type, Gender (male), COPD, etc., which are character types, should be converted to factors.
# be aware of Date.of.Extubation which is encoded as character whereas all others should be encoded as factor

filter70_data <- filter70_data %>% 
  mutate_if(is.character, as.factor)

# imputation errors are resulting and this could be due to: 
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


# let's analyze each variable with missing values 
###### 1) DCD.vs.DBD which ahs 7.27% missingness
# this variable relates to the donor itself (not patient); different types of organ donors used in lung transplantation
table(filter70_data$DCD.vs.DBD)
# DBD: Donation after brain death involves donation of organs after the patient meets criteria for death by neurological criteria.
# DCD: Donation after cardiac death involves donation of organs after irreversible cessation of circulatory and respiratory function
# NDD: Neurological Determination of Death or brain death basically 
# FALSE: ??? (2 observations has FALSE)
## i scanned through the literature quick to see if donor status affected patient outcomes but few to none really discuss any relationship between the two
## .. Most focus on grafts and kidney organs too
## Evidence1: For kidney transplants, it's observed that mid and long-term outcomes for DCD grafts are equivalent to DBD kidneys. While short-term outcomes
# ..may be poorer for DCD grafts, the impact of delayed graft function on DCD graft survival is less significant, suggesting good recovery potential (Siddiqi. H 2023)
## Evidence2: observational study comparing DCD vs DBD 
# ..results may lack accuracy & validity but say no difference in organ quality just different means in presevring the organ (Elmer A. 2022)
######## I recommend we remove it for now but we can do available case analysis for it at the end to see if it impacts patient survival-related outcomes or we can use logreg to impute

# Let's change the 2 FALSE encoded values to NA so it is easier to perform any sort of analysis on this predictor
filter70_data <- filter70_data %>% 
  mutate(DCD.vs.DBD = ifelse(DCD.vs.DBD == "FALSE", NA, DCD.vs.DBD))
table(filter70_data$DCD.vs.DBD)


filterDCD_data <- filter70_data %>% 
  select(-DCD.vs.DBD)

missing <- as.data.frame(sapply(filterDCD_data, function(x) sum(is.na(x)) / length(x) * 100))
colnames(missing) <- "Percent_Missing"

########## LAS.score 6.25%
# it is a lung allocation score; used with blood type and the distance between the candidate and the donor hospital to determine priority for receiving a lung transplant


# run the mice code to get the default methods to later store it as default 
default_method <- mice(filter70_data, maxit=0)

# Store the default imputation methods selected by mice in the 'methods' variable
methods <- default_method$method  

# Identify which variables are factors with more than two levels:
# 'sapply' applies the given function to each column of 'filter70_data'.
# 'is.factor(x)' checks if a column is a factor (categorical variable).
# 'nlevels(x) > 2' checks if the factor has more than two levels (categories).
# The result is a logical vector where TRUE indicates a multilevel factor.
multilevel_factors <- sapply(filter70_data, function(x) is.factor(x) && nlevels(x) > 2)

# Update the imputation methods for multilevel factors to 'polyreg':
# 'methods[multilevel_factors]' selects the imputation methods for multilevel factors.
# Assigning 'polyreg' to these methods ensures appropriate imputation for these variables.
methods[multilevel_factors] <- "polyreg" 

# Perform multiple imputation
imputed_data <- mice(filter70_data, method=methods, m=5, maxit=5)

# Completing the data with the imputed values
completed_data <- complete(imputed_data, 1) # Choosing the first imputed dataset as an example


