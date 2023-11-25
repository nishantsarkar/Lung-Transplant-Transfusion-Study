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

# DATA EXPLORATION
# Descriptive statistics for continuous variables
descriptive_stats <- data %>% select_if(is.numeric) %>% summary()
# Print descriptive statistics
print(descriptive_stats)

# Performing EDA
basic_eda <- function(data)
{
  glimpse(data) # Gives information on the data such as number of rows, columns, values in the data frame, and type of data
  print(status(data)) # Generates a table with information on the data such as number of zeros and NAs
  freq(data)
  print(profiling_num(diet.data)) # Generates a table with information on mean, std_dev, variance, skewness of the distribution, kurotsis, IQR, range_98, and range_80  
  plot_num(data) # Generates plots for each variable and its data
  describe(data) # Generates an extensive summary that includes count, mean, standard deviation, minimum, maximum, and various percentiles for each numeric variable
}

basic_eda(data)

# CLEANING THE DATA
# First removing columns with over 70% missing data 
threshold <- 50  # threshold for missing values
data <- data[, which(colMeans(!is.na(data)) > (threshold / 100))]
vis_miss(data)

# Imputation for the rest of columns
# Separate continuous and categorical variables
continuous_vars <- sapply(data, function(x) is.numeric(x) & length(unique(x[!is.na(x)])) > 15)
categorical_vars <- sapply(data, function(x) is.factor(x) | (is.character(x) & length(unique(x[!is.na(x)])) <= 15))

# Impute continuous variables using mean or median
data[, continuous_vars] <- lapply(data[, continuous_vars], function(x) {
  if(sum(is.na(x)) / length(x) < 0.1) {  # Check if missing data is less than 10%
    x[is.na(x)] <- mean(x, na.rm = TRUE)  # Use median(x, na.rm = TRUE) for skewed data
  }
  return(x)
})

# Impute categorical variables using mode
data[, categorical_vars] <- lapply(data[, categorical_vars], function(x) {
  if(sum(is.na(x)) / length(x) < 0.1) {  # Check if missing data is less than 10%
    mode <- names(sort(table(x), decreasing = TRUE))[1]
    x[is.na(x)] <- mode
  }
  return(x)
})

View(data)

# Assuming 'OR Date' is the date of surgery and is related to ICU admit date
data$`ICU Admit Date-Time`[is.na(data$`ICU Admit Date-Time`)] <- 
  data$`OR Date`[is.na(data$`ICU Admit Date-Time`)] # plus or minus some interval as needed
# Similar approach for ICU Discharge Date-Time
# Assuming an average ICU stay duration can be calculated or estimated
avg_icu_stay <- median(data$`ICU Discharge Date-Time` - data$`ICU Admit Date-Time`, na.rm = TRUE)
data$`ICU Discharge Date-Time`[is.na(data$`ICU Discharge Date-Time`)] <- 
  data$`ICU Admit Date-Time`[is.na(data$`ICU Discharge Date-Time`)] + avg_icu_stay

# Omitting Last missing NA value 
data <- na.omit(data)

# COMPARITIVE TESTS
# Comparing patients requiring massive transfusions (>10 RBC units) vs. those who don't.
# Using t-tests or Mann-Whitney U tests for continuous variables, and chi-squared tests for categorical variables.
t.test(data$`Total 24hr RBC` ~ data$`Massive Transfusion`)
chisq.test(table(data$COPD, data$`Massive Transfusion`))

# Correlation analysis 
cor.test(data$Pre_Hb, data$`Total 24hr RBC`, method="pearson")

# Survival Analysis 
# Comparing survival rates between patients with and without massive transfusions. DEATH_DATE indicates mortality, and OR Date represents the start date.
# Assuming 'ALIVE_30DAYS_YN' is a binary variable indicating survival
# 1 for alive, 0 for deceased
data$Survival_Time <- 30  # Survival time in days
surv_object <- Surv(data$Survival_Time, data$ALIVE_30DAYS_YN == 1)
surv_fit <- survfit(surv_object ~ data$`Massive Transfusion`)
plot(surv_fit)
survdiff(surv_object ~ data$`Massive Transfusion`)

# Regression Analysis 
lm_icu_los <- lm(ICU_LOS ~ `Total 24hr RBC` + Age + `Gender (male)`, data=data)
summary(lm_icu_los)

# Visualization using a ggplot
# Creating a boxplot comparing ICU length of stay between massive transfusion groups.
ggplot(data, aes(x=factor(`Massive Transfusion`), y=ICU_LOS)) + geom_boxplot()