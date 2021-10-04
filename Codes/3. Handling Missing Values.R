library(base)
library(utils)
library(stats)
library(imputeMissings)
library(naniar)
data <- read.csv("https://raw.githubusercontent.com/Anirudh-Mishra/Working-with-data/main/dataset_19BDS0078.csv")
head(data)
dim(data)
#summary(data)

# Removing rows and columns having >60% null values
data <- data[which(rowMeans(!is.na(data)) > 0.6), which(colMeans(!is.na(data)) > 0.6)]
#summary(data)

# Imputing data with mean since not much skewness
data$Volume_Weighted_Average_Price[is.na(data$Volume_Weighted_Average_Price)] <- mean(data$Volume_Weighted_Average_Price, na.rm = TRUE)

# Imputing data with median due to skewness in data
data$Trading_Volume[is.na(data$Trading_Volume)] <- median(data$Trading_Volume, na.rm = TRUE)  
summary(data)

dim(data)

data <- data[, c(1,2,3,4,5,7,6)]
summary(data)

# Imputing data with RandomForest algorithm
val <- compute(data[3:6], method="randomForest")
data[3:6] <- impute(data[3:6], object = val)
miss_var_summary(data)

# The dataset used in this experiment consists of stock data which involves interdependent columns, 
# thus requiring an imputation method that accounts for observation of all other column values for 
# filling the missing value in the column itself.

# RandomForest is a multivariate imputation technique meaning it considers multiple variables for 
# filling missing data as described and required in the above problem. It can accomodate non-linearities 
# in data and due to multiple columns being considered for filling a missing data value, the introduction
# of bias seems highly unlikely. That is why I've used this technique.
 