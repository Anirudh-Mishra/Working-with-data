library(base)
library(utils)
library(stats)
library(modeest)
library(graphics)
library(tidyquant)

data <- read.csv("https://raw.githubusercontent.com/Anirudh-Mishra/Working-with-data/main/dataset_19BDS0078.csv")
#head(data)
#dim(data)
#summary(data)

df = subset(data, select = -c(Close_Price, Highest_Price, Lowest_Price))
#df

# No_of_Transactions is selected as the column for performing given operations 



# MEAN
mean_func <- function(data){
  sum = 0
  for(x in 1:length(data)) {
    sum <- sum + data[x]
  }
  sum
  data_mean <- sum/length(data)
  data_mean
}

data_mean <- mean_func(df[, 'No_of_Transactions'])

cat('\nMean Comparison\n')
# User calculated mean
data_mean  

# Predefined function mean
mean(df[,'No_of_Transactions']) 



# MEDIAN
median_func <- function(data){
  values <- sort(data, decreasing = FALSE)
  #print(length(values))
  if(length(data) %% 2 == 1)
    data_median <- values[length(data)/2 + 1]
  else{
    #print('yes')
    #print(values[length(data)/2])
    #print(values[length(data)/2 + 1])
    data_median <- (values[length(data)/2] + values[length(data)/2 + 1]) / 2
  }
  data_median
}

data_median <- median_func(df[, 'No_of_Transactions'])

cat('\nMedian Comparison\n')
# User calculated Median
data_median

# Predefined function Median
median(df[, 'No_of_Transactions'], na.rm = FALSE)



# MODE
mode_func <- function(x) {
  freq <- table(x)
  freq_table <- sort(freq, decreasing = T)
  highFreq <- freq_table[[1]]
  calc_mode <- which(freq_table==highFreq)
  calc_mode
}

data_mode <- mode_func(df[, 'No_of_Transactions'])

cat('\nMode Comparison\n')
# User calculated Mode
data_mode

# Predefined function Mode
mfv(df[, 'No_of_Transactions'])



# IQR
IQR_func <- function(data){
  values_iqr <- sort(data, decreasing = FALSE)
  q1 <- median_func(values_iqr[0:(length(values_iqr)%/%2)+1])
  q2 <- median_func(values_iqr)
  q3 <- median_func(values_iqr[(length(values_iqr)%/%2+1):length(values_iqr)])
  data_iqr <- c(q1, q2, q3)
  data_iqr
}

data_iqr_values <- IQR_func(df[, 'No_of_Transactions'])

cat('\nIQR Comparison\n')
# User calculated IQR
data_iqr <- data_iqr_values[3] - data_iqr_values[1]
data_iqr

# Predefined function IQR
iqr_values <- summary(df[, 'No_of_Transactions'])
iqr_values[5] - iqr_values[2]



# STANDARD DEVIATION
SD_func <- function(data) {
  temp = 0
  for(x in 1:length(data)){
    temp = temp + ((data_mean - data[x])**2)
  }
  var <- temp %/% (length(data)-1)
}

data_sd <- SD_func(df[, 'No_of_Transactions'])**0.5

cat('\nStandard Deviation Comparison\n')
# User calculated standard deviation
data_sd

# Predefined function standard deviation
sd(df[, 'No_of_Transactions'])



# PROBABILITY VALUES ON EMPIRICAL RULE
cat('\nProbability values on empirical rule\n') 

# For 68% of the data
cat('68% data lies between\n') 
print(paste(data_mean - data_sd, data_mean+data_sd))

# For 95% of the data
cat('\n95% data lies between\n') 
print(paste(data_mean-(data_sd*2), data_mean+(data_sd*2)))

# For 99.7% of the data
cat('\n99.7% data lies between\n') 
print(paste(data_mean-(data_sd*3),data_mean+(data_sd*3)))



# PLOTTING AND VISUALISING DATA
# Graph
col <- as.vector(df[, 'No_of_Transactions'])
plot(col, main = "Number of Transactions")

# Histogram
hist(col, probability = T)
curve(dnorm(x, mean = data_mean, sd= data_sd), add = T, main = "Number of Transactions")

cat('\n\n')

# To suppress warnings
options(warn=-1)




# HYPOTHESIS TESTING

# The data used in this particular part of the experiment involves stock data of two stocks namely, Exxon Mobil Corporation and Intel Corporation. 
# The details of both these stocks in the 5 month gap of March to August of the current year is inserted into variables   
exxon <- tq_get('XOM', from = "2021-03-01", to = "2021-08-01", get = "stock.prices")
intel <- tq_get('INTC', from = "2021-03-01", to = "2021-08-01", get = "stock.prices")

# Then the subset of the data is considered where we take into account only the open prices of the two stocks for the purpose of comparison of significance.
exxon <- exxon$high
intel <- intel$high

#exxon
#intel

# A T-test is carried out to test the significance of the difference between the open prices of both stocks.

# STEP 1 -> For the purpose of a constant result, the whole sample of 106 rows is considered rather than a random sample.
# STEP 2 -> Null Hypothesis -  There is no difference or significance relationship between the open prices of the two stocks.
# STEP 3 -> Alternate Hypothesis -  The true difference is different from zero meaning a significant difference does exist.
# STEP 4 -> Significance level(aplha value) is considered to be 5%(0.05).
# STEP 5 -> Test Statistic and corresponding p-value are calculated.

result = t.test(sample(exxon, 100), sample(intel, 100), var.equal = T)
cat('\n')
result

# Test statistic value
print(result[1])

# p-Value
print(result[3])

# STEP 6 -> DRAWING A CONCLUSION
# Figuring out whether to accept or reject null hypothesis
if(result[3]<0.05){
  print('Null Hypothesis rejected')
  # In this case the result shows the p-value comes out to be lesser than the significance level chosen (i.e 0.05) as a result of which the null hypothesis is rejected
  # in favor of the alternate hypothesis and hence there exists a significant difference between the open prices of both the stock symbols.
}else{
  print('Null Hypothesis accepted')
  # In this case the result shows the p-value comes out to be greater than the significance level chosen (i.e 0.05) as a result of which the null hypothesis is accepted  
  # and hence there exists no significant difference between the open prices of both the stock symbols.
}
