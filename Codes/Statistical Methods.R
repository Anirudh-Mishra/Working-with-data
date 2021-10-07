library(base)
library(utils)
library(stats)
library(modeest)
library(graphics)

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