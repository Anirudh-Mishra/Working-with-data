library(base)
library(utils)
library(stats)
data <- read.csv("https://raw.githubusercontent.com/anthoniraj/datasets/main/data_cleaning/tweet.csv")
head(data)
dim(data)
names(data)
str(data)
summary(data)

# Dropping X,X.1,X.2 due to all values being NA
drops <- c("X","X.1","X.2"); 
data <- data[, !(names(data) %in% drops)];
print("")
head(data)

# Convert retweetCount to numeric format and replace NA values with 0 indicating no tweet
data$retweetCount <- as.numeric(data$retweetCount)
data$retweetCount[is.na(data$retweetCount)] <- 0
summary(data)

# Filter sub set with retweet count greater than 1000
data <- data[data$retweetCount > 1000, ]
dim(data)

# Creating date and time objects from timestamp objects
data$Created <- strptime(data$created, format="%d-%m-%Y %H:%M")
#data

# Retrieve rows containing @usernames in the text columns
data <- data[grepl("@",data$text),]
#data$text

# Extracting username from the text column
format <- function(s){
    s <- strsplit(s, " ")
    lapply(s, function(s)s[grepl("@[[:alnum:]]", s)])
}
data$username <- c(format(data$text))

#Extracting text from HTML script from statusSource column
data$statusSource <- gsub("<[^>]+>","",data$statusSource)

# Displaying dataframe
head(data)
View(data)