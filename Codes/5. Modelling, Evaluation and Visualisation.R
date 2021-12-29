# For this experiment, a dataset is to be created from custom mailbox between the given time duration of 14 days labelled as spam and non-spam. This dataset is then to be used
# to train and test the Multinomial Naive Bayes Model for classification after which the model is successfully able to classify new unseen emails as spam or non-spam based on the
# email contents. The dataset will consist of email header information namely sender_email_id, subject, date and the target variable that
# is the class of the email(spam or non-spam). 


# IMPORTING LIBRARIES
library(base)
library(utils)
library(stats)
library(dplyr)
library(e1071)
library(gmodels)
library(ggplot2)
library(graphics)

# LOADING THE DATASET
data <- read.csv("https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0078.csv")
summary(data)

# DATA PREPROCESSING
data$Message_body <- (gsub("[^0-9A-Za-z///' ]", "", as.matrix(data$Message_body))) 
data$Subject <- (gsub("[^0-9A-Za-z///' ]", "", as.matrix(data$Subject)))
data$Message_body <- (gsub("x000D", " ", as.matrix(data$Message_body))) 
data$Message_body <- (gsub("  ", " ", as.matrix(data$Message_body))) 

processDate1 <- function(string){
  string1 <- substr(string, 6, 25)
  string1
}

processDate2 <- function(string){
  string1 <- substr(string, 0, 20)
  string1
}

data$DateTime[nchar(data$DateTime) < 29] <- lapply(data$DateTime[nchar(data$DateTime) < 29], processDate2)
data$DateTime[nchar(data$DateTime) >= 29] <- lapply(data$DateTime[nchar(data$DateTime) >= 29], processDate1)

data <- select(data, MsgFrom, Subject, Message_body, Spam)
data <- filter(data, Spam %in% c('Yes','No'))

data= data[sample(1:nrow(data)), ]

# Creation of test and train sets
data_train <- data[1:180,]
data_test <- data[180:201,]

# MODEL PREPARATION
# Training the model on training data
data_classifier <- naiveBayes(Spam ~ MsgFrom+Subject+Message_body, data=data_train)

# Predictions carried out by model on test data
data_test_pred <- predict(data_classifier, data_test)

data_test_pred
data_test$Spam


# CUSTOM TESTING
# Calculation of precision, recall, F1 Score and Accuracy
TP <- 0  # True positive count
TN <- 0  # True negative count
FP <- 0  # False positive count
FN <- 0  # False negative count

for (x in 1:length(data_test_pred)) {
  
  if(data_test_pred[x] == "Yes" && data_test$Spam[x] == "Yes")
    TP <- TP+1
  
  else if(data_test_pred[x] == "Yes" && data_test$Spam[x] == "No")
    FP <- FP+1
  
  else if(data_test_pred[x] == "No" && data_test$Spam[x] == "Yes")
    FN <- FN+1
  
  else if(data_test_pred[x] == "No" && data_test$Spam[x] == "No")
    TN <- TN+1
    
}

Precision <- TP/(TP+FP)                                    # Precision Calculation
Recall <- TP/(TP+FN)                                       # Recall Calculation
F1_Score <- (2*Precision*Recall)/(Precision + Recall)      # F1 Score Calculation
Accuracy <- (TP + TN)/(TP + FP + TN + FN)                  # Accuracy Calculation

print(paste0("Precision: ", Precision))
print(paste0("Recall: ", Recall))
print(paste0("F1 Score: ", F1_Score))
print(paste0("Accuracy: ", Accuracy))


# BUILT IN TESTING
# Confusion matrix to assess model performance
CrossTable(data_test_pred, data_test$Spam, prop.chisq= FALSE, prop.t=FALSE, dnn=c('Predicted','Actual'))


# VISUALISATION
# Plotting Data Properties using Pie Chart
labels <- c('Spam', 'Non-Spam')
train_count <- c(nrow(filter(data_train, Spam=='Yes')), nrow(filter(data_train, Spam=='No')))
test_count <- c(nrow(filter(data_test, Spam=='Yes')), nrow(filter(data_test, Spam=='No')))

data_train_viz <- data.frame(group=labels, value=train_count)
ggplot(data_train_viz, aes(x=labels, y=value, fill=group)) + 
        geom_bar(stat="identity", width=1) + 
        ggtitle("Training Data Statistics") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        geom_label(aes(label = value), position = position_stack(vjust = 0.5), show.legend = FALSE)


data_test_viz <- data.frame(group=labels, value=test_count)
ggplot(data_test_viz, aes(x=labels, y=value, fill=group)) + 
        geom_bar(stat="identity", width=1) + 
        ggtitle("Testing Data Statistics") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        geom_label(aes(label = value), position = position_stack(vjust = 0.5), show.legend = FALSE)


# Plotting Confusion Matrix 
Actual <- factor(c("Spam", "Spam", "Non-Spam", "Non-Spam"))
Predicted <- factor(c("Spam", "Non-Spam", "Spam", "Non-Spam"))
Y <- c(TP, FN, FP, TN)
df <- data.frame(Actual, Predicted, Y)
df
ggplot(data=df, mapping = aes(x = Actual, y = Predicted)) + 
        geom_tile(aes(fill = Y), color = "white") +
        geom_text(aes(label = Y), vjust = 1) + 
        scale_fill_gradient(low = "white", high = "red") + 
        ggtitle("Confusion Matrix") + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

