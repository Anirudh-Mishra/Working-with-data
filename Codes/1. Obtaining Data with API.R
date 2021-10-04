library('jsonlite')
library('httr')

# Setting a variable for date
date <- "2021-08-27"

# Forming the url by pasting date variable into API call
url = paste0("https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/",date,"?adjusted=true&apiKey=Ucyjj1XsbcJEJ2X4KcYnFNoikjI0BFE1")
url

# Extracting JSON format data from URL
data <- fromJSON(url)
data

# Converting JSON format data to a DataFrame
dfs = as.data.frame(data)

# Dropping unnecessary columns
drops <- c("queryCount", "resultsCount", "adjusted", "status", "request_id", "count")
dfs <- dfs[ , !(names(dfs) %in% drops)]

# Setting Column Headers
colnames(dfs) <- c("Exchange_Symbol", 
                   "Trading_Volume",
                   "Volume_Weighted_Average_Price",
                   "Open_Price",
                   "Close_Price", 
                   "Highest_Price", 
                   "Lowest_Price", 
                   "Unix_Msec_Timestamp",
                   "No_of_Transactions") 

# Adding Additional Date Column in the DataFrame
dfs["Date"] = date
dfs <- dfs[, c(10,1,2,3,4,5,6,7,8,9)]

# Write Newly Acquired Data into CSV File
write.table(dfs, file="D:\\Study Items\\PDS\\Theory DA\\dataset_19BDS0078.csv", sep=",", col.names = !file.exists("D:\\Study Items\\PDS\\Theory DA\\dataset_19BDS0078.csv"), row.names = FALSE, append = TRUE)