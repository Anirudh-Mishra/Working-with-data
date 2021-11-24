library('rvest')

#Specifying the url for desired website to be scraped
for(k in 1947:1948){
  for(j in 1:4){
    if(j == 1)
      y <- 1
    else if(j == 2)
      y <- 4
    else if(j == 3)
      y <- 7
    else
      y <- 10
    
    if(j != 4)
      url <- paste("https://fred.stlouisfed.org/release/tables?rid=53&eid=12998&od=", toString(k), "-0", toString(y), "-01#", sep="")
    
    else
      url <- paste("https://fred.stlouisfed.org/release/tables?rid=53&eid=12998&od=", toString(k), "-", toString(y), "-01#", sep="")
    
    
    print(url)
    #Reading the HTML code from the website
    webpage <- read_html(url)


    #Obtaining quarter data element
    quarter_data_html <- html_nodes(webpage,'.fred-rls-elm-vl-th')

    #Converting the quarter head data to text
    quarter_data <- html_text(quarter_data_html)

    #Stripping spaces and newline character from headings
    for (i in 1:length(quarter_data)){
      quarter_data[i] = gsub("\n                ", "", quarter_data[i])
      quarter_data[i] = gsub("\n              ", "", quarter_data[i])
    }
    quarter_data


    #Obtaining value header data element
    value_head_data_html <- html_nodes(webpage,'.fred-rls-elm-nm')

    #Converting the value header data to text
    value_head_data <- html_text(value_head_data_html)
    value_head_data

    #Obtaining value header data element
    value_data_html <- html_nodes(webpage,'.fred-rls-elm-vl-td')
    
    #Converting the quarter head data to text
    value_data_temp <- html_text(value_data_html)

    #Stripping spaces and newline character from value data and pre-processing
    value_data_temp <- gsub("[^0-9.-]", "", value_data_temp)
    value_data_temp <- as.numeric(value_data_temp)
    value_data_temp
    value_data1 <- vector(mode = "list", length = 0)
    value_data2 <- vector(mode = "list", length = 0)
    value_data3 <- vector(mode = "list", length = 0)
    for (i in 1:length(value_data_temp)){
      if(i %% 3 == 1){
        value_data1 <- c(value_data1, value_data_temp[i])
      }

      if(i %% 3 == 2){
        value_data2 <- c(value_data2, value_data_temp[i])
      }

      if(i %% 3 == 0){
        value_data3 <- c(value_data3, value_data_temp[i])
      }
    }
    value_data <- list(value_data1, value_data2, value_data3)

    value_data[[1]][[1]]


    #Creation of DataFrame
    GDP_df <- data.frame(matrix(ncol = 27, nrow = 0))
    x <- c("Quarter Year",
           "Gross domestic product",
           "Personal consumption expenditures",
           "Goods",
           "Durable goods",
           "Nondurable goods",
           "Services",
           "Gross private domestic investment",
           "Fixed investment",
           "Nonresidential",
           "Structures",
           "Equipment",
           "Intellectual property products",
           "Residential", "Change in private inventories",
           "Net exports of goods and services",
           "Exports",
           "Exported Goods",
           "Exported Services",
           "Imports",
           "Imported Goods",
           "Imported Services",
           "Government consumption expenditures and gross investment",
           "Federal",
           "National defense",
           "Nondefense",
           "State and local")

    colnames(GDP_df) <- x
    GDP_df


    #Appending to DataFrame
    # Populate the row
    new.row <- data.frame("Quarter Year"=quarter_data[1],
                          "Gross domestic product"=value_data[[1]][[1]],
                          "Personal consumption expenditures"=value_data[[1]][[2]],
                          "Goods"=value_data[[1]][[3]],
                          "Durable goods"=value_data[[1]][[4]],
                          "Nondurable goods"=value_data[[1]][[5]],
                          "Services "=value_data[[1]][[6]],
                          "Gross private domestic investmentt"=value_data[[1]][[7]],
                          "Fixed investment"=value_data[[1]][[8]],
                          "Nonresidential"=value_data[[1]][[9]],
                          "Structures"=value_data[[1]][[10]],
                          "Equipment"=value_data[[1]][[11]],
                          "Intellectual property products"=value_data[[1]][[12]],
                          "Residential"=value_data[[1]][[13]],
                          "Change in private inventories"=value_data[[1]][[14]],
                          "Net exports of goods and services"=value_data[[1]][[15]],
                          "Exports"=value_data[[1]][[16]],
                          "Exported Goods"=value_data[[1]][[17]],
                          "Exported Services"=value_data[[1]][[18]],
                          "Imports"=value_data[[1]][[19]],
                          "Imported Goods"=value_data[[1]][[20]],
                          "Imported Services"=value_data[[1]][[21]],
                          "Government consumption expenditures and gross investment"=value_data[[1]][[22]],
                          "Federal"=value_data[[1]][[23]],
                          "National defense"=value_data[[1]][[24]],
                          "Nondefense"=value_data[[1]][[25]],
                          "State and local"=value_data[[1]][[26]])

    # Add the row
    GDP_df <- rbind(GDP_df, new.row)


    GDP_df

    #Appending DataFrame values to CSV file
    write.table(GDP_df,"D:\\Study Items\\PDS\\Theory DA\\GDP_Data.csv", sep = ",", row.names = FALSE, col.names = !file.exists("GDP_Data.csv"), append=T)

  }
}
