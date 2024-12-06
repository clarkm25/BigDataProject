library(rpart)
library(rpart.plot) # for plotting
library(bslib) 
library(scales) # for comma formatting

# Author: Gavin Raguindin
# This file is responsible for plotting and predicting the sales of a certain region based on platform and genre

# function to remove outliers (IQR)
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  
  return(data)
}

# remove outliers for each target column using the remove_outliers function
vgsales_uncleaned <- read.csv("vgsales.csv")
data <- na.omit(vgsales_uncleaned)
data <- remove_outliers(vgsales_uncleaned, "NA_Sales")
data <- remove_outliers(vgsales_uncleaned, "EU_Sales")
data <- remove_outliers(vgsales_uncleaned, "JP_Sales")
data <- remove_outliers(vgsales_uncleaned, "Other_Sales")