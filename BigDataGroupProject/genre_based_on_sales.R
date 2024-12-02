library(rpart)
library(rpart.plot)

#########################################################################
######## Classification Tree for Predicting Genre based on Sales ########
#########################################################################
removeOutliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25)
  Q3 <- quantile(data[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ])
}

# Read the data
vgsales_uncleaned <- read.csv("vgsales.csv")

vgsales <- removeOutliers(vgsales_uncleaned, 'NA_Sales')
vgsales <- removeOutliers(vgsales_uncleaned, 'EU_Sales')
vgsales <- removeOutliers(vgsales_uncleaned, 'JP_Sales')
vgsales <- removeOutliers(vgsales_uncleaned, 'Other_Sales')
vgsales <- removeOutliers(vgsales_uncleaned, 'Global_Sales')

# First attempt at model - Creates a tree with a single leave..
genre_by_sales_model <- rpart(Genre ~ NA_Sales + EU_Sales + JP_Sales + 
                                Other_Sales, data = vgsales, method = "class")
rpart.plot(genre_by_sales_model, type = 3, extra = 101, fallen.leaves = TRUE, 
           box.palette = "Blues", main = "Classification Tree for Genre based on Sales")

# Actual model
genre_by_sales_model <- rpart(Genre ~ NA_Sales + EU_Sales + JP_Sales + 
                                Other_Sales, data = vgsales, method = "class", 
                              control = rpart.control(cp = 0.001))
rpart.plot(genre_by_sales_model, type = 3, extra = 101, fallen.leaves = TRUE,
           box.palette = "Blues", main = "Classification Tree for Genre based on Sales")

# Predictions 
pred <- predict(genre_by_sales_model, vgsales, type = "class")
matrix <- table(Predict = pred, Actual = vgsales$Genre)
matrix

# Misclassification - 0.743
miss <- 1 - sum(diag(matrix)) / sum(matrix)
print(miss)

# Actual classification - 0.256
actual <- 1 - miss
print(actual)

# Pruned Tree
pruned_model <- rpart(Genre ~ NA_Sales + EU_Sales + JP_Sales + 
                        Other_Sales, data = vgsales, method = "class", 
                      control=rpart.control(cp = 0.0001))
printcp(pruned_model)
optimal_cp <- pruned_model$cptable[which.min(pruned_model$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(pruned_model, cp=optimal_cp)
rpart.plot(pruned_tree, type = 3, extra = 101, fallen.leaves = TRUE, box.palette = "Blues", 
           main = "Pruned Classification Tree for Genre based on Sales")

# Predictions for Pruned Tree
predict_pruned <- predict(pruned_tree, vgsales, type = "class")
matrix <- table(Predicted = predicted_pruned, Actual = vgsales$Genre)
print(matrix)

# Misclassification for Pruned Tree - 0.754
miss <- 1 - sum(diag(matrix)) / sum(matrix)
print(miss)

# Actual classification for Pruned Tree - 0.245
actual <- 1 - miss
print(actual)


genre_by_sales_model <- rpart(Genre ~ NA_Sales + EU_Sales + JP_Sales + 
                                Other_Sales, data = vgsales, method = "class")
rpart.plot(genre_by_sales_model, type = 3, extra = 101, fallen.leaves = TRUE, 
           box.palette = "Blues", main = "Classification Tree for Genre based on Sales")

