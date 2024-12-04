library(rpart)
library(rpart.plot)
library(dplyr)

#########################################################################
######## Classification Tree for Predicting Genre based on Sales ########
#########################################################################
remove_outliers <- function(df, cols) {
  for (col in cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    df <- df %>%
      filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
  }
  return(df)
}

# Read the data
vgsales_uncleaned <- read.csv("vgsales.csv")

numeric_cols <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
vgsales <- remove_outliers(vgsales_uncleaned, numeric_cols)

set.seed(2)
train = sample(1:nrow(vgsales), 5000)
vgsales.test = vgsales[-train,]
Genre.test=vgsales$Genre[-train]

# Actual model
genre_by_sales_model <- rpart(as.factor(Genre) ~ Global_Sales + Platform + 
                                Publisher, data = vgsales, subset=train, method = "class")
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
matrix <- table(Predicted = predict_pruned, Actual = vgsales$Genre)
print(matrix)

# Misclassification for Pruned Tree - 0.754
miss <- 1 - sum(diag(matrix)) / sum(matrix)
print(miss)

# Actual classification for Pruned Tree - 0.245
actual <- 1 - miss
print(actual)
