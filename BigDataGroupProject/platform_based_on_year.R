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

numeric_cols <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
vgsales <- remove_outliers(vgsales_uncleaned, numeric_cols)

vgsales <- vgsales %>%
  mutate(Region = case_when(
    NA_Sales >= EU_Sales & NA_Sales >= JP_Sales ~ "NA",
    EU_Sales > NA_Sales & EU_Sales >= JP_Sales ~ "EU",
    JP_Sales > NA_Sales & JP_Sales > EU_Sales ~ "JP",
    TRUE ~ "Other"
  ))

vgsales$Region <- as.factor(vgsales$Region)

# Summarize total sales by publisher and takes the top 20
top_publishers <- vgsales %>%
  group_by(Publisher) %>%
  summarize(TotalSales = sum(Global_Sales, na.rm = TRUE)) %>% 
  arrange(desc(TotalSales)) %>%
  slice_head(n = 10) %>%
  pull(Publisher)

# Filter dataset for only the top 20 publishers
vgsales <- vgsales %>%
  filter(Publisher %in% top_publishers)


# Summarize total sales by platform and takes the top 5
top_platforms <- vgsales %>%
  group_by(Platform) %>%
  summarize(TotalSales = sum(Global_Sales, na.rm = TRUE)) %>% 
  arrange(desc(TotalSales)) %>%
  slice_head(n = 5) %>%
  pull(Platform)

# Filter dataset for only the top 5 platforms
vgsales <- vgsales %>%
  filter(Platform %in% top_platforms)

platform_by_year_model <- rpart(as.factor(Platform) ~ Year + Publisher + 
                                  Region, data = vgsales, method = "class")
rpart.plot(platform_by_year_model, type = 3, extra = 101, fallen.leaves = TRUE, box.palette = "Blues", 
           main = "Classification Tree for Platform Based on Year and Publisher")

# Predictions 
pred <- predict(platform_by_year_model, vgsales, type = "class")
matrix <- table(Predict = pred, Actual = vgsales$Platform)
matrix

# Misclassification - 0.743
miss <- 1 - sum(diag(matrix)) / sum(matrix)
print(miss)

# Actual classification - 0.256
actual <- 1 - miss
print(actual)

# Pruned Tree
pruned_model <- rpart(as.factor(Platform) ~ Year + Publisher + Genre + 
                        NA_Sales + EU_Sales + JP_Sales + Other_Sales, data = vgsales, method = "class", 
                      control=rpart.control(cp = 0.0001))
printcp(pruned_model)
optimal_cp <- pruned_model$cptable[which.min(pruned_model$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(pruned_model, cp=optimal_cp)
rpart.plot(pruned_tree, type = 3, extra = 101, fallen.leaves = TRUE, box.palette = "Blues", 
           main = "Pruned Classification Tree for Platform based on Year and Publisher")

# Predictions for Pruned Tree
predict_pruned <- predict(pruned_tree, vgsales, type = "class")
matrix <- table(Predicted = predict_pruned, Actual = vgsales$Platform)
print(matrix)

# Misclassification for Pruned Tree - 0.754
miss <- 1 - sum(diag(matrix)) / sum(matrix)
print(miss)

# Actual classification for Pruned Tree - 0.245
actual <- 1 - miss
print(actual)

# Convert non-numeric columns to numeric
vgsales_transformed <- vgsales
vgsales_transformed[] <- lapply(vgsales, function(col) {
  if (is.factor(col) || is.character(col)) {
    as.numeric(as.factor(col))
  } else {
    col
  }
})

# Use pairs() on the transformed dataset
pairs(vgsales_transformed)

