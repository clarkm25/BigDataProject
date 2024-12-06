# Author: Tyler Santos
# Date: 12/5/24
# Fall 2024
#
# This R script is used to predict a region that will yield the highest sales
# given regional sales and a platform.This code cleans the dataset by removing outliers
# and also only using data for the top 10 platforms. 

library(dplyr)
library(rpart)
library(rpart.plot)

game_data <- read.csv("vgsales.csv")

# Create new category, Region, based on the region with the highest sales
game_data <- game_data %>%
  mutate(Region = case_when(
    NA_Sales >= EU_Sales & NA_Sales >= JP_Sales ~ "NA",
    EU_Sales > NA_Sales & EU_Sales >= JP_Sales ~ "EU",
    JP_Sales > NA_Sales & JP_Sales > EU_Sales ~ "JP",
    TRUE ~ "Other"
  ))

game_data$Region <- as.factor(game_data$Region)

# Function to remove outliers using the interquartile range method
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

# List of numeric columns to clean
numeric_cols <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")

game_data_clean <- remove_outliers(game_data, numeric_cols)

# Aggregate by Platform to determine the top 10 platforms by total sales
top_platforms <- game_data_clean %>%
  group_by(Platform) %>%
  summarize(Total_Sales = sum(NA_Sales + EU_Sales + JP_Sales + Other_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 10) %>%
  pull(Platform)

# Filter the cleaned data to include only the top 10 platforms
game_data_clean <- game_data_clean %>%
  filter(Platform %in% top_platforms)

# Function to build and prune a tree based on input parameters
build_tree <- function(cp, max_depth) {
  tree_model <- rpart(
    Region ~ Platform + NA_Sales + EU_Sales + JP_Sales + Other_Sales, 
    data = game_data_clean, 
    method = "class",
    control = rpart.control(cp = cp, maxdepth = max_depth)
  )
  
  optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
  pruned_tree <- prune(tree_model, cp = optimal_cp)
  
  return(pruned_tree)
}

# Save the data and tree-building function 
save(game_data_clean, build_tree, file = "tree_model.RData")

