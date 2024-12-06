# Author: Max Clark
# Date: December 6th, 2024
#
# This file cleans up our dataset by removing outliers. It then (for the sake
# of being runnable) cleans the dataset more by taking the entries with the 
# top 10 publishers and the top 5 platforms. After cleaning the data, it creates
# a function that makes the classification tree model which predicts the platform
# used given the year the game came out, the region where the game is most 
# popular, and the publisher of the game.

library(rpart)
library(rpart.plot)
library(dplyr)

##############################################################################
#Classification Tree for Predicting Platform based on Year, Publisher, Region#
##############################################################################

# Function to remove outliers based on the quantile they are in.
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

# Remove any outliers based on the sales
numeric_cols <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
vgsales <- remove_outliers(vgsales_uncleaned, numeric_cols)

# Creates a new column for Region
vgsales <- vgsales %>%
  mutate(Region = case_when(
    NA_Sales >= EU_Sales & NA_Sales >= JP_Sales ~ "NA",
    EU_Sales > NA_Sales & EU_Sales >= JP_Sales ~ "EU",
    JP_Sales > NA_Sales & JP_Sales > EU_Sales ~ "JP",
    TRUE ~ "Other"
  ))

# Find top 10 publishers based on sales
top_publishers <- vgsales %>%
  group_by(Publisher) %>%
  summarize(TotalSales = sum(Global_Sales, na.rm = TRUE)) %>% 
  arrange(desc(TotalSales)) %>%
  slice_head(n = 10) %>%
  pull(Publisher)

# Clean data to only include top 10 publishers
vgsales <- vgsales %>%
  filter(Publisher %in% top_publishers)


# Find top 5 platforms based on sales
top_platforms <- vgsales %>%
  group_by(Platform) %>%
  summarize(TotalSales = sum(Global_Sales, na.rm = TRUE)) %>% 
  arrange(desc(TotalSales)) %>%
  slice_head(n = 5) %>%
  pull(Platform)

# Clean data to only include top 5 platforms
vgsales <- vgsales %>%
  filter(Platform %in% top_platforms)

# Builds tree with depth and control being given by user
build_platform_tree <- function(cp, depth) {
  model <- rpart(as.factor(Platform) ~ Year + Publisher + Region, 
                 data = vgsales, 
                 method = "class", 
                 control = rpart.control(cp = cp, maxdepth = depth)
                 )
  
  optimal_cp <- model$cptable[which.min(model$cptable[,"xerror"]), "CP"]
  tree <- prune(model, cp = optimal_cp)
  
  return(tree)
}

save(vgsales, build_platform_tree, file="platform_tree_model.RData")


