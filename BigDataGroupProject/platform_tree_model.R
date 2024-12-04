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

vgsales <- vgsales %>%
  mutate(Region = case_when(
    NA_Sales >= EU_Sales & NA_Sales >= JP_Sales ~ "NA",
    EU_Sales > NA_Sales & EU_Sales >= JP_Sales ~ "EU",
    JP_Sales > NA_Sales & JP_Sales > EU_Sales ~ "JP",
    TRUE ~ "Other"
  ))

vgsales$Region <- as.factor(vgsales$Region)

numeric_cols <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
vgsales <- remove_outliers(vgsales_uncleaned, numeric_cols)

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

tree_building <- function(cp, depth) {
  model <- rpart(as.factor(Platform) ~ Year + Publisher + Region, 
                 data = vgsales, 
                 method = "class", 
                 control = rpart.control(cp = cp, maxdepth = depth)
                 )
  
  optimal_cp <- model$cptable[which.min(model$cptable[,"xerror"]), "CP"]
  tree <- prune(model, cp = optimal_cp)
  
  return(tree)
}

save(vgsales, tree_building, file="platform_tree_model.RData")


