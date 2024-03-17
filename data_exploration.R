# Setup -------------------------------------------------------------------

# Install required libraries / packages

#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("randomForest")
library(randomForest)

# Data loading ------------------------------------------------------------

# read in the data
data <- read.csv("at-technical-assessment-_2023.csv")

# number of rows in the data set
nrow(data) # 9753 cars

# column names
names(data) 

# what makes are there in the data
unique(data$make) # BMW only

# how many models are included in the data
length(unique(data$model)) # 58 models


# Missingness -------------------------------------------------------------

# Apply function to each column
na_counts <- lapply(data, function(col) sum(is.na(col)))

# Convert the result to a dataframe
na_counts_df <- data.frame(column = names(data), na_count = unlist(na_counts), row.names = NULL)

# Filter out rows with 0 NA count
na_counts_df <- na_counts_df[na_counts_df$na_count != 0, ]

# Bar plots for categorical variables -------------------------------------

# Create a function to generate bar plots for categorical variables
plot_categorical <- function(data, var) {
  ggplot(data, aes(y = !!as.name(var))) +
    geom_bar(fill = "blue") +
    labs(title = paste("Count of", var), x = var, y = "Count") #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

# Get the list of categorical variables
cat_vars <- c("body_type", "fuel_type", "transmission", "colour", "year", "engine_size")

# Create bar plots for each categorical variable
plots <- lapply(cat_vars, function(var) plot_categorical(data, var))

# Arrange the plots in a grid
grid.arrange(grobs = plots, ncol = 2)

# model and area plots
plot_categorical(data, "model")
plot_categorical(data, "area")

# using substring to get more broader categories
plot_categorical(data %>% mutate(model_larger_cat = as.factor(substr(model, 0, 1))), "model_larger_cat")
plot_categorical(data %>% mutate(area_larger_cat = as.factor(substr(area, 0, 1))), "area_larger_cat")


# Hist and boxplots for continous variables -------------------------------

# Select continuous variables
continuous_vars <- c("mileage", "price", "price_position")

# Create histograms for all continuous variables
hist_plots <- lapply(continuous_vars, function(var) {
  
  ggplot(data, aes(x = !!as.name(var))) +
    geom_histogram(bins = ceiling(sqrt(length(data[[var]]))), 
                   fill = "red", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  
})

# Create boxplots for all continuous variables
boxplot_plots <- lapply(continuous_vars, function(var) {
  ggplot(data, aes(y = !!as.name(var))) +
    geom_boxplot(fill = "red", color = "black") +
    labs(title = paste("Boxplot of", var), x = "", y = var) +
    theme_minimal()
})

# Arrange plots side by side
combined_plots <- mapply(grid.arrange, hist_plots, boxplot_plots, SIMPLIFY = FALSE)

# Plot histograms and boxplots side by side
grid.arrange(grobs = combined_plots, ncol = length(continuous_vars))


# Scatter plots -----------------------------------------------------------

# variables to seek relationships

# year vs price scatter plot
year_vs_price_scatter <- ggplot(data, aes(x = year, y = price)) +
        geom_point(colour = "blue") +
        labs(title = paste("Scatter plot of year vs price"),
             x = "year", y = "price") +
        theme_minimal()

# year vs mileage scatter plot
year_vs_mileage_scatter <- ggplot(data, aes(x = year, y = mileage)) +
  geom_point(colour = "blue") +
  labs(title = paste("Scatter plot of year vs mileage"),
       x = "year", y = "mileage") +
  theme_minimal()

# mileage vs price scatter plot
mileage_vs_price_scatter <- ggplot(data, aes(x = mileage, y = price)) +
  geom_point(colour = "blue") +
  labs(title = paste("Scatter plot of mileage vs price"),
       x = "mileage", y = "price") +
  theme_minimal()

# Plot scatter plots using grid.arrange
grid.arrange(grobs = list(year_vs_price_scatter, year_vs_mileage_scatter, mileage_vs_price_scatter), nrow = 3)  # Change the number of columns as needed

# Correlation coefficient tests -------------------------------------------

# Omit all NAs from the dataset
data_removed_nas <- na.omit(data)

# Compute Pearson's correlation coefficient
pearson_cor <- cor(data_removed_nas[c("year", "mileage", "price")], method = "pearson")

# Test significance of Pearson's correlation
# Sample size
n <- nrow(data_removed_nas)

# Degrees of freedom for Pearson's correlation
df <- n - 2

# Calculate t-statistic for Pearson's correlation
t_stat_pearson <- pearson_cor * sqrt(df) / sqrt(1 - pearson_cor^2)

# Calculate p-value for Pearson's correlation
p_value_pearson <- 2 * pt(abs(t_stat_pearson), df = df, lower.tail = FALSE)

# Print the correlation matrix
cat("Pearson's correlation coefficient:\n")
print(pearson_cor)

# Compute Spearman's correlation coefficient
spearman_cor_year_price <- cor.test(data_removed_nas$year, data_removed_nas$price, method = "spearman")
spearman_cor_year_mileage <- cor.test(data_removed_nas$year, data_removed_nas$mileage, method = "spearman")
spearman_cor_mileage_price <- cor.test(data_removed_nas$mileage, data_removed_nas$price, method = "spearman")

# check if p-value is less than significance level (0.05)
cat("\nSpearman's correlation coefficient:\n")
print(spearman_cor_year_price)
print(spearman_cor_year_mileage)
print(spearman_cor_mileage_price)

# Compute kendall's correlation coefficient
kendall_cor_year_price <- cor.test(data_removed_nas$year, data_removed_nas$price, method = "kendall")
kendall_cor_year_mileage <- cor.test(data_removed_nas$year, data_removed_nas$mileage, method = "kendall")
kendall_cor_mileage_price <- cor.test(data_removed_nas$mileage, data_removed_nas$price, method = "kendall")

# check if p-value is less than significance level (0.05)
cat("\nkendall's correlation coefficient:\n")
print(kendall_cor_year_price)
print(kendall_cor_year_mileage)
print(kendall_cor_mileage_price)

# Chi squared tests -------------------------------------------------------

# List of categorical variables
categorical_vars <- c("fuel_type", "body_type", "transmission", "model", "colour", "area", "engine_size")

# Create an empty matrix to store the p-values
p_values <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars), dimnames = list(categorical_vars, categorical_vars))

# Loop through pairs of variables
for (i in 1:length(categorical_vars)) {
  for (j in 1:length(categorical_vars)) {
    # Create a two way table of counts
    two_way_table <- table(data_removed_nas[[categorical_vars[i]]], data_removed_nas[[categorical_vars[j]]])
    # Perform chi-squared test
    chi_sq_test <- chisq.test(two_way_table)
    # Store the p-value in the matrix
    p_values[i, j] <- chi_sq_test$p.value
    
    # print(two_way_table)
  }
}

# Print the p-values matrix
print(p_values)

# Feature columns stacked bar plot ----------------------------------------

# List of binary feature columns
binary_features <- c("feature_1", "feature_2", "feature_3", "feature_4", "feature_5", "feature_6", "feature_7", "feature_8", "feature_9", "feature_10")

# Reshape data for plotting
binary_data <- melt(data_removed_nas[, binary_features])

# Plot stacked bar plot
ggplot(binary_data, aes(x = variable, fill = factor(value))) +
  geom_bar(position = "stack") +
  labs(title = "Occurrences of 1s and 0s in Binary Feature Columns",
       x = "Binary Feature Columns",
       y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Correlation heatmap feature cols ----------------------------------------

# Calculate the phi coefficient matrix
phi_matrix <- rcorr(as.matrix(data_removed_nas[, binary_features]), type = "pearson")$r

# Create a heatmap
ggplot(data = melt(phi_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1),
                       name = "Phi Coefficient") +
  labs(title = "Correlation Heatmap of Binary Feature Columns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# ANOVA price position and feature cols -----------------------------------

# Perform ANOVA for each binary feature column
anova_results <- lapply(binary_features, function(feature) {
  aov_result <- aov(price_position ~ get(feature), data = data_removed_nas)
  return(summary(aov_result))
})

# View the ANOVA results
print(anova_results)


# Random forest feature importance ----------------------------------------

# as the target variable representing the position of price

# Fit a random forest model
model <- randomForest(price_position ~ ., 
                      data = data_removed_nas[, c("price_position", binary_features)], 
                      importance = TRUE)

# Visualize feature importance
varImpPlot(model)

# print tabulated values for feature importance
print(importance(model))
