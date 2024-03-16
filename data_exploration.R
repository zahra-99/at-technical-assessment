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

# Plots for continuous variables ------------------------------------------

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
year_vs_price_scatter <- ggplot(data, aes(x = year, y = price)) +
        geom_point(colour = "blue") +
        labs(title = paste("Scatter plot of year vs price"),
             x = "year", y = "price") +
        theme_minimal()

year_vs_mileage_scatter <- ggplot(data, aes(x = year, y = mileage)) +
  geom_point(colour = "blue") +
  labs(title = paste("Scatter plot of year vs mileage"),
       x = "year", y = "mileage") +
  theme_minimal()

mileage_vs_price_scatter <- ggplot(data, aes(x = mileage, y = price)) +
  geom_point(colour = "blue") +
  labs(title = paste("Scatter plot of mileage vs price"),
       x = "mileage", y = "price") +
  theme_minimal()

# Plot scatter plots using grid.arrange
grid.arrange(grobs = list(year_vs_price_scatter, year_vs_mileage_scatter, mileage_vs_price_scatter), nrow = 3)  # Change the number of columns as needed

