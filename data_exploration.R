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
