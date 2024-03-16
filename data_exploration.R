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
