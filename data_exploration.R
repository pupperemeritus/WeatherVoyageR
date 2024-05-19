library(dplyr)
library(tidymodels)
library(tidyverse)
library(ggplot2)
weather_data <- read.csv("./data/cleaned_data.csv")
head(weather_data)
glimpse(weather_data)
summary(weather_data)
str(weather_data)



# Get domain for each attribute
for (col in names(weather_data)) {
  cat("Attribute:", col, "\n")
  if (is.factor(weather_data[[col]])) {
    cat("Domain:", levels(weather_data[[col]]), "\n")
  } else {
    cat("Domain:", unique(weather_data[[col]]), "\n")
  }
  cat("\n")
}

# Get range for each attribute
for (col in names(weather_data)) {
  cat("Attribute:", col, "\n")
  if (is.character(weather_data[[col]])) {
    cat("Domain:", unique(weather_data[[col]]), "\n")
  } else {
    cat("Range:", range(weather_data[[col]]), "\n")
  }
  cat("\n")
}

# Find number of missing values for each attribute
missing_values <- sapply(weather_data, function(x) sum(is.na(x)))

# Print the results
print(missing_values)
