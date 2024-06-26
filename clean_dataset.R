# Load required library
library(dplyr)

combined_data <- read.csv("./data/combined_data.csv")
replace_nines_with_na <- function(data, columns) {
  for (col in columns) {
    data[[col]] <- ifelse(grepl("^9+\\.?9*$|^\\.9+9*$|^\\.$", as.character(data[[col]])), NA, data[[col]])
  }
  return(data)
}


columns_to_replace <- c("TEMP", "DEWP", "SLP", "VISIB", "WDSP", "MXSPD", "GUST", "PRCP", "SNDP", "STP")

replace_na_with_zero <- function(data, columns) {
  for (col in columns) {
    data[[col]][is.na(data[[col]])] <- 0
  }
  return(data)
}
# Replace missing values with NA
combined_data_cleaned <- replace_nines_with_na(combined_data, columns_to_replace)

# List of columns where NA values should be replaced with 0
columns_to_replace <- c("SNDP")

combined_data_cleaned <- replace_na_with_zero(combined_data_cleaned, columns_to_replace)

na_percentages <- colMeans(is.na(combined_data_cleaned))

# Identify columns with more than 10% NA values
columns_to_drop <- names(na_percentages[na_percentages > 0.2])

# Drop columns with more than 10% NA values
combined_data_cleaned <- combined_data_cleaned[, !names(combined_data_cleaned) %in% columns_to_drop]

# Define conversion functions
fahrenheit_to_celsius <- function(temp_fahrenheit) {
  return((temp_fahrenheit - 32) * 5 / 9)
}

inches_to_pascal <- function(pressure_inches) {
  return(pressure_inches * 3386.39)
}

miles_to_kilometers <- function(visibility_miles) {
  return(visibility_miles * 1.60934)
}

knots_to_mps <- function(wind_speed_knots) {
  return(wind_speed_knots * 0.514444)
}

inches_to_mm <- function(precip_inches) {
  return(precip_inches * 25.4)
}

inches_to_cm <- function(snow_depth_inches) {
  return(snow_depth_inches * 2.54)
}

# Convert units
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(
    TEMP_SI = ifelse(!is.na(TEMP), fahrenheit_to_celsius(TEMP), NA),
    DEWP_SI = ifelse(!is.na(DEWP), fahrenheit_to_celsius(DEWP), NA),
    VISIB_SI = ifelse(!is.na(VISIB), miles_to_kilometers(VISIB), NA),
    WDSP_SI = ifelse(!is.na(WDSP), knots_to_mps(WDSP), NA),
    MXSPD_SI = ifelse(!is.na(MXSPD), knots_to_mps(MXSPD), NA),
    MAX_SI = ifelse(!is.na(MAX), fahrenheit_to_celsius(MAX), NA),
    MIN_SI = ifelse(!is.na(MIN), fahrenheit_to_celsius(MIN), NA),
    PRCP_SI = ifelse(!is.na(PRCP), inches_to_mm(PRCP), NA),
    SNDP_SI = ifelse(!is.na(SNDP), inches_to_cm(SNDP), NA)
  ) %>%
  select(-c(TEMP, DEWP, SLP, VISIB, WDSP, MXSPD, MAX, MIN, PRCP, SNDP))
combined_data_cleaned$Fog <- (combined_data_cleaned$FRSHTT %/% 100000) %% 10
combined_data_cleaned$Rain_Drizzle <- (combined_data_cleaned$FRSHTT %/% 10000) %% 10
combined_data_cleaned$Snow_Ice_Pellets <- (combined_data_cleaned$FRSHTT %/% 1000) %% 10
combined_data_cleaned$Hail <- (combined_data_cleaned$FRSHTT %/% 100) %% 10
combined_data_cleaned$Thunder <- (combined_data_cleaned$FRSHTT %/% 10) %% 10
combined_data_cleaned$Tornado_Funnel_Cloud <- combined_data_cleaned$FRSHTT %% 10

# Combine binary flags into a single bitmap column
combined_data_cleaned$FRSHTT <- paste(combined_data_cleaned$Fog, combined_data_cleaned$Rain_Drizzle, combined_data_cleaned$Snow_Ice_Pellets,
  combined_data_cleaned$Hail, combined_data_cleaned$Thunder, combined_data_cleaned$Tornado_Funnel_Cloud,
  sep = ""
)

# Define major columns where NA values should be considered for dropping rows
major_columns <- c("TEMP", "DEWP", "SLP", "VISIB", "WDSP", "MXSPD", "PRCP")

# Drop rows with NA values only in major columns
combined_data_cleaned <- combined_data_cleaned[complete.cases(combined_data[, major_columns]), ]

write.csv(combined_data_cleaned, file = "cleaned_data.csv", sep = ",")
