# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the CSV file
weather_data <- read.csv("./data/cleaned_data.csv")

# Convert 'DATE' column to date format
weather_data$DATE <- as.Date(weather_data$DATE)

# Extract day of the year (1-365/366) from the 'DATE' column
weather_data$DAY_OF_YEAR <- yday(weather_data$DATE)

# Identify numeric columns
numeric_cols <- c(
  "TEMP_SI", "DEWP_SI",
  "WDSP_SI", "MXSPD_SI", "MAX_SI", "MIN_SI", "PRCP_SI", "SNDP_SI"
)
weather_data[numeric_cols] <- lapply(weather_data[numeric_cols], as.numeric)

# Calculate mean and standard deviation for each numeric attribute, grouped by day of the year and station name
mean_sd_by_day_and_name <- weather_data %>%
  group_by(DAY_OF_YEAR, NAME) %>%
  summarise(
    TEMP_SI_mean = mean(TEMP_SI, na.rm = TRUE),
    TEMP_SI_sd = sd(TEMP_SI, na.rm = TRUE),
    DEWP_SI_mean = mean(DEWP_SI, na.rm = TRUE),
    DEWP_SI_sd = sd(DEWP_SI, na.rm = TRUE),
    WDSP_SI_mean = mean(WDSP_SI, na.rm = TRUE),
    WDSP_SI_sd = sd(WDSP_SI, na.rm = TRUE),
    MXSPD_SI_mean = mean(MXSPD_SI, na.rm = TRUE),
    MXSPD_SI_sd = sd(MXSPD_SI, na.rm = TRUE),
    MAX_SI_mean = mean(MAX_SI, na.rm = TRUE),
    MAX_SI_sd = sd(MAX_SI, na.rm = TRUE),
    MIN_SI_mean = mean(MIN_SI, na.rm = TRUE),
    MIN_SI_sd = sd(MIN_SI, na.rm = TRUE),
    PRCP_SI_mean = mean(PRCP_SI, na.rm = TRUE),
    PRCP_SI_sd = sd(PRCP_SI, na.rm = TRUE),
    SNDP_SI_mean = mean(SNDP_SI, na.rm = TRUE),
    SNDP_SI_sd = sd(SNDP_SI, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate and filter extreme values (3, 4, 5 standard deviations from the mean) grouped by day of the year and station name
multipliers <- c(3, 4, 5)
extreme_values_by_multiplier <- list()

for (multiplier in multipliers) {
  extreme_values <- weather_data %>%
    filter(
      abs(TEMP_SI - mean_sd_by_day_and_name$TEMP_SI_mean) < 15 * mean_sd_by_day_and_name$TEMP_SI_sd |
        abs(DEWP_SI - mean_sd_by_day_and_name$DEWP_SI_mean) < 15 * mean_sd_by_day_and_name$DEWP_SI_sd |
        abs(WDSP_SI - mean_sd_by_day_and_name$WDSP_SI_mean) < 15 * mean_sd_by_day_and_name$WDSP_SI_sd |
        abs(MXSPD_SI - mean_sd_by_day_and_name$MXSPD_SI_mean) > 15 * mean_sd_by_day_and_name$MXSPD_SI_sd |
        abs(MAX_SI - mean_sd_by_day_and_name$MAX_SI_mean) < 15 * mean_sd_by_day_and_name$MAX_SI_sd |
        abs(MIN_SI - mean_sd_by_day_and_name$MIN_SI_mean) < 15 * mean_sd_by_day_and_name$MIN_SI_sd |
        abs(PRCP_SI - mean_sd_by_day_and_name$PRCP_SI_mean) < 15 * mean_sd_by_day_and_name$PRCP_SI_sd |
        abs(SNDP_SI - mean_sd_by_day_and_name$SNDP_SI_mean) < 15 * mean_sd_by_day_and_name$SNDP_SI_sd
    ) %>%
    filter(
      abs(TEMP_SI - mean_sd_by_day_and_name$TEMP_SI_mean) > multiplier * mean_sd_by_day_and_name$TEMP_SI_sd |
        abs(DEWP_SI - mean_sd_by_day_and_name$DEWP_SI_mean) > multiplier * mean_sd_by_day_and_name$DEWP_SI_sd |
        abs(WDSP_SI - mean_sd_by_day_and_name$WDSP_SI_mean) > multiplier * mean_sd_by_day_and_name$WDSP_SI_sd |
        abs(MXSPD_SI - mean_sd_by_day_and_name$MXSPD_SI_mean) > multiplier * mean_sd_by_day_and_name$MXSPD_SI_sd |
        abs(MAX_SI - mean_sd_by_day_and_name$MAX_SI_mean) > multiplier * mean_sd_by_day_and_name$MAX_SI_sd |
        abs(MIN_SI - mean_sd_by_day_and_name$MIN_SI_mean) > multiplier * mean_sd_by_day_and_name$MIN_SI_sd |
        abs(PRCP_SI - mean_sd_by_day_and_name$PRCP_SI_mean) > multiplier * mean_sd_by_day_and_name$PRCP_SI_sd |
        abs(SNDP_SI - mean_sd_by_day_and_name$SNDP_SI_mean) > multiplier * mean_sd_by_day_and_name$SNDP_SI_sd
    ) %>%
    select(NAME, DAY_OF_YEAR, TEMP_SI, DEWP_SI, WDSP_SI, MXSPD_SI, MAX_SI, MIN_SI, PRCP_SI, SNDP_SI)

  extreme_values_by_multiplier[[as.character(multiplier)]] <- extreme_values
}

# Print or use the extreme values for 3, 4, and 5 standard deviations from the mean grouped by day of the year and station name
for (multiplier in multipliers) {
  print(extreme_values_by_multiplier[[as.character(multiplier)]])
}
