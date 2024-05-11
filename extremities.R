library(dplyr)

# Assuming you have loaded and preprocessed your weather data as 'weather_data'

# Step 1: Calculate Means and Standard Deviations for every date
daily_means_stds <- weather_data %>%
  group_by(NAME, MonthDay) %>%
  summarize(
    TEMP_SI_mean = mean(TEMP_SI),
    TEMP_SI_std = sd(TEMP_SI),
    WDSP_SI_mean = mean(WDSP_SI),
    WDSP_SI_std = sd(WDSP_SI),
    PRCP_SI_mean = mean(PRCP_SI),
    PRCP_SI_std = sd(PRCP_SI)
  ) %>%
  ungroup()

# Step 2: Identify Events Exceeding Standard Deviations for every date
find_extreme_events <- function(row) {
  deviations <- c(TEMP_SI = 1, WDSP_SI = 2, PRCP_SI = 3)  # Change values as needed
  extreme_events <- list()
  for (var in names(deviations)) {
    mean_val <- row[[paste0(var, "_mean")]]
    std_val <- row[[paste0(var, "_std")]]
    threshold <- deviations[var] * std_val
    extreme_events[[var]] <- threshold
  }
  return(extreme_events)
}

daily_extreme_events <- daily_means_stds %>%
  rowwise() %>%
  mutate(extreme_events = list(find_extreme_events(cur_data()))) %>%
  ungroup()

# Displaying extreme events exceeding 1, 2, 3 standard deviations for every date
daily_events_1_std <- daily_extreme_events %>%
  filter(TEMP_SI_mean > extreme_events$TEMP_SI | WDSP_SI_mean > extreme_events$WDSP_SI | PRCP_SI_mean > extreme_events$PRCP_SI)

daily_events_2_std <- daily_extreme_events %>%
  filter(TEMP_SI_mean > extreme_events$TEMP_SI & WDSP_SI_mean > extreme_events$WDSP_SI |
           WDSP_SI_mean > extreme_events$WDSP_SI & PRCP_SI_mean > extreme_events$PRCP_SI)

daily_events_3_std <- daily_extreme_events %>%
  filter(TEMP_SI_mean > extreme_events$TEMP_SI & WDSP_SI_mean > extreme_events$WDSP_SI & PRCP_SI_mean > extreme_events$PRCP_SI)

# print("Events exceeding 1 standard deviation for every date:")
print(daily_events_1_std)

print("\nEvents exceeding 2 standard deviations for every date:")
print(daily_events_2_std)

print("\nEvents exceeding 3 standard deviations for every date:")
print(daily_events_3_std)

