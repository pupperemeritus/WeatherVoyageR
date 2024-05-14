library(dplyr)

# Get the list of files in the directory
files <- list.files("indian_stations_output", full.names = TRUE)

# Initialize an empty data frame to hold the combined data
combined_data <- data.frame()
desired_column_names <- c(
  "STATION", "DATE", "LATITUDE", "LONGITUDE", "ELEVATION", "NAME",
  "TEMP", "TEMP_ATTRIBUTES", "DEWP", "DEWP_ATTRIBUTES", "SLP",
  "SLP_ATTRIBUTES", "STP", "STP_ATTRIBUTES", "VISIB", "VISIB_ATTRIBUTES",
  "WDSP", "WDSP_ATTRIBUTES", "MXSPD", "GUST", "MAX", "MAX_ATTRIBUTES",
  "MIN", "MIN_ATTRIBUTES", "PRCP", "PRCP_ATTRIBUTES", "SNDP", "FRSHTT"
)

# Loop through each file and read it into a data frame
for (file in files) {
  # Read the file into a data frame
  data <- read.csv(file, sep = " ", header = FALSE)
  print(data)
  # Append the data to the combined data framea
  combined_data <- bind_rows(combined_data, data)
}

# Write the combined data to a csv file
write.csv(combined_data, "combined_data.csv", sep = ",", row.names = FALSE, col.names = desired_column_names)
