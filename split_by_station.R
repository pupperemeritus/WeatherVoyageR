# Load necessary libraries
library(data.table)

# Set the main directory containing subdirectories by year
main_directory <- "/run/media/pupperemeritus/Seagate Expansion Drive/gsod_archive_parallel"

# Define the desired column names for the output CSV files
desired_column_names <- c(
  "STATION", "DATE", "LATITUDE", "LONGITUDE", "ELEVATION", "NAME",
  "TEMP", "TEMP_ATTRIBUTES", "DEWP", "DEWP_ATTRIBUTES", "SLP",
  "SLP_ATTRIBUTES", "STP", "STP_ATTRIBUTES", "VISIB", "VISIB_ATTRIBUTES",
  "WDSP", "WDSP_ATTRIBUTES", "MXSPD", "GUST", "MAX", "MAX_ATTRIBUTES",
  "MIN", "MIN_ATTRIBUTES", "PRCP", "PRCP_ATTRIBUTES", "SNDP", "FRSHTT"
)


# Get a list of all subdirectories (years) in the main directory
subdirectories <- list.dirs(main_directory, recursive = FALSE)

# Loop through each subdirectory (year)
for (subdir in subdirectories) {
  # Log progress
  cat("Processing files in directory:", subdir, "\n")

  # Set the working directory to the current subdirectory
  setwd(subdir)

  # Get a list of all CSV files in the current subdirectory
  csv_files <- list.files(pattern = "*.csv")

  # Loop through each CSV file in the current subdirectory
  for (file in csv_files) {
    # Log progress
    cat("Processing file:", file, "\n")

    # Read the CSV file into a data table
    data <- read.csv(file, header = TRUE, sep = ",")

    # Get unique station names from the data
    unique_stations <- unique(data$NAME)

    # Loop through each unique station
    for (station in unique_stations) {
      # Subset the data for the current station
      station_data <- data[data$NAME == station, ]

      # Define the output file name based on the station name
      output_file <- file.path("/home/pupperemeritus/minorproject2/combined_data/", paste0(station, ".csv"))

      # Create the output directory if it doesn't exist
      if (!dir.exists(dirname(output_file))) {
        dir.create(dirname(output_file), recursive = TRUE)
      }

      # Write the station data to the output file with desired column names
      write.table(station_data, file = output_file, row.names = FALSE, col.names = FALSE, append = TRUE)
    }
  }

  # Log progress
  cat("Completed processing files in directory:", subdir, "\n")
}

# Log completion of the entire script
cat("All files processed successfully.\n")

# Set the working directory back to the main directory after processing all subdirectories
setwd(main_directory)
