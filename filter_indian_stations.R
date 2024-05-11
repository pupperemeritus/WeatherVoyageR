# Set file paths
data_folder <- "/home/pupperemeritus/minorproject2/combined_data"
output_folder <- "/home/pupperemeritus/minorproject2/indian_stations_output"

# List files in data folder
files <- list.files(path = data_folder, pattern = ", IN.csv", full.names = TRUE)

# Create output folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Initialize progress counter
progress_count <- 0

# Function to count number of years of data for a station
count_years <- function(file_path) {
  data <- read.csv(file_path,sep=" ")
  n_years <- nrow(data)
  print(n_years)
  n_years/365
}

# Loop through files
for (file_path in files) {
  # Extract station name from file name
  file_name <- basename(file_path)
  station_name <- gsub(".csv", "", file_name)
  
  # Count years of data for the station
  years_count <- count_years(file_path)
  
  # If station has at least 4 years of data, move file to output folder
  if (years_count >= 4) {
    file.copy(file_path, file.path(output_folder, file_name))
    cat("Moved file:", station_name, "\n")
  } else {
    cat("Skipping file:", station_name, "due to less than 4 years of data.\n")
  }
  
  # Increment progress counter
  progress_count <- progress_count + 1
  cat("Progress:", progress_count, "out of", length(files), "files processed.\n")
}

cat("Files moved successfully.\n")
