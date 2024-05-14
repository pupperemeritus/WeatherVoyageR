# Load necessary libraries
library(cluster)
library(leaflet)
library(dplyr)
library(leaflet.extras)

weather_data <- read.csv("./data/cleaned_data.csv")

# Select the columns needed for clustering
clustering_data <- weather_data[, c("LATITUDE", "LONGITUDE", "TEMP_SI", "PRCP_SI", "ELEVATION")]

scaled_data <- scale(clustering_data)

# Check for NA, NaN, and Inf
any_na <- anyNA(scaled_data)
any_nan <- any(apply(scaled_data, 2, function(x) any(is.nan(x))))
any_inf <- any(is.infinite(scaled_data))

# Handle NA by replacing with column mean
if (any_na) {
  scaled_data[is.na(scaled_data)] <- colMeans(scaled_data, na.rm = TRUE)
}

# Handle NaN by replacing with column mean
if (any_nan) {
  scaled_data[is.nan(scaled_data)] <- colMeans(scaled_data, na.rm = TRUE)
}

# Handle Inf by replacing with column mean
if (any_inf) {
  scaled_data[is.infinite(scaled_data)] <- colMeans(scaled_data, na.rm = TRUE)
}

# Determine the number of clusters (you can change this)
num_clusters <- 7

# Perform k-means clustering after handling NA, NaN, and Inf
kmeans_result <- kmeans(scaled_data, centers = num_clusters)

# Add cluster assignments to the original data frame
weather_data$Cluster <- kmeans_result$cluster

# Create a color palette for clusters
cluster_colors <- rainbow(num_clusters)

# Create a leaflet map
map <- leaflet() %>%
  setView(lng = 78.9629, lat = 20.5937, zoom = 5) # Center the map on India
cluster_options <- markerClusterOptions(lazyLoading = TRUE, chunkedLoading = TRUE)

map <- map %>% addCircleMarkers(
  lng = weather_data$LONGITUDE,
  lat = weather_data$LATITUDE,
  data = weather_data$Cluster,
  radius = 5,
  color = cluster_colors,
  fill = FALSE,
  clusterOptions = cluster_options
)

# Display the map
map
