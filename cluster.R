# Load necessary libraries
library(cluster)
library(leaflet)

# Select the columns needed for clustering
clustering_data <- weather_data[, c("LATITUDE", "LONGITUDE", "TEMP_SI", "PRCP_SI", "ELEVATION")]

# Standardize the data
scaled_data <- scale(clustering_data)

# Determine the number of clusters (you can change this)
num_clusters <- 3

# Perform k-means clustering
kmeans_result <- kmeans(scaled_data, centers = num_clusters)

# Add cluster assignments to the original data frame
weather_data$Cluster <- kmeans_result$cluster

# Create a color palette for clusters
cluster_colors <- colorFactor(palette = "Set1", domain = weather_data$Cluster)

# Create a leaflet map
map <- leaflet() %>%
  setView(lng = 78.9629, lat = 20.5937, zoom = 5)  # Center the map on India

# Add markers for each station with cluster color
for (i in 1:nrow(weather_data)) {
  map <- addMarkers(map, 
                    lng = weather_data$LONGITUDE[i], 
                    lat = weather_data$LATITUDE[i], 
                    cluster = weather_data$Cluster[i], 
                    label = weather_data$STATION[i],
                    color = cluster_colors(weather_data$Cluster[i]))
}

# Display the map
map
