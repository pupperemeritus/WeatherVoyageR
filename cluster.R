install.packages('shinydashboard')
library(shiny)
library(cluster)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(htmlwidgets)

# Load data
weather_data <- read.csv("cleaned_data.csv")

# Convert DATE column to Date type
weather_data$DATE <- as.Date(weather_data$DATE)

# Extract season information from dates (assuming seasons are defined as quarters)
weather_data$Season <- cut(
  as.POSIXlt(weather_data$DATE)$mon + 1, # Adding 1 because months are indexed from 0
  breaks = c(0, 2, 3, 6, 9, 11, 12),
  labels = c("Winter", "Spring", "Summer", "Monsoon", "Fall", "Winter"), # Include Winter twice for full cycle
  include.lowest = TRUE
)

# Determine the number of clusters (you can change this)
num_clusters <- 6

# Create a color palette for clusters
cluster_colors <- rainbow(num_clusters)

createSeasonMap <- function(season_name, zoom_level = 2) {
  # Filter data for the specified season
  season_data <- weather_data %>% filter(Season == season_name)
  
  # Group by Station and calculate average values
  grouped_data <- season_data %>%
    group_by(STATION) %>%
    summarize(
      AVG_TEMP_SI = mean(TEMP_SI, na.rm = TRUE),
      AVG_PRCP_SI = mean(PRCP_SI, na.rm = TRUE),
      AVG_ELEVATION = mean(ELEVATION, na.rm = TRUE),
      LONGITUDE = first(LONGITUDE), # Add longitude column
      LATITUDE = first(LATITUDE) # Add latitude column
    ) %>%
    ungroup()
  
  # Scale the data
  scaled_data <- scale(grouped_data[, c("AVG_TEMP_SI", "AVG_PRCP_SI", "AVG_ELEVATION")])
  
  # Perform k-means clustering
  kmeans_result <- kmeans(scaled_data, centers = num_clusters)
  
  # Add cluster assignments to the grouped data
  grouped_data$Cluster <- kmeans_result$cluster
  
  # Calculate cluster sizes for adjusting zoom level
  cluster_sizes <- table(kmeans_result$cluster)
  max_cluster_size <- max(cluster_sizes)
  zoom_adjustment <- ceiling(log10(max_cluster_size)) # Adjust zoom based on cluster size
  
  # Create a leaflet map to visualize the clustered data for this season
  map <- leaflet(data = grouped_data) %>%
    addTiles() %>%
    addCircleMarkers(
      radius = 5,
      color = cluster_colors[grouped_data$Cluster],
      fillOpacity = 0.8 # Adjust fill opacity
    ) %>%
    addLegend(
      "topleft",
      colors = cluster_colors,
      labels = paste("Cluster", 1:num_clusters),
      opacity = 1,
      title = "Clusters"
    ) %>%
    setView(lng = mean(grouped_data$LONGITUDE), lat = mean(grouped_data$LATITUDE), zoom = zoom_level + zoom_adjustment) %>%
    # Add JavaScript code to handle marker click events and show popup with details
    onRender("
      function(map) {
        this.on('click', function(event) {
          var marker = event.layer;
          marker.bindPopup(
            '<b>Station:</b> ' + marker.options.title + '<br>' +
            '<b>Avg Temperature:</b> ' + marker.options.avg_temp + '<br>' +
            '<b>Avg Precipitation:</b> ' + marker.options.avg_prcp + '<br>' +
            '<b>Avg Elevation:</b> ' + marker.options.avg_elevation
          ).openPopup();
        });
      }
    ")
  for (i in 1:nrow(grouped_data)) {
    map <- map %>%
      addCircleMarkers(
        lng = grouped_data$LONGITUDE[i],
        lat = grouped_data$LATITUDE[i],
        radius = 5,
        color = cluster_colors[grouped_data$Cluster[i]],
        fillOpacity = 0.8,
        popup = paste(
          "<b>Station:</b> ", grouped_data$STATION[i], "<br>",
          "<b>Avg Temperature:</b> ", round(grouped_data$AVG_TEMP_SI[i], 2), "<br>",
          "<b>Avg Precipitation:</b> ", round(grouped_data$AVG_PRCP_SI[i], 2), "<br>",
          "<b>Avg Elevation:</b> ", round(grouped_data$AVG_ELEVATION[i], 2)
        )
      )
  }
  
  return(map)
}

# Define UI for the application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Seasonal Weather Clustering",
    titleWidth = 600
  ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "tabs",
      menuItem("Winter", tabName = "winter", icon = icon("snowflake")), # Updated icon names
      menuItem("Spring", tabName = "spring", icon = icon("tree")), # Updated icon names
      menuItem("Monsoon", tabName = "monsoon", icon = icon("cloud-rain")), # Updated icon names
      menuItem("Summer", tabName = "summer", icon = icon("sun")), # Updated icon names
      menuItem("Fall", tabName = "fall", icon = icon("leaf")) # Updated icon names
    )
  ),
  dashboardBody(
    tags$style(
      HTML("
      .content-wrapper, .right-side {
        margin-left: 200px; /* Sidebar width */
      }
      .main-sidebar {
        width: 200px;
      }
      .leaflet-container {
        height: 100vh !important;
        width: calc(100vw - 200px) !important; /* Full width minus sidebar width */
        margin: 0;
        padding: 0;
      }
      .leaflet-bottom.leaflet-right {
        right: 10px !important;
        bottom: 10px !important;
        padding: 5px;
      }
      .leaflet-control {
        font-size: 12px !important;
      }
      ")
    ),
    tabItems(
      tabItem(
        tabName = "winter",
        leafletOutput("map_winter", width = "100%", height = "100%")
      ),
      tabItem(
        tabName = "spring",
        leafletOutput("map_spring", width = "100%", height = "100%")
      ),
      tabItem(
        tabName = "summer",
        leafletOutput("map_summer", width = "100%", height = "100%")
      ),
      tabItem(
        tabName = "fall",
        leafletOutput("map_fall", width = "100%", height = "100%")
      ),
      tabItem(
        tabName = "monsoon",
        leafletOutput("map_monsoon", width = "100%", height = "100%")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$map_winter <- renderLeaflet({
    createSeasonMap("Winter")
  })
  output$map_spring <- renderLeaflet({
    createSeasonMap("Spring")
  })
  output$map_summer <- renderLeaflet({
    createSeasonMap("Summer")
  })
  output$map_monsoon <- renderLeaflet({
    createSeasonMap("Monsoon")
  })
  output$map_fall <- renderLeaflet({
    createSeasonMap("Fall")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
