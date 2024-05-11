# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

# Load data

# UI
ui <- fluidPage(
  titlePanel("Weather Station Analysis"),
  
  # Leaflet map
  leafletOutput("map"),
  
  # Dropdown menu for selecting weather stations
  selectInput("station", "Select Station:", choices = unique(weather_data$NAME)),
  
  # Display station details
  verbatimTextOutput("station_details")
)

# Server
server <- function(input, output, session) {
  
  weather_data <- reactiveFileReader(intervalMillis = 1000, session,
                                     filePath = "cleaned_data.csv",
                                     readFunc = read.csv)
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 78.9629, lat = 20.5937, zoom = 5)
  })
  
  # Filter data for selected station
  selected_station <- reactive({
    req(input$station)
    filter(weather_data(), NAME == input$station)
  })
  
  # Update map marker when station is selected
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = selected_station(),
                 lng = ~LONGITUDE,
                 lat = ~LATITUDE,
                 popup = ~paste("Station:", STATION, "<br>",
                                "Name:", NAME, "<br>",
                                "Elevation:", ELEVATION, "<br>",
                                "Temperature:", TEMP, "°C"),
                 label = ~paste("Avg Temp:", round(mean(selected_station()$TEMP, na.rm = TRUE), 2), "°C"),
                 labelOptions = labelOptions(direction = "auto"))
  })
  
  # Calculate average temperature and most occurrence of FRSHTT type year-wise
  output$stats <- renderPrint({
    station <- selected_station()
    if (nrow(station) > 0) {
      stats <- station %>%
        group_by(year = year(DATE)) %>%
        summarise(Avg_Temperature = mean(TEMP, na.rm = TRUE),
                  Most_FRSHTT = names(which.max(table(FRSHTT))))
      stats
    } else {
      "No data available for selected station."
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
