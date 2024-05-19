# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

# Load data outside server for better performance
weather_data <- read.csv("./data/cleaned_data.csv")
station_data <- NULL

# UI
ui <- fluidPage(
  titlePanel("Weather Station Analysis"),

  # Leaflet map
  leafletOutput("map"),

  # Dropdown menu for selecting weather stations
  selectInput("station", "Select Station:", choices = NULL),

  # Display station details
  verbatimTextOutput("station_details"),

  # Display statistics
  verbatimTextOutput("stats")
)

# Server
server <- function(input, output, session) {
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 78.9629, lat = 20.5937, zoom = 5)
  })

  # Update station dropdown choices
  observe({
    updateSelectInput(session, "station", choices = unique(weather_data$NAME))
  })

  # Filter data for selected station
  selected_station <- reactive({
    req(input$station)
    filter(weather_data, NAME == input$station)
  })

  # Update map marker when station is selected
  observe({
    station <- selected_station()
    if (nrow(station) > 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(
          data = station,
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          popup = ~ paste(
            "Station:", STATION, "<br>",
            "Name:", NAME, "<br>",
            "Elevation:", ELEVATION, "<br>",
            "Temperature:", TEMP_SI, "°C"
          ),
          label = ~ paste("Avg Temp:", round(mean(TEMP_SI, na.rm = TRUE), 2), "°C"),
          labelOptions = labelOptions(direction = "auto")
        )
    }
  })

  # Calculate average temperature and most occurrence of FRSHTT type year-wise
  output$stats <- renderPrint({
    station <- selected_station()
    if (nrow(station) > 0) {
      stats <- station %>%
        group_by(year = year(DATE)) %>%
        summarise(
          Avg_Temperature = mean(TEMP_SI, na.rm = TRUE),
          Most_FRSHTT = names(which.max(table(FRSHTT)))
        )
      print(stats)
    } else {
      "No data available for selected station."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
