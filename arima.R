#datta
install.packages('astsa')
library(shiny)
library(astsa)
library(dplyr)
library(forecast)
# Load your cleaned data
weather_data <- read.csv("cleaned_data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Weather Forecasting App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Select Station:",
                  choices = unique(weather_data$NAME)),
      numericInput("forecast_days", "Number of Forecast Days:", value = 7, min = 1, max = 30),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("forecast_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$submit, {
    # Filter data based on selected station
    selected_station_data <- filter(weather_data, NAME == input$station)
    
    if (nrow(selected_station_data) == 0) {
      # Handle case where no data is available for the selected station
      # You can show an error message or take appropriate action
      return(NULL)
    }
    
    # Check for missing values in the temperature data
    if (any(is.na(selected_station_data$TEMP_SI))) {
      # Handle missing values (e.g., impute or remove)
      # Here's a simple example of imputation with mean
      selected_station_data$TEMP_SI[is.na(selected_station_data$TEMP_SI)] <- mean(selected_station_data$TEMP_SI, na.rm = TRUE)
    }
    
    # Convert date to time series object
    weather_ts <- ts(selected_station_data$TEMP_SI, frequency = 365)
    
    tryCatch({
      # Fit ARIMA model using astsa package
      arima_model <- arima(weather_ts, order = c(1, 0, 1))
      
      # Forecast
      forecast_values <- forecast(arima_model, h = input$forecast_days)
      
      # Plot forecast
      output$forecast_plot <- renderPlot({
        plot(forecast_values, main = "Temperature Forecast", xlab = "Date", ylab = "Temperature")
      })
    }, error = function(e) {
      # Handle errors
      cat("An error occurred:", e$message, "\n")
      # You can display an error message or take other actions as needed
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


