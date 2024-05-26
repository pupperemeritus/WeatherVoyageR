#code actual vs pred
# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(randomForest)
library(lubridate)
library(caret)
head(weather_data)
total_records <- nrow(weather_data)
print(total_records)
# Data Preprocessing
weather_data <- weather_data %>%
  mutate(DATE = as.Date(DATE),
         Year = year(DATE),
         Month = month(DATE),
         Day = day(DATE)) %>%
  filter(!is.na(TEMP_SI))

# Define the UI
ui <- fluidPage(
  titlePanel("Predicting Temperature on given date for selected station"),
  sidebarLayout(
    sidebarPanel(
      selectInput("station_name", "Select Station:", choices = unique(weather_data$NAME)),
      dateInput("date", "Select Date:", value = Sys.Date()),
      actionButton("predict", "Predict Temperature")
    ),
    mainPanel(
      textOutput("prediction"),
      plotOutput("tempPlot"),
      textOutput("accuracy")
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  observeEvent(input$predict, {
    station_data <- weather_data %>%
      filter(NAME == input$station_name)
    
    # Feature Engineering
    station_data <- station_data %>%
      mutate(DayOfYear = yday(DATE))
    
    # Model Training
    set.seed(123)
    train_index <- createDataPartition(station_data$TEMP_SI, p = 0.8, list = FALSE)
    train_data <- station_data[train_index, ]
    test_data <- station_data[-train_index, ]
    
    rf_model <- randomForest(TEMP_SI ~ DayOfYear + Year + Month + Day, data = train_data, ntree = 100)
    
    # Predict temperature for the selected date
    future_date <- as.Date(input$date)
    future_data <- data.frame(
      DayOfYear = yday(future_date),
      Year = year(future_date),
      Month = month(future_date),
      Day = day(future_date)
    )
    predicted_temp <- predict(rf_model, newdata = future_data)
    
    # Calculate Metrics
    actual_temp <- test_data$TEMP_SI
    predicted_temp_test <- predict(rf_model, newdata = test_data)
    mae <- mean(abs(predicted_temp_test - actual_temp))
    mse <- mean((predicted_temp_test - actual_temp)^2)
    r_squared <- cor(predicted_temp_test, actual_temp)^2
    
    # Check if the selected date exists in the weather data
    actual_temp_for_date <- station_data %>%
      filter(DATE == future_date) %>%
      select(TEMP_SI) %>%
      pull()
    
    output$prediction <- renderText({
      if(length(actual_temp_for_date) > 0) {
        paste("Predicted Temperature on", input$date, ":", round(predicted_temp, 2), "°C\n",
              "Actual Temperature on", input$date, ":", round(actual_temp_for_date, 2), "°C")
      } else {
        paste("Predicted Temperature on", input$date, ":", round(predicted_temp, 2), "°C\n",
              "No actual temperature data available for this date.")
      }
    })
    
    output$tempPlot <- renderPlot({
      ggplot(train_data, aes(x = DATE, y = TEMP_SI)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_vline(xintercept = as.numeric(future_date), linetype = "dashed", color = "red") +
        annotate("point", x = future_date, y = predicted_temp, color = "red", size = 3) +
        annotate("text", x = future_date, y = predicted_temp, label = round(predicted_temp, 2), vjust = -1, color = "red") +
        theme_minimal() +
        ggtitle(paste("Temperature Prediction for Station:", input$station_name)) +
        xlab("Date") + ylab("Temperature (°C)")
    }, height = 400, width = 600)
    
    output$accuracy <- renderText({
      paste("Mean Absolute Error (MAE):", round(mae, 2), "°C\n",
            "Mean Squared Error (MSE):", round(mse, 2), "°C\n",
            "R-squared:", round(r_squared, 2))
    })
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
