# Load necessary libraries
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

# Read the CSV file
weather_data <- read.csv("./data/cleaned_data.csv")

# Convert 'DATE' column to date format
weather_data$DATE <- as.Date(weather_data$DATE)

# Extract day of the year (1-365/366) from the 'DATE' column
weather_data$DAY_OF_YEAR <- yday(weather_data$DATE)

# Identify numeric columns
numeric_cols <- c(
    "TEMP_SI", "DEWP_SI",
    "WDSP_SI", "MXSPD_SI", "MAX_SI", "MIN_SI", "PRCP_SI", "SNDP_SI"
)
weather_data[numeric_cols] <- lapply(weather_data[numeric_cols], as.numeric)

# Calculate and filter extreme values (3, 4, 5 standard deviations from the mean) grouped by day of the year and station name
multipliers <- c(3, 4, 5)
extreme_values_by_multiplier <- list()

# Create a lookup table for mean and standard deviation by day of the year and station name
mean_sd_lookup <- weather_data %>%
    group_by(DAY_OF_YEAR, NAME) %>%
    summarise(
        TEMP_SI_mean = mean(TEMP_SI, na.rm = TRUE),
        TEMP_SI_sd = sd(TEMP_SI, na.rm = TRUE),
        DEWP_SI_mean = mean(DEWP_SI, na.rm = TRUE),
        DEWP_SI_sd = sd(DEWP_SI, na.rm = TRUE),
        WDSP_SI_mean = mean(WDSP_SI, na.rm = TRUE),
        WDSP_SI_sd = sd(WDSP_SI, na.rm = TRUE),
        MXSPD_SI_mean = mean(MXSPD_SI, na.rm = TRUE),
        MXSPD_SI_sd = sd(MXSPD_SI, na.rm = TRUE),
        MAX_SI_mean = mean(MAX_SI, na.rm = TRUE),
        MAX_SI_sd = sd(MAX_SI, na.rm = TRUE),
        MIN_SI_mean = mean(MIN_SI, na.rm = TRUE),
        MIN_SI_sd = sd(MIN_SI, na.rm = TRUE),
        PRCP_SI_mean = mean(PRCP_SI, na.rm = TRUE),
        PRCP_SI_sd = sd(PRCP_SI, na.rm = TRUE),
        SNDP_SI_mean = mean(SNDP_SI, na.rm = TRUE),
        SNDP_SI_sd = sd(SNDP_SI, na.rm = TRUE),
        .groups = "drop"
    )

for (multiplier in multipliers) {
    extreme_values <- weather_data %>%
        left_join(mean_sd_lookup, by = c("DAY_OF_YEAR", "NAME")) %>%
        filter(
            abs(TEMP_SI - TEMP_SI_mean) > multiplier * TEMP_SI_sd |
                abs(DEWP_SI - DEWP_SI_mean) > multiplier * DEWP_SI_sd |
                abs(WDSP_SI - WDSP_SI_mean) > multiplier * WDSP_SI_sd |
                abs(MXSPD_SI - MXSPD_SI_mean) > multiplier * MXSPD_SI_sd |
                abs(MAX_SI - MAX_SI_mean) > multiplier * MAX_SI_sd |
                abs(MIN_SI - MIN_SI_mean) > multiplier * MIN_SI_sd |
                abs(PRCP_SI - PRCP_SI_mean) > multiplier * PRCP_SI_sd |
                abs(SNDP_SI - SNDP_SI_mean) > multiplier * SNDP_SI_sd
        ) %>%
        select(NAME, DATE, DAY_OF_YEAR, TEMP_SI, DEWP_SI, WDSP_SI, MXSPD_SI, MAX_SI, MIN_SI, PRCP_SI, SNDP_SI)

    extreme_values_by_multiplier[[as.character(multiplier)]] <- extreme_values
}

# UI
ui <- fluidPage(
    titlePanel("Weather Station Extreme Values Analysis"),

    sidebarLayout(
        sidebarPanel(
            selectInput("station", "Select Station:", choices = unique(weather_data$NAME)),
            selectInput("multiplier", "Select Extremity Level:", choices = multipliers, selected = 3),
            selectInput("attribute", "Select Attribute:", choices = c(
                "Temperature" = "TEMP_SI",
                "Dew Point" = "DEWP_SI",
                "Wind Speed" = "WDSP_SI",
                "Max Wind Speed" = "MXSPD_SI",
                "Max Temperature" = "MAX_SI",
                "Min Temperature" = "MIN_SI",
                "Precipitation" = "PRCP_SI",
                "Snow Depth" = "SNDP_SI"
            ), selected = "TEMP_SI")
        ),

        mainPanel(
            plotOutput("extremePlot")
        )
    )
)

library(viridis)

# Server
server <- function(input, output) {
    selected_data <- reactive({
        req(input$station, input$multiplier, input$attribute)
        multiplier <- as.character(input$multiplier)
        extreme_values_by_multiplier[[multiplier]] %>%
            filter(NAME == input$station) %>%
            select(DATE, DAY_OF_YEAR, NAME, !!sym(input$attribute))
    })

    output$extremePlot <- renderPlot({
        data <- selected_data()
        attribute <- input$attribute

        if (nrow(data) > 0) {
            # Filter out rows with NA in the selected attribute
            data <- data %>%
                filter(!is.na(!!sym(attribute))) %>%
                group_by(DAY_OF_YEAR) %>%
                summarise(count = n(), .groups = 'drop')

            ggplot(data, aes(x = DAY_OF_YEAR, y = count, fill = count)) +
                geom_bar(stat = "identity", alpha = 0.8) +
                labs(
                    title = paste("Extreme Values for Station:", input$station),
                    x = "Day of Year", y = "Count",
                    fill = "Count"
                ) +
                scale_fill_viridis_c(option = "D") +
                theme_minimal() +
                theme(
                    text = element_text(family = "Arial", color = "black"),
                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                    axis.title = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    legend.title = element_text(size = 14),
                    legend.text = element_text(size = 12),
                    panel.grid.major = element_line(color = "grey90"),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "white", color = "black"),
                    plot.background = element_rect(fill = "white", color = "black")
                ) +
                scale_y_continuous(limits = c(0, NA))  # Bound the y-axis from going below 0
        } else {
            ggplot() +
                labs(
                    title = "No extreme values found for the selected station and extremity level.",
                    x = "Day of Year", y = "Count"
                )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)