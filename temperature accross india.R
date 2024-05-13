library(ggplot2)
weather_data <- read.csv("cleaned_data.csv")
# Plot temperature values on a map of India with a contrast gradient
india_map <- ggplot(weather_data, aes(x = LONGITUDE, y = LATITUDE, color = TEMP_SI)) +
  geom_point() +
  labs(title = "Temperature Distribution Across India",
       x = "Longitude",
       y = "Latitude",
       color = "Temperature (°C)") +
  scale_color_gradientn(colors = c("blue", "green", "red"))  # Contrast gradient color scale
ggsave("india_map.png", india_map, width = 10, height = 6)

# Time series analysis
time_series_plot <- ggplot(weather_data, aes(x = DATE, y = TEMP_SI)) +
  geom_line() +
  labs(title = "Time Series Plot of Temperature",
       x = "Date",
       y = "Temperature (°C)")
ggsave("time_series_plot.png", time_series_plot, width = 10, height = 6)

library(ggplot2)



