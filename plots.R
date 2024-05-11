library(ggplot2)
library(dplyr)

weather_data$DATE <- as.Date(weather_data$DATE)

# 1. Time Series Plot for Temperature
ggplot(weather_data, aes(x = DATE, y = TEMP_SI)) +
  geom_line() +
  labs(title = "Temperature Trends Over Time", x = "Date", y = "Temperature (°C)") +
  facet_wrap(~STATION)

# 2. Histograms/Density Plots for Temperature and Dew Point
ggplot(weather_data, aes(x = TEMP_SI)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Temperature Distribution", x = "Temperature (°C)", y = "Frequency") +
  facet_wrap(~STATION)

ggplot(weather_data, aes(x = DEWP_SI)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Dew Point Distribution", x = "Dew Point (°C)", y = "Frequency") +
  facet_wrap(~STATION)

# 3. Scatter Plot for Temperature vs Dew Point
ggplot(weather_data, aes(x = DEWP_SI, y = TEMP_SI)) +
  geom_point() +
  labs(title = "Temperature vs Dew Point", x = "Dew Point (°C)", y = "Temperature (°C)") +
  facet_wrap(~STATION)

# 4. Box Plots for Temperature by Weather Event
ggplot(weather_data, aes(x = Fog, y = TEMP_SI)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Temperature Distribution by Fog", x = "Fog (0 = No, 1 = Yes)", y = "Temperature (°C)") +
  facet_wrap(~STATION)

# 5. Bar Plot for Count of Weather Events
weather_events <- weather_data %>%
  summarize_all(sum) %>%
  gather(key = "Event", value = "Count", Fog:Tornado_Funnel_Cloud)

ggplot(weather_events, aes(x = Event, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Count of Weather Events", x = "Weather Event", y = "Count") +
  facet_wrap(~STATION)

# 6. Correlation Matrix Heatmap
correlation_matrix <- cor(weather_data[, c("TEMP_SI", "DEWP_SI", "WDSP_SI", "VISIB_SI")])

ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Correlation Matrix Heatmap",
       x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

