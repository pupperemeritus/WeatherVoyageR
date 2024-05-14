library(ggplot2)
library(viridis)

weather_data <- read.csv("./cleaned_data.csv")

# Combine the attributes into a single dataframe
weather_attributes <- c("Fog", "Rain_Drizzle", "Snow_Ice_Pellets", "Hail", "Thunder", "Tornado_Funnel_Cloud")
combined_data <- cbind(weather_data["TEMP_SI"], sapply(weather_attributes, function(attr) factor(weather_data[[attr]])))

# Reshape the data for plotting
combined_data_long <- tidyr::pivot_longer(combined_data, cols = -TEMP_SI, names_to = "Attribute", values_to = "Presence")

# Faceted density plot of temperature by weather attributes with discrete color scale
density_plot <- ggplot(combined_data_long, aes(x = TEMP_SI, fill = Presence)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Attribute, scales = "free") + # Facet by weather attributes
  labs(
    title = "Density Plot of Temperature by Weather Attribute",
    x = "Temperature (°C)",
    y = "Density"
  ) +
  scale_fill_viridis_d(option = "D") + # Use Viridis color palette
  theme_minimal()

# Print the plot
print(density_plot)
