install.packages("xts")
install.packages("tsbox")
weather_data <- read.csv("cleaned_data.csv")
#names(weather_data)
#tail(weather_data$NAME)
#range(weather_data$DATE)
library(xts)
library(tsbox)
library(dplyr)
library(lubridate)

# Convert DATE column to Date type
weather_data$DATE <- as.Date(weather_data$DATE)

# Function to perform analysis for a single station
analyze_station <- function(station_name = "AGARTALA, IN" ) {
  # Filter data for the selected station
  station_data <- filter(weather_data, NAME == station_name)
  
  # Check if station_data is not empty
  if (nrow(station_data) == 0) {
    stop("No data found for the selected station")
  }
  
  # Return the filtered data
  return(station_data)
}

# Call the function to filter data for "HYDERABAD INTERNATIONAL AIRPORT, IN" 
station_data <- analyze_station()

# Check the first few rows of the filtered data
head(station_data)

aggregated_data <- station_data %>%
  group_by(DATE) %>%
  summarize(MAX_SI = mean(MAX_SI),
            MIN_SI = mean(MIN_SI),
            PRCP_SI = mean(PRCP_SI))

# Convert aggregated data to xts object
historical= xts(aggregated_data[, c("MAX_SI", "MIN_SI", "PRCP_SI")], order.by = aggregated_data$DATE)

historical = ts_regular(historical)

historical = na.fill(historical, "extend")

historical = window(historical, start=as.Date("2000-01-01"), end=as.Date("2022-12-31"))

#A plot() of MIN_SI and MAX_SI show the annual cycle of temperatures as well as extreme temperature events that spike above the general curve.
plot(ts_ts(historical$MAX_SI), col="#d84315", bty="n", las=1, fg=NA, 
     ylim=c(-20, 120), ylab="Temperature (C)")

lines(ts_ts(historical$MIN_SI), col="#ffc107")

grid(nx=NA, ny=NULL, lty=1, col="black")

legend("topright", fill=c("#d84315", "#ffc107"), cex=0.7,
       legend=c("MAX_SI", "MIN_SI"), bg="white")


#The plot() of daily precipitation shows no clear seasonal pattern, although the presence of a limited number of high precipitation days 
barplot(historical$PRCP_SI, border=NA, col="#388E3C", ylim=c(0, 2),
        space=0, bty="n", las=1, fg=NA, ylab="Daily Rainfall (mm)")

grid(nx=NA, ny=NULL, lty=1)

#Analyze the Data
#Summary Descriptive Statistics
summary(historical)
historical[historical$MIN_SI == min(historical$MIN_SI)]
historical[historical$MAX_SI == max(historical$MAX_SI)]
historical[historical$PRCP_SI == max(historical$PRCP_SI)]

#Seasonal Statistics(months)
historical$MONTH = format(index(historical), "%m")
months = split(as.numeric(historical$MAX_SI), historical$MONTH)
sapply(months, summary)
months = split(as.numeric(historical$MIN_SI), historical$MONTH)
sapply(months, summary)
#Time Series Decomposition

# Linear interpolation
decomposition <- stl(na.approx(ts_ts(historical$MAX_SI)), s.window = 365, t.window = 7001)
plot(decomposition)
summary(decomposition$time.series[,"trend"])
#
decomposition <- stl(na.approx(ts_ts(historical$PRCP_SI)), s.window = 365, t.window = 7001)
plot(decomposition)
summary(decomposition$time.series[,"trend"])
#Aggregation
# monthly.tmax = period.apply(historical$MAX_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = mean)
# plot(ts_ts(monthly.tmax), col="darkred", ylim=c(20, 100), 
#      lwd=3, bty="n", las=1, fg=NA, ylab="MAX_SI (C)")
# grid(nx=NA, ny=NULL, lty=1)
# # Convert historical data to zoo object
historical_zoo <- as.zoo(historical)
# Aggregate data to monthly frequency
monthly_mean <- aggregate(historical_zoo, as.yearmon, mean)
# Plot the aggregated monthly data
plot(monthly_mean$MAX_SI, col = "darkred", ylim = c(20, 100),
     lwd = 3, bty = "n", las = 1, fg = NA, ylab = "MAX_SI (C)")

monthly.prcp = period.apply(historical$PRCP_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = sum)
plot(ts_ts(monthly.prcp), col="orange", 
      lwd=3, bty="n", las=1, fg=NA, ylab="Monthly Precipitation (mm)")
grid(nx=NA, ny=NULL, lty=1)
#plot(monthly_mean$PRCP_SI, col = "darkgreen", ylim = c(20, 100), 
  #   lwd = 3, bty = "n", las = 1, fg = NA, ylab = "MAX_SI (C)")


# #AREA PLOT (Area plots can be useful for showing extremes vs. averages with aggregated data.)
tmax.mean = period.apply(historical$MAX_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = mean)
tmax.max = period.apply(historical$MAX_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = max)
tmin.mean = period.apply(historical$MIN_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = mean)
tmin.min = period.apply(historical$MIN_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = min)

tmax.area = c(as.numeric(tmax.max), rev(as.numeric(tmax.mean)))
tavg.area = c(as.numeric(tmax.mean), rev(as.numeric(tmin.mean)))
tmin.area = c(as.numeric(tmin.mean), rev(as.numeric(tmin.min)))

# # Convert historical$MAX_SI to a regular time series object with monthly frequency
# monthly_tmax <- ts(historical$MAX_SI, frequency = 12)
# print(monthly_tmax)
# # Get the index of the monthly time series
# indices <- index(monthly_tmax)
# # Reverse the indices for the area plot
# rev_indices <- rev(indices)
# # Calculate the maximum temperature for each month across all years
# tmax.max <- apply(monthly_tmax, 2, max, na.rm = TRUE)
# # Continue with your code using rev_indices
# tmax.max <- period.apply(monthly_tmax, INDEX = seq(1, length(monthly_tmax), by = 30.4375), FUN = max)
# tmin.mean <- period.apply(historical$MIN_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = mean)
# tmin.min <- period.apply(historical$MIN_SI, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = min)
# tmax.area <- c(as.numeric(tmax.max), rev(as.numeric(tmax.mean)))
# tavg.area <- c(as.numeric(tmax.mean), rev(as.numeric(tmin.mean)))
# tmin.area <- c(as.numeric(tmin.mean), rev(as.numeric(tmin.min)))


# Use rev_indices in your subsequent code
indices = c(index(ts_ts(tmax.mean)), rev(index(ts_ts(tmax.mean))))

plot(NA, xlim=range(indices), ylim=c(-20, 100), 
     lwd=3, bty="n", las=1, fg=NA, ylab="Monthly Temperatures (C)")
polygon(indices, tmax.area, border=NA, col="darkred")
polygon(indices, tavg.area, border=NA, col="lightgray")
polygon(indices, tmin.area, border=NA, col="navy")
grid(nx=NA, ny=NULL, lty=1)


#Holt-Winters Forecasts
library(forecast)

training.data = period.apply(historical$MAX_SI, seq(1, nrow(historical) - 1, 30.4375), max)
model.tmax = hw(ts_ts(training.data), h=60)
plot(model.tmax, lwd=3, bty="n", las=1, fg=NA)
grid(nx=NA, ny=NULL, lty=1)
#-
model.tmax = hw(ts_ts(training.data), h=720)
plot(model.tmax, lwd=3, bty="n", las=1, fg=NA)
grid(nx=NA, ny=NULL, lty=1)


#ARIMA Forecasts
training.data = ts_ts(historical$MAX_SI)
parameters = auto.arima(training.data)
print(parameters)

arima.model = arima(training.data, order = c(5,0,1), seasonal = list(order=c(0,1,0), period=365))
arima.tmax = forecast(arima.model, 1825)
plot(arima.tmax, lwd=3, bty="n", las=1, fg=NA, 
     xlim=c(2018, 2026), ylab="Mean Monthly High Temperature (F)")
grid(nx=NA, ny=NULL, lty=1)

#Heating and Cooling Degree Days
historical$TAVG = (historical$MAX_SI + historical$MIN_SI) / 2
historical$HDD = ifelse(historical$TAVG < 65, 65 - historical$TAVG, 0)
historical$CDD = ifelse(historical$TAVG > 65, historical$TAVG - 65, 0)
monthly.cdd = period.apply(historical$CDD, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = sum)

monthly.hdd = period.apply(historical$HDD, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = sum)

barplot(merge.xts(monthly.cdd, monthly.hdd), 
        col=c("navy", "darkred"), las=1, fg="white",
        space=0, border=NA)
grid(nx=NA, ny=NULL, lty=1)
legend(x="topright", fill=c("navy", "darkred"), legend=c("CDD", "HDD"), bg="white")

decomposition = stl(ts_ts(historical$CDD), s.window=365, t.window=7001)
plot(decomposition)

decomposition = stl(ts_ts(historical$HDD), s.window=365, t.window=7001)
plot(decomposition)

#HDD and CDD can be estimated from monthly temperature averages.
historical$TAVG = (historical$MAX_SI + historical$MIN_SI) / 2
monthly.tavg = period.apply(historical$TAVG, INDEX = seq(1, nrow(historical) - 1, 30.4375), FUN = mean)
estimate.hdd = ifelse(monthly.tavg < 65, 65 - monthly.tavg, 0) * 30
estimate.cdd = ifelse(monthly.tavg > 65, monthly.tavg - 65, 0) * 30
barplot(merge.xts(estimate.cdd, estimate.hdd), 
        col=c("navy", "darkred"), las=1, fg="white",
        space=0, border=NA)
grid(nx=NA, ny=NULL, lty=1)
legend(x="topright", fill=c("navy", "darkred"), legend=c("CDD", "HDD"), bg="white")

#Extreme Precipitation Events
historical$FLOOD = ifelse(historical$PRCP_SI > 1, 1, 0)
flooding = period.apply(historical$FLOOD, INDEX = seq(1, nrow(historical) - 1, 365.25), FUN = sum)
plot(ts_ts(flooding), col="darkgreen", 
     lwd=3, bty="n", las=1, fg=NA, 
     ylab="Annual Days with High Precipitation")
grid(nx=NA, ny=NULL, lty=1)
summary(as.numeric(flooding))

decomposition <- stl(na.approx(ts_ts(historical$FLOOD)), s.window = 365, t.window = 7001)
plot(decomposition)
