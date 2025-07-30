# Weather Data Analysis and Visualization for Indian Stations

### Authors: Rohith Nagula Malyala, Sri Guru Datta Pisupati, Sanjeev Varma Katari

---

## Project Overview

This project presents a comprehensive workflow for analyzing and visualizing weather data from the Global Summary of the Day (GSOD) dataset, with a specific focus on stations across India. The entire process, from data acquisition and cleaning to exploratory analysis and the development of interactive applications, is implemented using R and Shiny.

The project transforms raw, complex weather data into actionable insights through a series of structured steps:

1. **Automated Data Pipeline**: Fetches, extracts, and organizes decades of weather data.
2. **Rigorous Cleaning**: Implements robust data cleaning, unit conversion, and feature engineering.
3. **Exploratory Analysis**: Generates static and interactive plots to uncover trends, distributions, and correlations.
4. **Interactive Dashboards**: Deploys several Shiny web applications for interactive data exploration, clustering, extreme event analysis, and forecasting.

These analyses and applications serve as valuable tools for researchers, analysts, and stakeholders involved in weather-related studies and decision-making.

---

## Features

- **Automated Data Ingestion**: Python script to download and extract GSOD data from the NCEI archives.
- **Data Processing Pipeline**: R scripts to organize, filter for Indian stations, combine, and clean the data.
- **Feature Engineering**: Converts units from imperial to SI and decodes the `FRSHTT` (Fog, Rain, Snow, etc.) bitmask into individual indicator columns.
- **Exploratory Plots**: A suite of plots to visualize temperature distributions, time-series trends, and spatial data.
- **Interactive Shiny Applications**:
  - An interactive map for station-specific analysis.
  - A seasonal clustering dashboard to group stations with similar weather patterns.
  - An extreme weather event analyzer to identify and visualize outliers.
  - Forecasting tools using ARIMA, SARIMA, and Random Forest models.

---

## ⚙️ Data Processing Pipeline

The data is prepared for analysis through a multi-step pipeline that ensures it is clean, organized, and relevant.

### Step 1: Download and Extract GSOD Data

A Python script automates the download of compressed `tar.gz` archive files for each year (from 1929 to 2023) from the NCEI/NOAA data source. It uses a `ThreadPoolExecutor` for parallel processing to speed up the download and extraction. After extraction, the script deletes the archives to save space.

### Step 2: Organize Data by Station

An R script processes the extracted files from each year. It reads the raw data and splits it into individual CSV files, one for each unique weather station, using the station name as the filename. This organizes the global dataset into a more manageable structure.

### Step 3: Filter for Indian Stations

To focus the analysis on India, a filtering script selects only the relevant stations. It identifies files corresponding to Indian stations (ending in `, IN.csv`) and checks if the station has at least **four years** of data to ensure a sufficiently robust dataset for analysis.

### Step 4: Combine Indian Station Data

The filtered files for Indian stations are read and consolidated into a single master CSV file named `combined_data.csv`. This creates a unified dataset for the next stage of cleaning.

### Step 5: Clean, Transform, and Engineer Features

The combined data undergoes a significant cleaning and transformation process:

- **Missing Value Handling**: Placeholder values (e.g., `9999.9`, `99.99`) are replaced with `NA`. `NA` values in the snow depth (`SNDP`) column are replaced with 0.
- **Column Pruning**: Columns with more than 20% missing values are dropped to improve data quality.
- **Unit Conversion**: Measurements are converted from imperial to SI units for standardization.
  - Temperature (Fahrenheit) -> Celsius: $T_C = (T_F - 32) \times \frac{5}{9}$
  - Visibility (Miles) -> Kilometers
  - Wind Speed (Knots) -> Meters/Second
  - Precipitation (Inches) -> Millimeters
- **Feature Engineering (`FRSHTT`)**: The 6-digit `FRSHTT` code (Fog, Rain, Snow, Hail, Thunder, Tornado) is decoded from a bitmask into six separate binary indicator columns, making it easier to analyze individual weather events.
- **Final Cleanup**: Rows with `NA` values in major meteorological columns are removed. The final dataset is saved as `cleaned_data.csv`.

---

## Exploratory Data Analysis & Visualizations

A series of plots were generated to explore the cleaned data, revealing patterns in temperature and weather events.

- **Faceted Density Plot**: Visualizes the distribution of temperature during different weather events (Fog, Rain, Snow, etc.). This helps show, for example, that snow and hail are associated with lower temperatures.
- **Spatial Temperature Distribution**: A map of India with points colored by temperature provides a clear visual of regional temperature variations.
- **Time Series Analysis**: Plots of temperature and precipitation over time for individual stations reveal annual cycles, long-term trends, and extreme events.
- **Correlation Heatmaps**: A heatmap of the correlation matrix between numeric variables like temperature, dew point, and wind speed helps identify relationships between meteorological variables.

---

## Interactive Analysis Applications (Shiny Apps)

Several interactive web applications were developed using R Shiny to allow for dynamic exploration and analysis of the weather data.

### 1. Weather Station Analysis

An interactive Leaflet map allows users to select any weather station from a dropdown menu. The map centers on the selected station, and a marker displays key details. Below the map, a table shows the average yearly temperature and the most frequently occurring weather event (`FRSHTT`) for that station.

### 2. Seasonal Weather Clustering

This application groups weather stations into six distinct clusters based on their seasonal weather patterns. Using **k-means clustering** on average temperature, precipitation, and elevation, it generates separate maps for each season (Winter, Spring, Summer, Monsoon, Fall). Users can switch between seasonal tabs to see how station groupings change throughout the year.

### 3. Extreme Weather Event Analysis

This tool identifies and visualizes extreme weather events. An "extreme event" is defined as a measurement that deviates from the daily mean for that station by a user-selected number of standard deviations (3, 4, or 5). The user can select a station, an attribute (e.g., Temperature, Precipitation), and an extremity level to generate a bar chart showing the frequency of extreme events for each day of the year.

### 4. Temperature Forecasting (ARIMA/SARIMA)

Two applications provide time series forecasting for temperature. Users can select a station and the number of days to forecast.

- **SARIMA Model**: Fits a Seasonal ARIMA model `ARIMA(1,0,1)(1,0,1)[12]` to the temperature data to generate and plot a forecast.
- **ARIMA Model**: Fits a non-seasonal `ARIMA(1,0,1)` model for comparison.

### 5. Temperature Prediction (Random Forest)

This application predicts the temperature for a specific station on a future date using a **Random Forest** model. The model is trained on historical data using features like year, month, day, and day of the year. The app displays the predicted temperature alongside the actual temperature if available and reports model accuracy metrics (MAE, MSE, R-squared).

---

## Setup and Usage

### Prerequisites

- R and RStudio
- Python (for the initial data download)
- Required Python library: `requests`, `tqdm`

### R Libraries

To run all the scripts and applications, install the following R packages:

```r
install.packages(c(
  "shiny", "leaflet", "dplyr", "lubridate", "ggplot2", "viridis",
  "tidyr", "cluster", "shinydashboard", "htmlwidgets", "astsa",
  "forecast", "randomForest", "caret", "data.table", "xts", "tsbox"
))
```
