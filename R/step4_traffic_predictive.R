################################################################################
# Step 4: Predictive Analysis
# DB
################################################################################

################################################################################
# Setup
################################################################################

# Set working directory
setwd(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))

# Libraries
library(tidyverse)
library(fastDummies)

# Source helper functions
source(file.path("R", "helper_functions.R"))

# Load Data
traffic_data <- data.table::fread("data/full_traffic_data.csv") %>% 
  mutate(accident_date = as.Date(accident_date))
daily_traffic <- data.table::fread("data/daily_traffic_data.csv")
monthly_traffic <- data.table::fread("data/monthly_traffic_data.csv")
annual_traffic <- data.table::fread("data/annual_traffic_data.csv")
spatial_objects <- readRDS("data/mapping/spatial_objects.rds")
water_sf <- sf::read_sf("data/mapping/water.shp")
holidays_df <- data.table::fread("data/holidays.csv") %>% 
  mutate(date = as.Date(date),
         accident_date = as.Date(accident_date))

# Create rain dataset
rain_df <- traffic_data %>% 
  dplyr::select(accident_date, weather_condition_rain) %>% 
  group_by(accident_date) %>% 
  summarize(weather_condition_rain = max(weather_condition_rain, na.rm=TRUE)) %>% 
  mutate(rain_group = cumsum(weather_condition_rain == 1)) %>%
  group_by(rain_group) %>%
  mutate(since_rain = row_number() - 1) %>%
  ungroup() %>%
  dplyr::select(accident_date, since_rain) %>% 
  mutate(accident_date = as.Date(accident_date))

traffic_data <- traffic_data %>% 
  left_join(., rain_df, by='accident_date') %>% 
  filter(accident_year %in% 2017:2022)

hourly_medians <- traffic_data %>% 
  group_by(accident_year, accident_month, accident_hour) %>% 
  summarise(median_temp = median(temperature_f, na.rm=TRUE),
            median_humidity = median(humidity_percent, na.rm=TRUE),
            median_pressure = median(pressure_in, na.rm=TRUE),
            median_visibility = median(visibility_mi, na.rm=TRUE),
            median_wind_speed = median(wind_speed_mph, na.rm=TRUE),
            median_precipitation = median(precipitation_in, na.rm=TRUE))

daily_medians <- traffic_data %>% 
  group_by(accident_year, accident_month, accident_date) %>% 
  summarise(median_temp = median(temperature_f, na.rm=TRUE),
            median_humidity = median(humidity_percent, na.rm=TRUE),
            median_pressure = median(pressure_in, na.rm=TRUE),
            median_visibility = median(visibility_mi, na.rm=TRUE),
            median_wind_speed = median(wind_speed_mph, na.rm=TRUE),
            median_precipitation = median(precipitation_in, na.rm=TRUE))

monthly_medians <- traffic_data %>% 
  group_by(accident_year, accident_month) %>% 
  summarise(median_temp = median(temperature_f, na.rm=TRUE),
            median_humidity = median(humidity_percent, na.rm=TRUE),
            median_pressure = median(pressure_in, na.rm=TRUE),
            median_visibility = median(visibility_mi, na.rm=TRUE),
            median_wind_speed = median(wind_speed_mph, na.rm=TRUE),
            median_precipitation = median(precipitation_in, na.rm=TRUE))

# Select modeling variables
hourly_data <- traffic_data %>% 
  group_by(accident_date, accident_year, accident_month,  accident_hour, accident_dow, holiday) %>% 
  summarise(n_accidents = n(),
            temp = mean(temperature_f, na.rm=TRUE),
            humidity = mean(humidity_percent, na.rm=TRUE),
            pressure = mean(pressure_in, na.rm=TRUE),
            visibility = mean(visibility_mi, na.rm=TRUE),
            wind_speed = mean(wind_speed_mph, na.rm=TRUE),
            precipitation = mean(precipitation_in, na.rm=TRUE),
            weather_condition_cloudy = max(weather_condition_cloudy, na.rm=TRUE),
            weather_condition_fog_mist = max(weather_condition_fog_mist, na.rm=TRUE),
            weather_condition_freezing_conditions = max(weather_condition_freezing_conditions, na.rm=TRUE),
            weather_condition_haze_smoke = max(weather_condition_haze_smoke, na.rm=TRUE),
            weather_condition_other = max(weather_condition_other, na.rm=TRUE),
            weather_condition_rain = max(weather_condition_rain, na.rm=TRUE),
            weather_condition_snow = max(weather_condition_snow, na.rm=TRUE),
            weather_condition_thunderstorm = max(weather_condition_thunderstorm, na.rm=TRUE),
            weather_condition_unknown = max(weather_condition_unknown, na.rm=TRUE),
            weather_condition_wintry_mix_sleet = max(weather_condition_wintry_mix_sleet, na.rm=TRUE)) %>% 
  ungroup(.)

# Select modeling variables
daily_data <- traffic_data %>% 
  group_by(accident_date, accident_year, accident_month, accident_dow, holiday) %>% 
  summarise(n_accidents = n(),
            temp = mean(temperature_f, na.rm=TRUE),
            humidity = mean(humidity_percent, na.rm=TRUE),
            pressure = mean(pressure_in, na.rm=TRUE),
            visibility = mean(visibility_mi, na.rm=TRUE),
            wind_speed = mean(wind_speed_mph, na.rm=TRUE),
            precipitation = mean(precipitation_in, na.rm=TRUE)) %>% 
  ungroup(.)

na_counts <- hourly_data %>%
  ungroup(.) %>% 
  summarise(across(everything(), ~ sum(is.na(.))))


################################################################################
# Create time schedule
################################################################################

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2022-12-31")

# Generate a sequence of all dates
all_dates <- seq(start_date, end_date, by = "day")

# Create a data frame with all combinations of date, day of the week, and hour
time_schedule <- expand.grid(
  accident_date = all_dates,
  accident_hour = 0:23
)

# Add year, month, and day of the week columns
time_schedule <- time_schedule %>%
  mutate(
    accident_year = lubridate::year(accident_date),
    accident_month = lubridate::month(accident_date),
    accident_dow = lubridate::wday(accident_date, label = TRUE, abbr = TRUE)
  ) %>% 
  # Add holidays
  left_join(., holidays_df, by='accident_date') %>% 
  mutate(holiday = ifelse(is.na(holiday), 'None', holiday)) %>% 
  dplyr::select(-date)


# Combine
test <- time_schedule %>% 
  left_join(., hourly_data, by=c('accident_date', 'accident_hour',
                                 'accident_year', 'accident_month',
                                 'accident_dow', 'holiday')) %>% 
  left_join(., hourly_medians, by=c('accident_year', 'accident_month', 'accident_hour')) %>% 
  mutate(temp = ifelse(is.na(temp), median_temp, temp),
         humidity = ifelse(is.na(humidity), median_humidity, humidity),
         pressure = ifelse(is.na(pressure), median_pressure, pressure),
         visibility = ifelse(is.na(visibility), median_visibility, visibility),
         wind_speed = ifelse(is.na(wind_speed), median_wind_speed, wind_speed),
         precipitation = ifelse(is.na(precipitation), median_precipitation, precipitation)) %>% 
  dplyr::select(-c(median_temp, median_humidity, median_pressure,
                   median_visibility, median_visibility, median_wind_speed,
                   median_precipitation)) %>% 
  left_join(., daily_medians, by=c('accident_year', 'accident_month', 'accident_date')) %>% 
  mutate(temp = ifelse(is.na(temp), median_temp, temp),
         humidity = ifelse(is.na(humidity), median_humidity, humidity),
         pressure = ifelse(is.na(pressure), median_pressure, pressure),
         visibility = ifelse(is.na(visibility), median_visibility, visibility),
         wind_speed = ifelse(is.na(wind_speed), median_wind_speed, wind_speed),
         precipitation = ifelse(is.na(precipitation), median_precipitation, precipitation)) %>% 
  dplyr::select(-c(median_temp, median_humidity, median_pressure,
                   median_visibility, median_visibility, median_wind_speed,
                   median_precipitation)) %>% 
  left_join(., monthly_medians, by=c('accident_year', 'accident_month')) %>% 
  mutate(temp = ifelse(is.na(temp), median_temp, temp),
         humidity = ifelse(is.na(humidity), median_humidity, humidity),
         pressure = ifelse(is.na(pressure), median_pressure, pressure),
         visibility = ifelse(is.na(visibility), median_visibility, visibility),
         wind_speed = ifelse(is.na(wind_speed), median_wind_speed, wind_speed),
         precipitation = ifelse(is.na(precipitation), median_precipitation, precipitation)) %>% 
  dplyr::select(-c(median_temp, median_humidity, median_pressure,
                   median_visibility, median_visibility, median_wind_speed,
                   median_precipitation))



################################################################################
# Create Model
################################################################################

modeling_data <- hourly_data %>% 
  drop_na(.) %>% 
  filter(accident_year %in% 2017:2021)

validation_data <- hourly_data %>% 
  drop_na(.) %>% 
  filter(accident_year %in% 2022)

# Calculate the mean and variance of the count data
mean_n_accidents <- mean(modeling_data$n_accidents)
var_n_accidents <- var(modeling_data$n_accidents)

# Check for overdispersion
print(paste("Mean of n_accidents: ", mean_n_accidents))
print(paste("Variance of n_accidents: ", var_n_accidents))

# Define the control function for cross-validation
train_control <- caret::trainControl(method = "cv", number = 5, savePredictions = TRUE)

# Create folds
folds <- caret::createFolds(modeling_data$n_accidents, k = 5, list = TRUE, returnTrain = TRUE)

# Initialize a data frame to store results
cv_results <- data.frame(RMSE = numeric(), MAE = numeric())

# Perform cross-validation
for (i in 1:length(folds)) {
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(modeling_data)), train_indices)
  
  # Fit and evaluate the model
  fold_result <- fit_and_evaluate(train_indices, test_indices, modeling_data)
  cv_results <- rbind(cv_results, fold_result)
}

# Print cross-validation results
print(cv_results)

# Calculate summary statistics for cross-validation metrics
cv_summary_stats <- cv_results %>%
  summarise(
    mean_RMSE = mean(RMSE),
    sd_RMSE = sd(RMSE),
    mean_MAE = mean(MAE),
    sd_MAE = sd(MAE)
  )

# Display summary statistics for cross-validation metrics
print(cv_summary_stats)

# Plot RMSE by fold
ggplot(cv_results, aes(x = factor(seq_len(nrow(cv_results))), y = RMSE)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "RMSE by Cross-Validation Fold",
       x = "Fold",
       y = "RMSE") +
  theme_minimal()

# Plot MAE by fold
ggplot(cv_results, aes(x = factor(seq_len(nrow(cv_results))), y = MAE)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "MAE by Cross-Validation Fold",
       x = "Fold",
       y = "MAE") +
  theme_minimal()

train_control <- caret::trainControl(method = "cv", number = 5, savePredictions = "final")

# Assuming `training_data` is your training dataset and `validation_data` is your validation dataset
# Perform cross-validation
cv_results <- caret::train(
  n_accidents ~ accident_month + accident_hour + accident_dow + holiday + 
    temp + humidity + pressure + visibility + wind_speed + precipitation + 
    weather_condition_cloudy + weather_condition_fog_mist + 
    weather_condition_freezing_conditions + weather_condition_haze_smoke + 
    weather_condition_other + weather_condition_rain + 
    weather_condition_snow + weather_condition_thunderstorm + 
    weather_condition_unknown + weather_condition_wintry_mix_sleet,
  data = modeling_data,
  method = "glm.nb",
  trControl = train_control
)

# Print cross-validation results
print(cv_results)

# Make predictions on the validation set using the cross-validated model
validation_data$predicted <- predict(cv_results, newdata = validation_data)

# View the predictions
head(validation_data)

blah <- validation_data %>% 
  dplyr::select(accident_date, accident_hour, n_accidents, predicted)

# Calculate performance metrics on the validation set
rmse <- sqrt(mean((validation_data$n_accidents - validation_data$predicted)^2))
mae <- mean(abs(validation_data$n_accidents - validation_data$predicted))

# Print performance metrics
print(paste("RMSE on validation set: ", rmse))
print(paste("MAE on validation set: ", mae))


################################################################################
# Create Model
################################################################################

modeling_data <- daily_data %>% 
  drop_na(.) %>% 
  filter(accident_year %in% 2017:2021) %>% 
  mutate(n_accidents = log(n_accidents))

validation_data <- daily_data %>% 
  drop_na(.) %>% 
  filter(accident_year %in% 2022) %>% 
  mutate(n_accidents = log(n_accidents))

hist(modeling_data$n_accidents)
hist(validation_data$n_accidents)

# Calculate the mean and variance of the count data
mean_n_accidents <- mean(modeling_data$n_accidents)
var_n_accidents <- var(modeling_data$n_accidents)

# Check for overdispersion
print(paste("Mean of n_accidents: ", mean_n_accidents))
print(paste("Variance of n_accidents: ", var_n_accidents))

train_control <- caret::trainControl(method = "cv", number = 5, savePredictions = "final")

# Assuming `training_data` is your training dataset and `validation_data` is your validation dataset
# Perform cross-validation
cv_results <- caret::train(
  n_accidents ~ accident_month + accident_dow + holiday + 
    temp + humidity + pressure + visibility + wind_speed + precipitation,
  data = modeling_data,
  method = "glm.nb",
  trControl = train_control
)

# Print cross-validation results
print(cv_results)

# Make predictions on the validation set using the cross-validated model
validation_data$predicted <- predict(cv_results, newdata = validation_data)

# View the predictions
head(validation_data)

blah <- validation_data %>% 
  dplyr::select(accident_date, n_accidents, predicted) %>% 
  mutate(n_accidents = exp(n_accidents),
         predicted = exp(predicted))

# Calculate performance metrics on the validation set
rmse <- sqrt(mean((blah$n_accidents - blah$predicted)^2))
mae <- mean(abs(blah$n_accidents - blah$predicted))

# Print performance metrics
print(paste("RMSE on validation set: ", rmse))
print(paste("MAE on validation set: ", mae))

caret::R2(blah$predicted, blah$n_accidents)

