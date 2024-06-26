################################################################################
# Step 2: Exploratory Analysis
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

# Create rain dataset
rain_df <- traffic_data %>% 
  dplyr::select(accident_date, weather_condition_rain) %>% 
  group_by(accident_date) %>% 
  summarize(weather_condition_rain = max(weather_condition_rain, na.rm=TRUE)) %>% 
  mutate(rain_group = cumsum(weather_condition_rain == 1)) %>%
  group_by(rain_group) %>%
  mutate(since_rain = row_number() - 1) %>%
  ungroup() %>%
  select(accident_date, since_rain) %>% 
  mutate(accident_date = as.Date(accident_date))

traffic_data <- traffic_data %>% 
  left_join(., rain_df, by='accident_date') %>% 
  filter(accident_year %in% 2017:2022)

################################################################################
# Question 1: Is there a correlation between temperatures and accident counts?
################################################################################

temp_summary <- traffic_data %>% 
  filter(!is.na(temperature_f)) %>% 
  mutate(temperature_f = round(temperature_f, 0)) %>% 
  group_by(accident_month, temperature_f) %>% 
  summarise(n_accidents = n())

correlation <- cor(temp_summary$temperature_f, temp_summary$n_accidents, method = "pearson")

# Scatter plot of number of accidents vs. temperature
temp_summary %>% 
  ggplot() +
    geom_point(aes(x = temperature_f, y = n_accidents)) +
    geom_smooth(aes(x = temperature_f, y = n_accidents), 
                method = "lm", col = "blue") +
    labs(title = "Number of Accidents vs. Temperature",
         x = "Temperature",
         y = "Number of Accidents") +
    theme_minimal() +
  facet_grid(~accident_month)

# Fit GAMs for each month
gam_models <- temp_summary %>%
  filter(!is.na(temperature_f)) %>% 
  mutate(temperature_f = round(temperature_f, 0)) %>% 
  dplyr::group_by(accident_month) %>%
  dplyr::do(model = mgcv::gam(n_accidents ~ s(temperature_f), data = .))

# Extract summaries of the models
gam_summaries <- lapply(gam_models$model, summary)

# Extract important information (e.g., R-squared, deviance explained)
gam_results <- lapply(gam_summaries, function(x) {
  data.frame(
    r_squared = x$r.sq,
    deviance_explained = x$dev.expl,
    stringsAsFactors = FALSE
  )
})

# Combine results into a single data frame
gam_results_df <- do.call(rbind, gam_results)
gam_results_df$month <- unique(temp_summary$accident_month)

# Ensure predictions are only made for existing temperature values
gam_prediction_data <- temp_summary %>%
  dplyr::select(accident_month, temperature_f) %>%
  dplyr::distinct()

# Function to generate predictions for a given model and data
generate_predictions <- function(model, data) {
  data$predicted_num_accidents <- predict(model, newdata = data)
  return(data)
}

# Add predictions to the data frame
gam_predictions <- gam_models %>%
  dplyr::rowwise() %>%
  dplyr::mutate(predictions = list(generate_predictions(model, gam_prediction_data %>% 
                                                          dplyr::filter(accident_month == unique(accident_month))))) %>%
  dplyr::select(-model) %>%
  tidyr::unnest(predictions, names_sep = "_")

# Plot GAM fits
ggplot2::ggplot(temp_summary, aes(x = temperature_f, y = n_accidents)) +
  ggplot2::geom_point() +
  ggplot2::geom_line(data = gam_predictions, aes(x = predictions_temperature_f, y = predictions_predicted_num_accidents), col = "blue") +
  ggplot2::labs(title = "GAM: Number of Accidents vs. Temperature",
                x = "Temperature",
                y = "Number of Accidents") +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~ accident_month)

# There is a predictable relation between temperature and accident counts.  I believe this is
# likely related to time of day.

################################################################################
# Question 2: Is there a correlation between weather conditions and accident counts?
################################################################################

weather_data <- traffic_data %>% 
  dplyr::select(id, 
                weather_condition_cloudy, 
                weather_condition_fog_mist,
                weather_condition_freezing_conditions,
                weather_condition_haze_smoke,
                weather_condition_other,
                weather_condition_rain,
                weather_condition_snow, 
                weather_condition_thunderstorm,
                weather_condition_unknown,
                weather_condition_wintry_mix_sleet) %>% 
  mutate(weather_condition_sunny = ifelse(rowSums(dplyr::select(., starts_with("weather_condition_"))) == 0, 1, 0)) %>%
  # Drop rows with any NA values
  drop_na()

# Fit a logistic regression model
logistic_model <- glm(
  formula = 1 ~ weather_condition_sunny + weather_condition_cloudy + weather_condition_fog_mist + 
    weather_condition_freezing_conditions + weather_condition_haze_smoke + 
    weather_condition_other + weather_condition_rain + 
    weather_condition_snow + weather_condition_thunderstorm + 
    weather_condition_unknown + weather_condition_wintry_mix_sleet,
  data = weather_data,
  family = binomial(link = "logit")
)

# Summarize the model
summary(poisson_model)


################################################################################
# Question 3: Is there a correlation between precipitation and accident counts?
################################################################################



################################################################################
# Question 4: Is there a correlation between visibility and accident counts?
################################################################################



################################################################################
# Question 5: Are there any temporal trends in the accident counts over time?
################################################################################


################################################################################
# Question 6: Is there a correlation between hour of the day and accident?
################################################################################


################################################################################
# Question 7: Is there a correlation between day of week and accident?
################################################################################




