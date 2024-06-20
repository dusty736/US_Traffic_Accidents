################################################################################
# Step 1: Descriptive Analysis
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
  left_join(., rain_df, by='accident_date')

################################################################################
# Data Aggregation
################################################################################

hourly_counts <- traffic_data %>% 
  group_by(accident_hour) %>% 
  summarise(n_accidents = n(),
            accidents_per_hour = n() / 24,
            mean_days_since_rain = mean(since_rain, na.rm=TRUE),
            hourly_severity_average = mean(severity, na.rm=TRUE),
            hourly_duration_average = mean(duration_of_accident, na.rm=TRUE),
            hourly_distance_mi_average = mean(distance_mi, na.rm=TRUE)) %>% 
  mutate(rush_hour = ifelse(accident_hour %in% c(6:9, 16:18), 1, 0)) %>% 
  mutate_if(is.numeric, round, 2)

daily_counts <- traffic_data %>% 
  group_by(accident_date, accident_dow, holiday, since_rain) %>% 
  summarise(n_accidents = n(),
            accidents_per_hour = n() / 24,
            n_rush_hour_accidents = sum(accident_hour %in% c(6:9, 16:18), na.rm=TRUE),
            daily_avg_accident_hour = mean(as.numeric(accident_hour_dec, na.rm=TRUE)),
            daily_severity_average = mean(severity, na.rm=TRUE),
            daily_distance_mi_average = mean(distance_mi, na.rm=TRUE),
            ) %>% 
  mutate_if(is.numeric, round, 2)

monthly_counts <- traffic_data %>% 
  mutate(year_month = format(accident_date, "%Y-%m")) %>% 
  mutate(year_month = as.Date(paste0(year_month, "-01"))) %>% 
  group_by(year_month, accident_year, accident_month) %>% 
  summarise(n_accidents = n(),
            days_since_rain_avg = mean(since_rain, na.rm=TRUE),
            n_rush_hour_accidents = sum(accident_hour %in% c(6:9, 16:18), na.rm=TRUE),
            monthly_avg_accident_hour = mean(as.numeric(accident_hour_dec, na.rm=TRUE)),
            monthly_severity_average = mean(severity, na.rm=TRUE),
            monthly_distance_mi_average = mean(distance_mi, na.rm=TRUE),
            ) %>% 
  mutate(days_in_month = lubridate::days_in_month(year_month)) %>%
  mutate(accidents_per_hour = n_accidents / (24 * days_in_month),
         accidents_per_day = n_accidents / (days_in_month)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  dplyr::select(-days_in_month) %>% 
  dplyr::select(year_month, accident_year, accident_month, n_accidents, accidents_per_hour,
                accidents_per_day, everything())

annual_counts <- traffic_data %>% 
  group_by(accident_year) %>% 
  summarise(n_accidents = n(),
            accidents_per_hour = n() / 24,
            days_since_rain_avg = mean(since_rain, na.rm=TRUE),
            n_rush_hour_accidents = sum(accident_hour %in% c(6:9, 16:18), na.rm=TRUE),
            annual_avg_accident_hour = mean(as.numeric(accident_hour_dec, na.rm=TRUE)),
            annual_severity_average = mean(severity, na.rm=TRUE),
            annual_distance_mi_average = mean(distance_mi, na.rm=TRUE),
  ) %>% 
  mutate_if(is.numeric, round, 2)

annual_counts <- traffic_data %>% 
  group_by(accident_year) %>% 
  summarise(n_accidents = n(),
            accidents_per_month = n() / 12,
            days_since_rain_avg = mean(since_rain, na.rm=TRUE),
            n_rush_hour_accidents = sum(accident_hour %in% c(6:9, 16:18), na.rm=TRUE),
            annual_avg_accident_hour = mean(as.numeric(accident_hour_dec, na.rm=TRUE)),
            annual_severity_average = mean(severity, na.rm=TRUE),
            annual_distance_mi_average = mean(distance_mi, na.rm=TRUE),
  ) %>% 
  mutate(accidents_per_day = n_accidents / ifelse(accident_year == 2020, 366, 365),
         accidents_per_hour = n_accidents / (ifelse(accident_year == 2020, 366, 365) * 24)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  dplyr::select(accident_year, n_accidents, accidents_per_hour, accidents_per_day,
                accidents_per_month, everything())

################################################################################
# Question 1
# How many traffic accidents happen per year
################################################################################

print(annual_counts)

# accident_year n_accidents accidents_per_hour
#1          2016        9315              388. 
#2          2017       18497              771. 
#3          2018       18984              791  
#4          2019       14718              613. 
#5          2020       13211              550. 
#6          2021       16093              671. 
#7          2022       15416              642. 
#8          2023        1987              82.8

annuaL_total_counts_plot <- annual_counts %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
    geom_point(aes(x = accident_year, y = n_accidents)) +
    geom_line(aes(x = accident_year, y = n_accidents)) +
    lims(y = c(0, 20000)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
    labs(x = "Year",
         y = "Accident Count",
         title = "Washington State Annual Accident Count")

annual_hourly_rate_plot <- annual_counts %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = accident_year, y = accidents_per_hour)) +
  geom_line(aes(x = accident_year, y = accidents_per_hour)) +
  lims(y = c(0, 2.5)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year",
       y = "Accident Count per Hour",
       title = "Washington State Annual Accidents per Hour")

annual_daily_rate_plot <- annual_counts %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = accident_year, y = accidents_per_day)) +
  geom_line(aes(x = accident_year, y = accidents_per_day)) +
  lims(y = c(0, 60)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year",
       y = "Accident Count per Day",
       title = "Washington State Annual Accidents per Day")

annual_monthly_rate_plot <- annual_counts %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = accident_year, y = accidents_per_month)) +
  geom_line(aes(x = accident_year, y = accidents_per_month)) +
  lims(y = c(0, 2000)) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year",
       y = "Accident Count per Month",
       title = "Washington State Annual Accidents per Month")

annual_plots <- cowplot::plot_grid(annuaL_total_counts_plot, 
                                   annual_hourly_rate_plot, 
                                   annual_daily_rate_plot, 
                                   annual_monthly_rate_plot, 
                                   labels = "AUTO")

ggsave("plots/annual_accidents.png", plot = annual_plots, width = 10, height = 10, units = "in", dpi = 300)

################################################################################
# Question 2
# How many traffic accidents happen per month
################################################################################

monthly_total_counts_plot <- monthly_counts %>% 
  ungroup(.) %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = year_month, y = n_accidents)) +
  geom_line(aes(x = year_month, y = n_accidents, group = 1)) +
  lims(y = c(0, 3000)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year-Month",
       y = "Accident Count",
       title = "Washington State Monthly Accident Count")

monthly_hourly_rate_plot <- monthly_counts %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = year_month, y = accidents_per_hour)) +
  geom_line(aes(x = year_month, y = accidents_per_hour, group = 1)) +
  lims(y = c(0, 4)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year-Month",
       y = "Accident Count per Hour",
       title = "Washington State monthly Accidents per Hour")

monthly_daily_rate_plot <- monthly_counts %>% 
  filter(accident_year %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = year_month, y = accidents_per_day)) +
  geom_line(aes(x = year_month, y = accidents_per_day, , group = 1)) +
  lims(y = c(0, 100)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year-Month",
       y = "Accident Count per Day",
       title = "Washington State Monthly Accidents per Day")

monthly_plots <- cowplot::plot_grid(monthly_total_counts_plot, 
                                   monthly_hourly_rate_plot, 
                                   monthly_daily_rate_plot,
                                   labels = "AUTO")

ggsave("plots/monthly_accidents.png", plot = monthly_plots, width = 10, height = 10, units = "in", dpi = 300)


################################################################################
# Question 3
# How many traffic accidents happen per day
################################################################################

daily_total_counts_plot <- daily_counts %>% 
  ungroup(.) %>% 
  filter(lubridate::year(accident_date) %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = accident_date, y = n_accidents), shape='o', size=0.5) +
  geom_smooth(aes(x = accident_date, y = n_accidents), method = "loess", span = 0.05) +
  #lims(y = c(0, )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Date",
       y = "Accident Count",
       title = "Washington State daily Accident Count")

daily_hourly_rate_plot <- daily_counts %>% 
  filter(lubridate::year(accident_date) %in% 2017:2022) %>% 
  ggplot() +
  geom_point(aes(x = accident_date, y = accidents_per_hour), shape='o', size=0.5) +
  geom_smooth(aes(x = accident_date, y = accidents_per_hour), method = "loess", span = 0.05) +
  lims(y = c(0, 6)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Date",
       y = "Accident Count per Hour",
       title = "Washington State daily Accidents per Hour")

daily_plots <- cowplot::plot_grid(daily_total_counts_plot, 
                                  daily_hourly_rate_plot, 
                                  labels = "AUTO")

ggsave("plots/daily_accidents.png", plot = daily_plots, width = 10, height = 10, units = "in", dpi = 300)







