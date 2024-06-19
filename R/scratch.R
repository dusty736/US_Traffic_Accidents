################################################################################
# Data Exploration
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
traffic_data <- data.table::fread("data/WA_Accidents_March23.csv")

# Clean column names
traffic_data <- janitor::clean_names(traffic_data)

# Organize Columns of Interest
columns_of_interest <- c('id', 'severity', 'start_time', 
                         'end_time', 'start_lat', 'start_lng', 
                         'distance_mi', 'city', 'county', 'zipcode', 
                         'temperature_f', 'humidity_percent', 
                         'pressure_in', 'visibility_mi', 'wind_speed_mph', 
                         'precipitation_in', 'weather_condition', 
                         'sunrise_sunset', 'civil_twilight', 
                         'nautical_twilight', 'astronomical_twilight')

traffic_data <- traffic_data %>% 
  dplyr::select(all_of(columns_of_interest))

################################################################################
# Create Map Objects
################################################################################

# States
if (!file.exists(file.path("data", "mapping", "states.shp"))) {
  states_of_interest <- tigris::states() %>% 
    filter(STUSPS %in% c('OR', 'ID', 'WA'))
  saveRDS(states_of_interest, file.path("data","mapping", "states.shp"))
} else {
  states_of_interest <- readRDS(file.path("data","mapping", "states.shp"))
}

# Counties
if (!file.exists(file.path("data","mapping", "counties.shp"))) {
  counties_of_interest <- rbind(tigris::counties('OR') %>% 
                                  mutate(state = 'OR'),
                                tigris::counties('ID') %>% 
                                  mutate(state = 'ID'),
                                tigris::counties('WA') %>% 
                                  mutate(state = 'WA'))
  saveRDS(counties_of_interest, file.path("data","mapping", "counties.shp"))
} else {
  counties_of_interest <- readRDS(file.path("data","mapping", "counties.shp"))
}

# Roads
roads <- rbind(tigris::primary_secondary_roads('OR'),
               tigris::primary_secondary_roads('ID'),
               tigris::primary_secondary_roads('WA'))

roads_s1100 <- roads %>% 
  filter(MTFCC == 'S1100')
roads_s1200 <- roads %>% 
  filter(MTFCC == 'S1200')

# BBOX
bbox <- sf::st_bbox(states_of_interest %>% filter(STUSPS %in% c('WA')))
plotting_bbox <- bbox
plotting_bbox[1] <- plotting_bbox[[1]] - 0.05
plotting_bbox[2] <- plotting_bbox[[2]] - 0.05
plotting_bbox[3] <- plotting_bbox[[3]] + 0.05
plotting_bbox[4] <- plotting_bbox[[4]] + 0.05

# Define the bounding box for the specified area
bbox_seattle <- c(xmin = -122.555, ymin = 47.395, xmax = -122.095, ymax = 48.005)

# Create a POLYGON geometry from the bounding box
bbox_coords <- matrix(c(
  bbox_seattle["xmin"], bbox_seattle["ymin"],  # xmin, ymin
  bbox_seattle["xmin"], bbox_seattle["ymax"],  # xmin, ymax
  bbox_seattle["xmax"], bbox_seattle["ymax"],  # xmax, ymax
  bbox_seattle["xmax"], bbox_seattle["ymin"],  # xmax, ymin
  bbox_seattle["xmin"], bbox_seattle["ymin"]   # close the polygon by repeating the first point
), ncol = 2, byrow = TRUE)

bbox_polygon <- sf::st_polygon(list(bbox_coords))
bbox_sf <- sf::st_sfc(bbox_polygon, crs = 4326)

# King County
king_county <- counties_of_interest %>% 
  filter(state == 'WA' & NAME == 'King')

seattle_sf <- sf::st_intersection(sf::st_make_valid(sf::st_transform(king_county, 4326)), 
                                  sf::st_make_valid(bbox_sf))

seattle_bbox <- sf::st_bbox(seattle_sf)

king_county_bbox <- sf::st_bbox(king_county)
king_county_bbox[1] <- king_county_bbox[[1]] - 0.005
king_county_bbox[2] <- king_county_bbox[[2]] - 0.005
king_county_bbox[3] <- king_county_bbox[[3]] + 0.005
king_county_bbox[4] <- king_county_bbox[[4]] + 0.005

# Create a buffer around King County
buffered_king_county <- sf::st_buffer(king_county, dist = 0.01)

# Find counties that intersect with the buffered King County
intersecting_indices <- sf::st_intersects(counties_of_interest, buffered_king_county)

# Convert list of indices to logical vector
intersecting_logical <- lengths(intersecting_indices) > 0

# Extract the intersecting counties
king_adjacent_counties <- counties_of_interest[intersecting_logical, ]

water_counties <- sf::st_union(rbind(tigris::area_water("WA", "Kittitas"),
                                     tigris::area_water("WA", "Snohomish"),
                                     tigris::area_water("WA", "Yakima"),
                                     tigris::area_water("WA", "Chelan"),
                                     tigris::area_water("WA", "King"),
                                     tigris::area_water("WA", "Kitsap"),
                                     tigris::area_water("WA", "Pierce")))

################################################################################
# Feature Engineering
################################################################################

traffic_data <- traffic_data %>% 
  # New or updated columns
  mutate(accident_hour = paste0(lubridate::hour(start_time) + round(lubridate::minute(start_time) / 60, 2)),
         accident_month = lubridate::month(start_time),
         duration_of_accident = round(as.numeric(end_time - start_time), 0),
         accident_date = as.Date(start_time),
         accident_time = format(start_time, "%H:%M:%S"),
         zipcode = substr(zipcode, 1, 5),
         precipitation_in = ifelse(is.na(precipitation_in), 0, precipitation_in)) %>% 
  # Clean up names
  rename(longitude = start_lng,
         latitude = start_lat) %>% 
  # Organize
  dplyr::select(id, severity, longitude, latitude, accident_date, accident_month, 
                accident_hour, accident_time, duration_of_accident,
                distance_mi, zipcode, city, county, temperature_f,
                humidity_percent, pressure_in, visibility_mi, wind_speed_mph,
                precipitation_in, weather_condition, sunrise_sunset, 
                civil_twilight, nautical_twilight, astronomical_twilight)

# Traffic data categorical
traffic_coded <- traffic_data %>%
  dplyr::select(id, weather_condition, sunrise_sunset, civil_twilight, nautical_twilight, astronomical_twilight) %>%
  mutate(weather_condition = consolidate_weather(weather_condition),
         sunrise_sunset = replace_na(sunrise_sunset, "Unknown"),
         civil_twilight = replace_na(civil_twilight, "Unknown"),
         nautical_twilight = replace_na(nautical_twilight, "Unknown"),
         astronomical_twilight = replace_na(astronomical_twilight, "Unknown")) %>%
  dummy_cols(select_columns = c("weather_condition", "sunrise_sunset", "civil_twilight", "nautical_twilight", "astronomical_twilight"), 
             remove_first_dummy = TRUE, 
             remove_selected_columns = TRUE) %>% 
  janitor::clean_names(.)

# Combine
traffic_data_spatial <- traffic_data %>% 
  sf::st_as_sf(., coords=c('longitude', 'latitude'), crs=4326) %>% 
  dplyr::select(-c(weather_condition, sunrise_sunset, civil_twilight, nautical_twilight, astronomical_twilight)) %>% 
  left_join(., traffic_coded, by='id')

# Define Seattle IDS
seattle_accident_ids <- traffic_data_spatial %>% 
  sf::st_intersection(., seattle_sf) %>% 
  pull(id)

# Define King County IDs
king_accident_ids <- traffic_data_spatial %>% 
  sf::st_intersection(., sf::st_transform(king_county, 4326)) %>% 
  pull(id)

# Add seattle and king county column
traffic_data_spatial <- traffic_data_spatial %>% 
  mutate(in_seattle = ifelse(id %in% seattle_accident_ids, 1, 0),
         in_king_county = ifelse(id %in% king_accident_ids, 1, 0)) %>% 
  dplyr::select(id, in_seattle, in_king_county, everything())

################################################################################
# Investigate Data
################################################################################

skimr::skim(traffic_data)

# Numeric Investigation
numeric_traffic_data <- traffic_data %>% 
  dplyr::select(where(is.numeric))

################################################################################
# Sample Map
################################################################################

sample_accidents <- traffic_data %>% 
  sample_n(., 1000) %>% 
  mutate(accident_hour = as.numeric(accident_hour)) %>% 
  sf::st_as_sf(., coords=c('longitude', 'latitude'), crs=4326)


sample_accidents %>% 
  ggplot() +
  # Add Basemap
  geom_sf(data = states_of_interest, fill = 'white') +
  geom_sf(data = counties_of_interest, fill = 'white') +
  # Add Roads
  geom_sf(data = roads_s1100, color = "black", linewidth=0.2) +
  geom_sf(data = roads_s1200, color = "black", linewidth=0.05) +
  # Add accidents
  geom_sf(aes(color = accident_hour)) +
  scale_color_viridis_c() +
  # Limits
  xlim(plotting_bbox[1], plotting_bbox[3]) +
  ylim(plotting_bbox[2], plotting_bbox[4]) +
  
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

################################################################################
# King County Map
################################################################################

king_county_raster <- create_raster(df = traffic_data_spatial,
                                    region = king_county, 
                                    raster_crs = 32610, 
                                    mapping_crs = 4326, 
                                    resolution = 100, 
                                    date_col = 'accident_date')

annual_king_county_rasters <- create_annual_raster(df = traffic_data_spatial,
                                                   region = king_county, 
                                                   raster_crs = 32610, 
                                                   mapping_crs = 4326, 
                                                   resolution = 100, 
                                                   date_col = 'accident_date')

# Get percent difference
stars_raster_2016 <- annual_king_county_rasters$`2016`
stars_raster_2017 <- annual_king_county_rasters$`2017`
stars_raster_2018 <- annual_king_county_rasters$`2018`
stars_raster_2019 <- annual_king_county_rasters$`2019`

stars_raster_2021 <- annual_king_county_rasters$`2021`
stars_raster_2022 <- annual_king_county_rasters$`2022`
stars_raster_2023 <- annual_king_county_rasters$`2023`

raster_2016 <- as(stars_raster_2016, "Raster")
raster_2017 <- as(stars_raster_2017, "Raster")
raster_2018 <- as(stars_raster_2018, "Raster")
raster_2019 <- as(stars_raster_2019, "Raster")
raster_2021 <- as(stars_raster_2021, "Raster")
raster_2022 <- as(stars_raster_2022, "Raster")

raster_2016[raster_2016[] <= 0] <- NA
raster_2017[raster_2017[] <= 0] <- NA
raster_2018[raster_2018[] <= 0] <- NA
raster_2019[raster_2019[] <= 0] <- NA
raster_2021[raster_2021[] <= 0] <- NA
raster_2022[raster_2022[] <= 0] <- NA

pre_covid_average <- raster::calc(raster::stack(raster_2016, raster_2017, raster_2018, raster_2019),
                                  fun = mean, na.rm=TRUE)
post_covid_average <- raster::calc(raster::stack(raster_2021, raster_2022),
                                   fun = mean, na.rm=TRUE)

pre_covid_average[pre_covid_average[] <= 0] <- NA
post_covid_average[post_covid_average[] <= 0] <- NA

#pre_covid_average[pre_covid_average] <- NA

raster_diff <- post_covid_average - pre_covid_average
na_mask <- is.na(raster_diff[])
raster_diff[raster_diff[] == 0] <- NA

max_diff <- max(raster::values(raster_diff), na.rm = TRUE)
min_diff <- min(raster::values(raster_diff), na.rm = TRUE)
raster_diff_scaled <- (raster_diff - min_diff) / (max_diff - min_diff) * 2 - 1

# Replace infinite values with NA
raster_diff_scaled[na_mask] <- NA

raster_diff_scaled <- raster::mask(raster_diff_scaled, king_county)
raster_diff_scaled_stars <- stars::st_as_stars(raster_diff_scaled)
suppressWarnings(sf::st_crs(raster_diff_scaled_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly

raster_diff_stars <- stars::st_as_stars(raster_diff)
suppressWarnings(sf::st_crs(raster_diff_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly

pre_covid_average_stars <- stars::st_as_stars(pre_covid_average)
suppressWarnings(sf::st_crs(pre_covid_average_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly

post_covid_average_stars <- stars::st_as_stars(post_covid_average)
suppressWarnings(sf::st_crs(post_covid_average_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly


# Plot using ggplot2
#colors <- rev(RColorBrewer::brewer.pal(11, "RdYlGr"))

ggplot() + 
  stars::geom_stars(data = raster_diff_scaled_stars, na.action = na.omit) +
  scale_fill_gradientn(
    colors = c('darkgreen', 'lightgreen', 'white', 'red', 'purple'),
    values = scales::rescale(c(-1, 1)),
    limits = c(-1, 1),
    na.value = "transparent"
  ) +
  ggtitle("Accident Density") +
  # Add Basemap
  #geom_sf(data = states_of_interest, fill = 'white') +
  geom_sf(data = king_adjacent_counties, fill = NA, linewidth=0.5) +
  geom_sf(data = water_counties, fill = 'blue', alpha=0.1, linewidth=0.01) +
  # Add Roads
  geom_sf(data = roads_s1100, color = "black", linewidth=0.2) +
  geom_sf(data = roads_s1200, color = "black", linewidth=0.05) +
  # Limits
  xlim(king_county_bbox[1], king_county_bbox[3]) +
  ylim(king_county_bbox[2], king_county_bbox[4]) +
  labs(x = '',
       y = '',
       fill = 'Scaled Post-Pre Pandmeic Accident Average Difference',
       title = 'King County Traffic Accidents',
       subtitle = '100m') +
  theme_minimal()

################################################################################
# Seattle Map
################################################################################

seattle_raster <- create_raster(df = traffic_data_spatial,
                                region = seattle_sf, 
                                raster_crs = 32610, 
                                mapping_crs = 4326, 
                                resolution = 100, 
                                date_col = 'accident_date')

annual_seattle_rasters <- create_annual_raster(df = traffic_data_spatial,
                                               region = seattle_sf, 
                                               raster_crs = 32610, 
                                               mapping_crs = 4326, 
                                               resolution = 100, 
                                               date_col = 'accident_date')

# Get percent difference
stars_raster_2016 <- annual_seattle_rasters$`2016`
stars_raster_2017 <- annual_seattle_rasters$`2017`
stars_raster_2018 <- annual_seattle_rasters$`2018`
stars_raster_2019 <- annual_seattle_rasters$`2019`

stars_raster_2021 <- annual_seattle_rasters$`2021`
stars_raster_2022 <- annual_seattle_rasters$`2022`
stars_raster_2023 <- annual_seattle_rasters$`2023`

raster_2016 <- as(stars_raster_2016, "Raster")
raster_2017 <- as(stars_raster_2017, "Raster")
raster_2018 <- as(stars_raster_2018, "Raster")
raster_2019 <- as(stars_raster_2019, "Raster")
raster_2021 <- as(stars_raster_2021, "Raster")
raster_2022 <- as(stars_raster_2022, "Raster")

raster_2016[raster_2016[] <= 0] <- NA
raster_2017[raster_2017[] <= 0] <- NA
raster_2018[raster_2018[] <= 0] <- NA
raster_2019[raster_2019[] <= 0] <- NA
raster_2021[raster_2021[] <= 0] <- NA
raster_2022[raster_2022[] <= 0] <- NA

pre_covid_average <- raster::calc(raster::stack(raster_2016, raster_2017, raster_2018, raster_2019),
                                  fun = mean, na.rm=TRUE)
post_covid_average <- raster::calc(raster::stack(raster_2021, raster_2022),
                                   fun = mean, na.rm=TRUE)

pre_covid_average[pre_covid_average[] <= 0] <- NA
post_covid_average[post_covid_average[] <= 0] <- NA

#pre_covid_average[pre_covid_average] <- NA

raster_diff <- post_covid_average - pre_covid_average
na_mask <- is.na(raster_diff[])
raster_diff[raster_diff[] == 0] <- NA

max_diff <- max(raster::values(raster_diff), na.rm = TRUE)
min_diff <- min(raster::values(raster_diff), na.rm = TRUE)
raster_diff_scaled <- (raster_diff - min_diff) / (max_diff - min_diff) * 2 - 1

# Replace infinite values with NA
raster_diff_scaled[na_mask] <- NA

raster_diff_scaled <- raster::mask(raster_diff_scaled, king_county)
raster_diff_scaled_stars <- stars::st_as_stars(raster_diff_scaled)
suppressWarnings(sf::st_crs(raster_diff_scaled_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly

raster_diff_stars <- stars::st_as_stars(raster_diff)
suppressWarnings(sf::st_crs(raster_diff_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly

pre_covid_average_stars <- stars::st_as_stars(pre_covid_average)
suppressWarnings(sf::st_crs(pre_covid_average_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly

post_covid_average_stars <- stars::st_as_stars(post_covid_average)
suppressWarnings(sf::st_crs(post_covid_average_stars) <- sf::st_crs(stars_raster_2016))  # Ensure CRS is set correctly


# Plot using ggplot2
#colors <- rev(RColorBrewer::brewer.pal(11, "RdYlGr"))

ggplot() + 
  stars::geom_stars(data = raster_diff_scaled_stars, na.action = na.omit) +
  scale_fill_gradientn(
    colors = c('darkgreen', 'lightgreen', 'white', 'red', 'purple'),
    values = scales::rescale(c(-1, 1)),
    limits = c(-1, 1),
    na.value = "transparent"
  ) +
  ggtitle("Accident Density") +
  # Add Basemap
  #geom_sf(data = states_of_interest, fill = 'white') +
  geom_sf(data = king_adjacent_counties, fill = NA, linewidth=0.5) +
  geom_sf(data = water_counties, fill = 'blue', alpha=0.1, linewidth=0.01) +
  # Add Roads
  geom_sf(data = roads_s1100, color = "black", linewidth=0.2) +
  geom_sf(data = roads_s1200, color = "black", linewidth=0.05) +
  # Limits
  xlim(seattle_bbox[1], seattle_bbox[3]) +
  ylim(seattle_bbox[2], seattle_bbox[4]) +
  labs(x = '',
       y = '',
       fill = 'Scaled Post-Pre Pandmeic Accident Average Difference',
       title = 'Seattle metro Traffic Accidents',
       subtitle = '100m') +
  theme_minimal()

ggplot() + 
  stars::geom_stars(data = raster_diff_stars, na.action = na.omit) +
  scale_fill_gradientn(
    colors = c('darkgreen', 'lightgreen', 'yellow', 'red', 'purple'),
    values = scales::rescale(c(-5, 5)),
    limits = c(-5, 5),
    na.value = "transparent"
  ) +
  ggtitle("Accident Density") +
  # Add Basemap
  #geom_sf(data = states_of_interest, fill = 'white') +
  geom_sf(data = king_adjacent_counties, fill = NA, linewidth=0.5) +
  geom_sf(data = water_counties, fill = 'blue', alpha=0.1, linewidth=0.01) +
  # Add Roads
  geom_sf(data = roads_s1100, color = "black", linewidth=0.2) +
  geom_sf(data = roads_s1200, color = "black", linewidth=0.05) +
  # Limits
  xlim(seattle_bbox[1], seattle_bbox[3]) +
  ylim(seattle_bbox[2], seattle_bbox[4]) +
  labs(x = '',
       y = '',
       fill = 'Post-Pre Pandmeic Accident Average Difference',
       title = 'Seattle metro Traffic Accidents',
       subtitle = '100m') +
  theme_minimal()


################################################################################
# Annual Analsyis
################################################################################

annual_traffic_data <- traffic_data_spatial %>% 
  mutate(year = lubridate::year(accident_date)) %>% 
  filter(year %in% 2017:2022) %>% 
  group_by(in_seattle, in_king_county, year) %>% 
  summarize(n = n(),
            mean_accidents = n() / 365,
            mean_hour = mean(as.numeric(accident_hour), na.rm=TRUE),
            mean_severe = mean(severity, na.rm=TRUE),
            mean_duration = mean(duration_of_accident, na.rm=TRUE),
            mean_distance = mean(distance_mi, na.rm=TRUE),
            mean_temp = mean(temperature_f, na.rm=TRUE),
            mean_humidity = mean(humidity_percent, na.rm=TRUE),
            mean_pressure_in = mean(pressure_in, na.rm=TRUE),
            mean_visibility = mean(visibility_mi, na.rm=TRUE),
            mean_wind = mean(wind_speed_mph, na.rm=TRUE),
            mean_precipitation = mean(precipitation_in, na.rm=TRUE),
            n_cloudy = sum(weather_condition_cloudy, na.rm=TRUE),
            n_fog_mist = sum(weather_condition_fog_mist, na.rm=TRUE),
            n_freezing_conditions = sum(weather_condition_freezing_conditions, na.rm=TRUE),
            n_haze_smoke = sum(weather_condition_haze_smoke, na.rm=TRUE),
            n_other = sum(weather_condition_other, na.rm=TRUE),
            n_rain = sum(weather_condition_rain, na.rm=TRUE),
            n_snow = sum(weather_condition_snow, na.rm=TRUE),
            n_thunderstorm = sum(weather_condition_thunderstorm, na.rm=TRUE),
            n_unknown = sum(weather_condition_unknown, na.rm=TRUE),
            n_mix_sleet = sum(weather_condition_wintry_mix_sleet, na.rm=TRUE),
            n_ss_day = sum(sunrise_sunset_day, na.rm=TRUE),
            n_ss_night = sum(sunrise_sunset_night, na.rm=TRUE),
            n_ct_day = sum(civil_twilight_day, na.rm=TRUE),
            n_ct_night = sum(civil_twilight_night, na.rm=TRUE),
            n_nt_day = sum(nautical_twilight_day, na.rm=TRUE),
            n_nt_night = sum(nautical_twilight_night, na.rm=TRUE),
            n_at_day = sum(astronomical_twilight_day, na.rm=TRUE),
            n_at_night = sum(astronomical_twilight_night, na.rm=TRUE))

annual_traffic_data %>% 
  filter(in_seattle == 1) %>% 
  ggplot() +
    geom_point(aes(x = year, y = mean_accidents)) +
    lims(y = c(0, 35))

################################################################################
# Monthly Analysis
################################################################################

monthly_traffic_data <- traffic_data_spatial %>% 
  mutate(year = lubridate::year(accident_date)) %>% 
  group_by(year, accident_month) %>% 
  summarize(n = n(),
            n_days = n_distinct(accident_date),
            mean_hour = mean(as.numeric(accident_hour), na.rm=TRUE),
            mean_severe = mean(severity, na.rm=TRUE),
            mean_duration = mean(duration_of_accident, na.rm=TRUE),
            mean_distance = mean(distance_mi, na.rm=TRUE),
            mean_temp = mean(temperature_f, na.rm=TRUE),
            mean_humidity = mean(humidity_percent, na.rm=TRUE),
            mean_pressure_in = mean(pressure_in, na.rm=TRUE),
            mean_visibility = mean(visibility_mi, na.rm=TRUE),
            mean_wind = mean(wind_speed_mph, na.rm=TRUE),
            mean_precipitation = mean(precipitation_in, na.rm=TRUE),
            n_cloudy = sum(weather_condition_cloudy, na.rm=TRUE),
            n_fog_mist = sum(weather_condition_fog_mist, na.rm=TRUE),
            n_freezing_conditions = sum(weather_condition_freezing_conditions, na.rm=TRUE),
            n_haze_smoke = sum(weather_condition_haze_smoke, na.rm=TRUE),
            n_other = sum(weather_condition_other, na.rm=TRUE),
            n_rain = sum(weather_condition_rain, na.rm=TRUE),
            n_snow = sum(weather_condition_snow, na.rm=TRUE),
            n_thunderstorm = sum(weather_condition_thunderstorm, na.rm=TRUE),
            n_unknown = sum(weather_condition_unknown, na.rm=TRUE),
            n_mix_sleet = sum(weather_condition_wintry_mix_sleet, na.rm=TRUE),
            n_ss_day = sum(sunrise_sunset_day, na.rm=TRUE),
            n_ss_night = sum(sunrise_sunset_night, na.rm=TRUE),
            n_ct_day = sum(civil_twilight_day, na.rm=TRUE),
            n_ct_night = sum(civil_twilight_night, na.rm=TRUE),
            n_nt_day = sum(nautical_twilight_day, na.rm=TRUE),
            n_nt_night = sum(nautical_twilight_night, na.rm=TRUE),
            n_at_day = sum(astronomical_twilight_day, na.rm=TRUE),
            n_at_night = sum(astronomical_twilight_night, na.rm=TRUE))

monthly_traffic_data %>% 
  #filter(in_seattle == 1) %>% 
  ggplot() +
  geom_point(aes(x = accident_month, y = n / n_days)) +
  facet_wrap(~year)


monthly_traffic_data %>% 
  filter(in_seattle == 1) %>% 
  ggplot() +
  geom_point(aes(x = accident_month, y = n)) +
  facet_wrap(~year)

################################################################################
# Pre and Post Covid Statistical Analysis
################################################################################

pre_covid_years <- 2018:2019
post_covid_years <- 2021:2022

stats_df <- traffic_data_spatial %>% 
  sf::st_drop_geometry(.) %>% 
  filter(lubridate::year(accident_date) %in% c(2018, 2019, 2021, 2022),
         in_seattle == 1) %>% 
  mutate(period = ifelse(lubridate::year(accident_date) %in% pre_covid_years, 'pre', 'post')) %>%
  group_by(period) %>% 
  summarize(accidents = n(),
            accidents_per_year = n() / 2,
            accidens_per_day = n() / (365 * 2))
  
# Aggregate the number of accidents per day for each period
accidents_per_day <- traffic_data_spatial %>%
  sf::st_drop_geometry(.) %>% 
  filter(lubridate::year(accident_date) %in% c(2018, 2019, 2021, 2022),
         in_seattle == 1) %>% 
  mutate(period = ifelse(lubridate::year(accident_date) %in% pre_covid_years, 'pre', 'post')) %>%
  group_by(accident_date, period) %>%
  summarize(accidents_per_day = n(), .groups = 'drop')

accidents_per_day %>% 
  ggplot() +
    geom_point(aes(x = accident_date, y = accidents_per_day, color = period))

# Perform a two-sample t-test within dplyr using broom
t_test_result <- accidents_per_day %>%
  group_by(period) %>%
  summarize(accidents_per_day = list(accidents_per_day)) %>%
  summarise(broom::tidy(t.test(unlist(accidents_per_day[period == "pre"]), 
                               unlist(accidents_per_day[period == "post"])))) %>%
  ungroup()

# Print the t-test result
print(t_test_result)

# Aggregate the number of accidents per day for each period
accidents_per_year <- traffic_data_spatial %>%
  sf::st_drop_geometry(.) %>% 
  mutate(year = lubridate::year(accident_date)) %>% 
  filter(year %in% c(2018, 2019, 2021, 2022)) %>% 
  mutate(period = ifelse(year %in% pre_covid_years, 'pre', 'post')) %>%
  group_by(year, period) %>%
  summarize(accidents_per_year = n(), .groups = 'drop')

accidents_per_year %>% 
  ggplot() +
  geom_point(aes(x = year, y = accidents_per_year, color = period))

# Perform a two-sample t-test within dplyr using broom
t_test_result <- accidents_per_year %>%
  group_by(period) %>%
  summarize(accidents_per_year = list(accidents_per_year)) %>%
  summarise(broom::tidy(t.test(unlist(accidents_per_year[period == "pre"]), 
                               unlist(accidents_per_year[period == "post"])))) %>%
  ungroup()

# Print the t-test result
print(t_test_result)


