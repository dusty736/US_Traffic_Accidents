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

traffic_data_spatial <- traffic_data %>% 
  sf::st_as_sf(., coords=c('longitude', 'latitude'), crs=4326)

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
# Sample Map
################################################################################

king_county <- counties_of_interest %>% 
  filter(state == 'WA' & NAME == 'King')

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

king_county_raster <- create_raster(df = traffic_data_spatial,
                             region = king_county, 
                             raster_crs = 32610, 
                             mapping_crs = 4326, 
                             resolution = 500, 
                             date_col = 'accident_date')

annual_king_county_rasters <- create_annual_raster(df = traffic_data_spatial,
                                                   region = king_county, 
                                                   raster_crs = 32610, 
                                                   mapping_crs = 4326, 
                                                   resolution = 500, 
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
colors <- rev(RColorBrewer::brewer.pal(11, "RdYlGr"))

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
       subtitle = '500m') +
  theme_minimal()

################################################################################
# Seattle Map
################################################################################

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

king_county <- counties_of_interest %>% filter(NAME == 'King')

seattle_sf <- sf::st_intersection(sf::st_make_valid(sf::st_transform(king_county, 4326)), 
                                  sf::st_make_valid(bbox_sf))

seattle_bbox <- sf::st_bbox(seattle_sf)

seattle_raster <- create_raster(df = traffic_data_spatial,
                                region = seattle_sf, 
                                raster_crs = 32610, 
                                mapping_crs = 4326, 
                                resolution = 250, 
                                date_col = 'accident_date')

annual_seattle_rasters <- create_annual_raster(df = traffic_data_spatial,
                                               region = seattle_sf, 
                                               raster_crs = 32610, 
                                               mapping_crs = 4326, 
                                               resolution = 250, 
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
colors <- rev(RColorBrewer::brewer.pal(11, "RdYlGr"))

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


################################################################################
# Annual Analsyis
################################################################################

seattle_accident_ids <- traffic_data_spatial %>% 
  sf::st_intersection(., seattle_sf) %>% 
  pull(id)

# Recode weather
# Function to consolidate weather conditions
consolidate_weather <- function(weather) {
  case_when(
    weather %in% c("Clear", "Fair", "Fair / Windy") ~ "Clear/Fair",
    weather %in% c("Scattered Clouds", "Partly Cloudy", "Overcast", "Mostly Cloudy", 
                   "Cloudy", "Cloudy / Windy", "Mostly Cloudy / Windy", "Partly Cloudy / Windy") ~ "Cloudy",
    weather %in% c("Light Rain", "Rain", "Heavy Rain", "Light Rain / Windy", "Rain / Windy", 
                   "Heavy Rain / Windy", "Light Rain Shower", "Light Rain Showers", "Light Drizzle", 
                   "Heavy Drizzle", "Drizzle", "Light Drizzle / Windy", "Heavy T-Storm", 
                   "Light Rain with Thunder", "Drizzle and Fog") ~ "Rain",
    weather %in% c("Light Snow", "Snow", "Heavy Snow", "Light Snow / Windy", "Snow / Windy", 
                   "Light Snow Shower", "Light Snow and Sleet / Windy") ~ "Snow",
    weather %in% c("Thunderstorm", "Light Thunderstorms and Rain", "Thunderstorms and Rain", 
                   "Thunder in the Vicinity", "T-Storm", "Thunder", "Heavy T-Storm") ~ "Thunderstorm",
    weather %in% c("Mist", "Fog", "Patches of Fog", "Shallow Fog") ~ "Fog/Mist",
    weather %in% c("Haze", "Smoke", "Smoke / Windy") ~ "Haze/Smoke",
    weather %in% c("Light Freezing Rain", "Light Freezing Fog", "Light Freezing Drizzle", 
                   "Light Ice Pellets", "Freezing Rain / Windy") ~ "Freezing Conditions",
    weather %in% c("Wintry Mix", "Sleet", "Wintry Mix / Windy", "Light Snow and Sleet / Windy") ~ "Wintry Mix/Sleet",
    weather %in% c("N/A Precipitation", "Small Hail", "Squalls", "Blowing Dust / Windy", "Hail") ~ "Other",
    TRUE ~ "Unknown"
  )
}

test <- traffic_data_spatial %>% 
  mutate(weather = consolidate_weather(weather_condition))

traffic_spatial_coded <- traffic_data_spatial %>%
  filter(id %in% seattle_accident_ids) %>%
  dplyr::select(id, weather_condition, sunrise_sunset, civil_twilight, nautical_twilight, astronomical_twilight) %>%
  mutate(across(c(weather_condition, sunrise_sunset, civil_twilight, nautical_twilight, astronomical_twilight),
                ~ replace_na(.x, "Unknown"))) %>%  # Replace NA with "Unknown"
  mutate(across(c(weather_condition, sunrise_sunset, civil_twilight, nautical_twilight, astronomical_twilight),
                ~ replace(.x, .x == "", "Unknown"))) %>%  # Replace empty strings with "Unknown"
  pivot_longer(cols = c(weather_condition, sunrise_sunset, civil_twilight, nautical_twilight, astronomical_twilight),
               names_to = "variable", values_to = "value") %>%
  mutate(value = factor(value)) %>%
  pivot_wider(names_from = value, values_from = value, values_fill = list(value = 0), values_fn = list(value = length)) %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, 0, 1)))

annual_traffic_data <- traffic_data_spatial %>% 
  sf::st_intersection(., seattle_sf) %>% 
  mutate(year = lubridate::year(accident_date)) %>% 
  group_by(year) %>% 
  summarize(n = n(),
            mean_hour = mean(as.numeric(accident_hour), na.rm=TRUE),
            mean_severe = mean(severity, na.rm=TRUE),
            mean_duration = mean(duration_of_accident, na.rm=TRUE),
            mean_distance = mean(distance_mi, na.rm=TRUE),
            mean_temp = mean(temperature_f, na.rm=TRUE),
            mean_humidity = mean(humidity_percent, na.rm=TRUE),
            mean_pressure_in = mean(pressure_in, na.rm=TRUE),
            mean_visibility = mean(visibility_mi, na.rm=TRUE),
            mean_wind = mean(wind_speed_mph, na.rm=TRUE),
            mean_precipitation = mean(precipitation_in, na.rm=TRUE))




################################################################################
# Montly Map
################################################################################




################################################################################
# Hourly Map
################################################################################







