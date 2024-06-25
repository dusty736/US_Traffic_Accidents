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
  dplyr::select(all_of(columns_of_interest)) %>% 
  filter(county == 'King')

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
roads_s1400 <- roads %>% 
  filter(MTFCC == 'S1400')

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

spatial_object_output <- list(king_county = king_county,
                              king_county_adjacent = king_adjacent_counties,
                              seattle = seattle_sf,
                              water_counties = water_counties,
                              wa_bbox = plotting_bbox,
                              seattle_bbox = seattle_bbox,
                              roads_s1100 = roads_s1100,
                              roads_s1200 = roads_s1200,
                              roads_s1400 = roads_s1400,
                              roads = roads,
                              states_of_interest = states_of_interest,
                              counties_of_interest = counties_of_interest)

saveRDS(spatial_object_output, "data/mapping/spatial_objects.rds")

################################################################################
# Feature Engineering
################################################################################

# Define holidays
easter <- timeDate::Easter(2016:2023)
nyd <- timeDate::USNewYearsDay(2016:2023)
mlk <- timeDate::USMLKingsBirthday(2016:2023)
pres <- timeDate::USPresidentsDay(2016:2023)
mem <- timeDate::USMemorialDay(2016:2023)
ind <- timeDate::USIndependenceDay(2016:2023)
ld <- timeDate::USLaborDay(2016:2023)
cd <- timeDate::USColumbusDay(2016:2023)
ed <- timeDate::USElectionDay(2016:2023)
vd <- timeDate::USVeteransDay(2016:2023)
tg <- timeDate::USThanksgivingDay(2016:2023)
chr <- timeDate::USChristmasDay(2016:2023)
all_holidates <- list('easter' = as.Date(easter), 
                      'nyd' = as.Date(nyd), 
                      'mlk' = as.Date(mlk), 
                      'pres' = as.Date(pres), 
                      'mem' = as.Date(mem), 
                      'ind' = as.Date(ind), 
                      'ld' = as.Date(ld), 
                      'cd' = as.Date(cd), 
                      'ed' = as.Date(ed), 
                      'vd' = as.Date(vd), 
                      'tg' = as.Date(tg), 
                      'chr' = as.Date(chr))

# Convert the list into a data frame
holidays_df <- all_holidates %>%
  enframe(name = "holiday", value = "date") %>%
  unnest(cols = date) %>%
  mutate(accident_date = as.Date(date))

traffic_data_full <- traffic_data %>% 
  # New or updated columns
  mutate(accident_hour_dec = paste0(lubridate::hour(start_time) + round(lubridate::minute(start_time) / 60,
                                                                        2)),
         accident_hour = lubridate::hour(start_time),
         accident_dow = lubridate::wday(start_time, label = TRUE),
         accident_month = lubridate::month(start_time),
         accident_year = lubridate::year(start_time),
         duration_of_accident = round(as.numeric(end_time - start_time), 0),
         accident_date = as.Date(start_time),
         accident_time = format(start_time, "%H:%M:%S"),
         zipcode = substr(zipcode, 1, 5),
         precipitation_in = ifelse(is.na(precipitation_in), 0, precipitation_in)) %>% 
  # Clean up names
  rename(longitude = start_lng,
         latitude = start_lat) %>% 
  # Add holidays
  left_join(., holidays_df, by='accident_date') %>% 
  mutate(holiday = ifelse(is.na(holiday), 'None', holiday)) %>% 
  # Organize
  dplyr::select(id, severity, longitude, latitude, accident_date, accident_dow,
                accident_time, accident_hour, accident_hour_dec, accident_month, 
                accident_year, holiday, duration_of_accident,
                distance_mi, zipcode, city, county, temperature_f,
                humidity_percent, pressure_in, visibility_mi, wind_speed_mph,
                precipitation_in, weather_condition, sunrise_sunset, 
                civil_twilight, nautical_twilight, astronomical_twilight)

# Traffic data categorical
traffic_coded <- traffic_data_full %>%
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
traffic_data_spatial <- traffic_data_full %>% 
  sf::st_as_sf(., coords=c('longitude', 'latitude'), crs=4326, remove=FALSE) %>% 
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

# Add Seattle and king county column
traffic_data_spatial <- traffic_data_spatial %>% 
  mutate(in_seattle = ifelse(id %in% seattle_accident_ids, 1, 0),
         in_king_county = ifelse(id %in% king_accident_ids, 1, 0)) %>% 
  dplyr::select(id, in_seattle, in_king_county, everything())

################################################################################
# Daily Level Data
################################################################################

daily_traffic_data <- traffic_data_spatial %>% 
  group_by(accident_date, accident_dow, accident_month, accident_year, holiday) %>% 
  summarize(n = n(),
            mean_hour = mean(as.numeric(accident_hour_dec), na.rm=TRUE),
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

monthly_traffic_data <- traffic_data_spatial %>% 
  group_by(accident_month, accident_year) %>% 
  summarize(n = n(),
            mean_hour = mean(as.numeric(accident_hour_dec), na.rm=TRUE),
            mean_severe = mean(severity, na.rm=TRUE),
            mean_duration = mean(duration_of_accident, na.rm=TRUE),
            mean_distance = mean(distance_mi, na.rm=TRUE),
            mean_temp = mean(temperature_f, na.rm=TRUE),
            mean_humidity = mean(humidity_percent, na.rm=TRUE),
            mean_pressure_in = mean(pressure_in, na.rm=TRUE),
            mean_visibility = mean(visibility_mi, na.rm=TRUE),
            mean_wind = mean(wind_speed_mph, na.rm=TRUE),
            mean_precipitation = mean(precipitation_in, na.rm=TRUE),
            n_holiday = sum(holiday != 'None'),
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

annual_traffic_data <- traffic_data_spatial %>% 
  group_by(accident_year) %>% 
  summarize(n = n(),
            mean_hour = mean(as.numeric(accident_hour_dec), na.rm=TRUE),
            mean_severe = mean(severity, na.rm=TRUE),
            mean_duration = mean(duration_of_accident, na.rm=TRUE),
            mean_distance = mean(distance_mi, na.rm=TRUE),
            mean_temp = mean(temperature_f, na.rm=TRUE),
            mean_humidity = mean(humidity_percent, na.rm=TRUE),
            mean_pressure_in = mean(pressure_in, na.rm=TRUE),
            mean_visibility = mean(visibility_mi, na.rm=TRUE),
            mean_wind = mean(wind_speed_mph, na.rm=TRUE),
            mean_precipitation = mean(precipitation_in, na.rm=TRUE),
            n_holiday = sum(holiday != 'None'),
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


################################################################################
# Save
################################################################################

data.table::fwrite(traffic_data_spatial %>% sf::st_drop_geometry(.), "data/full_traffic_data.csv")
data.table::fwrite(daily_traffic_data %>% sf::st_drop_geometry(.), "data/daily_traffic_data.csv")
data.table::fwrite(monthly_traffic_data %>% sf::st_drop_geometry(.), "data/monthly_traffic_data.csv")
data.table::fwrite(annual_traffic_data %>% sf::st_drop_geometry(.), "data/annual_traffic_data.csv")





