#' Create annual rasters
#'
#' @param df - Spatial dataframe with data of interest
#' @param region - SF region of interest
#' @param raster_crs - Localized CRS in meters
#' @param mapping_crs - CRS of mapping (usually 4326)
#' @param resolution - Numeric resolution of pixels in meters
#' @param date_col - Chracter of date column
#'
#' @return List of rasters named by year
#' @export
#'
#' @examples create_annual_raster(df = traffic_data_spatial, region = king_county,  raster_crs = 32610,  mapping_crs = 4326,  resolution = 500,  date_col = 'accident_date')
create_annual_raster <- function(df, region, raster_crs, mapping_crs, resolution, date_col = 'accident_date') {
  
  # Create BBOX
  df_bbox <- sf::st_bbox(sf::st_transform(region, raster_crs))
  df_bbox[1] <- df_bbox[[1]] - resolution
  df_bbox[2] <- df_bbox[[2]] - resolution
  df_bbox[3] <- df_bbox[[3]] + resolution
  df_bbox[4] <- df_bbox[[4]] + resolution
  
  # Create extent
  extent <- raster::extent(df_bbox[c("xmin", "xmax", "ymin", "ymax")])
  
  # Define years
  years <- sort(unique(lubridate::year(df[[date_col]])))
  
  # Create template raster
  raster_obj <- raster::raster(extent, res = resolution, crs=raster_crs)
  
  # Separate data by year
  df_annual <- lapply(years, function(x) df %>% filter(lubridate::year(!!sym(date_col)) == x))
  
  # Rasterize
  raster_filled_annual <- lapply(seq_along(df_annual), function(x) {
    # Rasterize
    raster_filled <- raster::rasterize(sf::st_coordinates(sf::st_transform(df_annual[[x]], raster_crs)), 
                                       raster_obj, 
                                       fun = 'count', 
                                       background = 0)
    
    # Convert to mapping CRS
    raster_filled <- raster::projectRaster(raster_filled, crs = sf::st_crs(mapping_crs)$wkt)
    
    # Convert to stars object
    raster_filled_stars <- stars::st_as_stars(raster_filled)
    
    # Set CRS for the stars object
    suppressWarnings(sf::st_crs(raster_filled_stars) <- mapping_crs)
    
    return(raster_filled_stars)
  })
  
  names(raster_filled_annual) <- years
  
  return(raster_filled_annual)
}

#' Create raster
#'
#' @param df - Spatial dataframe with data of interest
#' @param region - SF region of interest
#' @param raster_crs - Localized CRS in meters
#' @param mapping_crs - CRS of mapping (usually 4326)
#' @param resolution - Numeric resolution of pixels in meters
#' @param date_col - Chracter of date column
#'
#' @return Raster of interest
#' @export
#'
#' @examples create_annual_raster(df = traffic_data_spatial, region = king_county,  raster_crs = 32610,  mapping_crs = 4326,  resolution = 500,  date_col = 'accident_date')
create_raster <- function(df, region, raster_crs, mapping_crs, resolution, date_col = 'accident_date') {
  
  # Create BBOX
  df_bbox <- sf::st_bbox(sf::st_transform(region, raster_crs))
  df_bbox[1] <- df_bbox[[1]] - resolution
  df_bbox[2] <- df_bbox[[2]] - resolution
  df_bbox[3] <- df_bbox[[3]] + resolution
  df_bbox[4] <- df_bbox[[4]] + resolution
  
  # Create extent
  extent <- raster::extent(df_bbox[c("xmin", "xmax", "ymin", "ymax")])
  
  # Create an empty raster
  raster_obj <- raster::raster(extent, res = resolution, crs=raster_crs)
  
  
  # Rasterize
  raster_filled <- raster::rasterize(sf::st_coordinates(sf::st_transform(df, raster_crs)), 
                                                        raster_obj, 
                                                        fun = 'count',
                                                        background = 0)
  raster_filled <- raster::projectRaster(raster_filled, crs = sf::st_crs(mapping_crs)$wkt)
  
  # Convert to stars and change crs
  raster_filled <- stars::st_as_stars(raster_filled)
  suppressWarnings(sf::st_crs(raster_filled) <- mapping_crs)
  
  return(raster_filled)
}


#' Consolidate weather variables
#'
#' @param weather - Character column of weather classifications
#'
#' @return - Character column of consolidated weather classifications
#' @export
#'
#' @examples consolidate_weather(traffic_data$weather_condition)
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

#' Create accident video
#'
#' @param df_sf - SF dataframe with accident_date, time_hour columns
#' @param start_date - Character YYYY-MM-DD date
#' @param end_date  - Character YYYY-MM-DD date
#' @param fps - Numeric for frames per second
#' @param res - Character of hourly or daily
#' @param bbox - Bounding region for map
#' @param spatial_list - List with map objects like states/counties
#' @param output_location - Character of file output location
#'
#' @return
#' @export
#'
#' @examples
accident_video_gen <- function(df_sf, start_date, end_date, fps, res, geo_res,
                               bbox, spatial_list, output_location) {
  
  # Filter for date
  df_sf <- df_sf %>% 
    filter(accident_date >= as.Date(start_date) & accident_date <= as.Date(end_date))
  
  # Get resolution
  if (res == 'hourly') {
    nframes <- length(unique(df_sf$time_hour)) # number of unique hours
    transition <- 'time_hour'
    label_text <- "time_hour"
  } else if (res == 'daily') {
    nframes <- length(unique(df_sf$accident_date)) # number of unique dates
    transition <- 'accident_date'
    label_text <- "accident_date"
  }
  
  if (geo_res == 'state') {
    t1_region <- spatial_list$states_of_interest
    t2_region <- spatial_list$counties_of_interest
    supp_roads <- spatial_list$roads_s1200
  } else if (geo_res == 'city') {
    t1_region <- spatial_list$king_county
    t2_region <- spatial_list$king_county_adjacent
    supp_roads <- spatial_list$roads_s1400
  }
  
  water <- sf::st_crop(sf::st_transform(spatial_objects$water_counties, 4326), 
                       bbox, 
                       10)
  
  # Create video
  accident_gif <- df_sf %>% 
    # Create map
    ggplot() +
    # Add Basemap
    geom_sf(data = t1_region, fill = 'white', linetype = "dotted", linewidth=0.2) +
    geom_sf(data = t2_region, fill = NA, linewidth=1) +
    # Add Roads
    geom_sf(data = spatial_list$roads_s1100, color = "black", linewidth=0.25) +
    geom_sf(data = spatial_list$roads_s1200, color = "black", linewidth=0.1) +
    geom_sf(data = supp_roads, color = "black", linewidth=0.05) +
    geom_sf(data = water, fill = 'blue', linewidth=0.0001, alpha=0.1) +
    # Add accidents
    geom_sf(size = 3, shape = 23, color = 'red', fill = 'orange', stroke = 1) +
    # Add dynamic label for time or date
    geom_text(aes(x = Inf, y = Inf, label = !!sym(label_text)), 
              hjust = 1.1, vjust = 1.1, color = 'black', size = 6) +
    # Transition
    gganimate::transition_time(!!sym(transition)) +
    gganimate::shadow_mark(past = TRUE, future = FALSE, alpha = 0.75, size = 1) +
    gganimate::ease_aes('linear') +
    # Limits
    xlim(bbox[1], bbox[3]) +
    ylim(bbox[2], bbox[4]) +
    theme(panel.border = element_rect(colour = "black", fill=NA),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
  
  animated_video <- gganimate::animate(accident_gif, 
                                       renderer = gganimate::av_renderer(), 
                                       fps = fps, 
                                       nframes = nframes,
                                       width = 1920,  # width in pixels
                                       height = 1080) # height in pixels
  
  gganimate::anim_save(output_location, animation = animated_video)
}


bbox_clip <- function(polygon_sf, bbox_sf, buffer) {
  
  buffered_bbox <- sf::st_buffer(sf::st_as_sfc(bbox), buffer)
  
  st_crop(polygon_sf, buffered_bbox)
}










