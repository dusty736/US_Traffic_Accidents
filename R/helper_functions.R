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
  raster_filled_annual <- lapply(seq_along(df_annual), 
                                 function(x) raster::rasterize(sf::st_coordinates(sf::st_transform(df_annual[[x]], raster_crs)), 
                                                               raster_obj, 
                                                               fun = 'count',
                                                               background = 0))
  
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
  
  return(raster_filled)
}














