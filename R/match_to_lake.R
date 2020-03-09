library(sf)
library(dplyr)

#' Match to lake
#' 
#' Given a data set of lakefish observations, the function checks if observations are in a lake, and if they are not, matches them to the closest one. (more?)
#'
#' @param data The fish observations. 
#' @param lake_polygons The polygons of the lakes themselves.
#' @param max_dist_from_lake Maximum tolerated distance from a lake before observation is removed.
#'
#' @return A new data set containing only observations that are closer than max_dist_from_lake to a lake.
#' @export
#'
#' @examples
match_to_lake <- function(data, lake_polygons, max_dist_from_lake = 10){
  #-------------------------------------------------------------------------------------------------
  # Transforning to sf and selecting some variables
  #-------------------------------------------------------------------------------------------------
  message("Transforming data and selecting variables...")
  data_sf <- data %>% 
    # Convert to sf object for easier handling. crs = Coordinate Reference System
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
    # Transform coordinate system, using same system as in "lakes"
    sf::st_transform(st_crs(lake_polygons)$epsg)

  
  #-------------------------------------------------------------------------------------------------
  # Find closest lake
  #-------------------------------------------------------------------------------------------------
  message("Joining occurrence data to lake polygons by closest lake...")
  occ_with_lakes <- sf::st_join(data_sf, lake_polygons, join = st_nearest_feature)

  #-------------------------------------------------------------------------------------------------
  # Find distance to closest lake
  #-------------------------------------------------------------------------------------------------
  message("Calculating distance to closest lake...")
  index <- sf::st_nearest_feature(x = data_sf, y = lake_polygons) # index of closest lake
  closest_lakes <- lake_polygons %>% slice(index) # slice based on the index
  dist_to_lake <- sf::st_distance(x = data_sf, y = closest_lakes, by_element = TRUE) # get distance
  occ_with_lakes$dist_to_lake <- as.numeric(dist_to_lake) # add the distance calculations to match data

  #-------------------------------------------------------------------------------------------------
  # Filter out occurrence records not matching lakes (given certain criteria)
  #-------------------------------------------------------------------------------------------------
  occ_matched <- occ_with_lakes %>% dplyr::filter(dist_to_lake < max_dist_from_lake) # 
  
  
  #-------------------------------------------------------------------------------------------------
  # Looking closer at occurrence records not matching a lake
  #-------------------------------------------------------------------------------------------------
  occ_far_from_lake <- occ_with_lakes %>% dplyr::filter(dist_to_lake > max_dist_from_lake)
  
  # Observations outside limit:
  message("Number of observations further than ", max_dist_from_lake ,"m from a lake: ", nrow(occ_far_from_lake))
  message("Done! We removed ", round(nrow(occ_far_from_lake)/nrow(occ_with_lakes)*100), "% of the original observations.")

  return(list(occ_matched, occ_with_lakes))
}
