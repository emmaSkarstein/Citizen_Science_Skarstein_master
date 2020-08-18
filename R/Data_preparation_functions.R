##############################################################################
# GENERAL DATA PREPARATION 
# for freshwater fish data used in masters' thesis
##############################################################################

library(dplyr)
library(plyr)
library(sf)

merge_occ_event <- function(file_path, file_type, sep = "\t", quote = "\"'"){
  occ_path <- paste0(file_path, "/occurrence.", file_type)
  event_path <- paste0(file_path, "/event.", file_type)
  
  occ <- read.table(occ_path, header = TRUE, sep = sep, quote = quote, 
                    fill = FALSE)
  event <- read.table(event_path, header = TRUE,sep = sep, quote = quote, 
                      fill = FALSE)
  
  data <- merge(occ, event, by = "eventID")
  
  saveRDS(data, paste0(file_path, "/merged.rds"))
}



# Note that the match-to-lake function was originally written by Anders G. Finstad.

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


data_prep <- function(dirty_data, lakes, max_dist = 20){
  #-------------------------------------------------------------------------
  # Match to closest lake 
  #-------------------------------------------------------------------------
  occ_list <- match_to_lake(dirty_data, lakes, max_dist)
  occ_matched <- occ_list[[1]]
  occ_w_lakes <- occ_list[[2]]
  
  #-------------------------------------------------------------------------
  # Remove all observations with no time variable
  #-------------------------------------------------------------------------
  occ_matched <- occ_matched[complete.cases(occ_matched$year, 
                                            occ_matched$month,occ_matched$day),]
  
  #-------------------------------------------------------------------------
  # Select 12 most prevalent species
  #-------------------------------------------------------------------------
  if(!("species" %in% colnames(occ_matched))){
    occ_matched <- occ_matched %>% dplyr::rename("species" = scientificName)
  }
  abundant_species <- occ_matched %>% filter(occurrenceStatus == "present") %>% 
    dplyr::count(species, sort = TRUE) %>% top_n(n = 12, wt = n)
  
  occ_matched <- occ_matched %>% filter(species %in% abundant_species$species) %>% 
    st_set_geometry(NULL)
  
  
  #-------------------------------------------------------------------------
  # Make occurence-status logical
  #-------------------------------------------------------------------------
  occ_matched$occurrenceStatus <- as.logical(mapvalues(occ_matched$occurrenceStatus, 
                                                       c("absent", "present"), c(FALSE,TRUE)))
  
  #-------------------------------------------------------------------------
  # Return clean data
  #-------------------------------------------------------------------------
  return(occ_matched)            
  
} 



