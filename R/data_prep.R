##############################################################################
# GENERAL DATA PREPARATION 
# for freshwater fish data used in masters' thesis
##############################################################################

library(dplyr)
library(plyr)

data_prep <- function(dirty_data, lakes, max_dist = 20){
  source("R/match_to_lake.R")
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
  
