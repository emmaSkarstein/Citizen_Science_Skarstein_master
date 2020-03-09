library(dplyr)
library(here)

#source(here::here("R/download_lakefish.R"))
source("R/match_to_lake.R")

occ_lakefish <- readRDS("Fish_status_survey_of_nordic_lakes/data/merged.rds")
lakes <- readRDS("data/Norwegian_lakes.rds")


#-------------------------------------------------------------------------
# Step 1: Filter out species not in list
#-------------------------------------------------------------------------
#fish_names <- read.table("fish_names.txt", sep = "\n", stringsAsFactors = FALSE)[,1]
#occ_lakefish <- occ %>% filter(species %in% fish_names)

#-------------------------------------------------------------------------
# Step 2: Match to closest lake
#-------------------------------------------------------------------------
#occ_list <- match_to_lake(occ_lakefish, lakes)
#occ_matched <- occ_list[[1]]
#occ_w_lakes <- occ_list[[2]]
#saveRDS(occ_matched, here::here("data", paste0("GBIF_download_", name, "_matched.rds")))
#saveRDS(occ_w_lakes, here::here("data", paste0("GBIF_download_", name, "_wlakes.rds")))

#-------------------------------------------------------------------------
# Step 3: Remove all observations with no time variable
#-------------------------------------------------------------------------
occ_matchedd <- readRDS("Fish_status_survey_of_nordic_lakes/data/occ_matched.rds")

occ_matched <- occ_matched[complete.cases(occ_matched$year,occ_matched$month,occ_matched$day),]
st_geometry(occ_matched) <- NULL

#-------------------------------------------------------------------------
#Step 4: Select 12 most prevalent species
#-------------------------------------------------------------------------
occ_matched <- occ_matched %>% rename("species" = scientificName)
abundant_species <- occ_matched %>% filter(occurrenceStatus == "present") %>% count(vars = species) %>% top_n(n = 12, wt = n) %>% arrange(desc(n))
occ_matched <- occ_matched %>% filter(species %in% abundant_species$vars)

#-------------------------------------------------------------------------
# For presence-absence: Make occurence-status logical
#-------------------------------------------------------------------------
occ_matched$occurrenceStatus <- as.logical(mapvalues(occ_matched$occurrenceStatus, c("absent", "present"), c(FALSE,TRUE)))

#-------------------------------------------------------------------------
# Save this in the data-folder
#-------------------------------------------------------------------------
saveRDS(occ_matched, "Fish_status_survey_of_nordic_lakes/data/matched_cleaned.rds")            
               
               

