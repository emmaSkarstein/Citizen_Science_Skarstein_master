

source("R/Data_preparation_functions.r")

lakes <- readRDS("data/Norwegian_lakes.rds")

# MERGING OCCURRENCE AND EVENT

#---------------------------------------------------------------------------
# NORS
#merge_occ_event(file_path = "NORS/data", file_type = "csv", sep = ",")

#---------------------------------------------------------------------------
# PIKE
#merge_occ_event(file_path = "PIKE/data", file_type = "txt", quote = "")

#---------------------------------------------------------------------------
# Kautokeino
# Not available?

#---------------------------------------------------------------------------
# Trondheim
# Already merged

#---------------------------------------------------------------------------
# Fish status survey of nordic lakes
merge_occ_event(file_path = "Fish_status_survey_of_nordic_lakes/data", 
                file_type = "txt")

#---------------------------------------------------------------------------
# Transcriptions of gillnet test-fishing
#merge_occ_event(file_path = "Transcribed_gillnet_test_fishing_data_norway/data", 
#                file_type = "txt")

#---------------------------------------------------------------------------
# Citizen science observations
# See separate script.


# CLEANING DATA
survey_dirty <- readRDS("Fish_status_survey_of_nordic_lakes/data/merged.rds")
#transc_gn_dirty <- readRDS("Transcribed_gillnet_test_fishing_data_norway/data/merged.rds")
artsobs_dirty <- readRDS("Nordic_Species_Observation_Services/data/GBIF_download_Norge.rds")


survey <- data_prep(survey_dirty, lakes)
saveRDS(survey, "Fish_status_survey_of_nordic_lakes/data/clean.rds")

#transc_gn <- data_prep(transc_gn_dirty, lakes)
#saveRDS(transc_gn, "Transcribed_gillnet_test_fishing_data_norway/data/clean.rds")

artsobs <- data_prep(artsobs_dirty, lakes, max_dist = 30)                         
saveRDS(artsobs, "Nordic_Species_Observation_Services/data/clean.rds")



