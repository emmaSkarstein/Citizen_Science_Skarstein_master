

source("R/data_prep.r")

lakes <- readRDS("data/Norwegian_lakes.rds")

survey_dirty <- readRDS("Fish_status_survey_of_nordic_lakes/data/merged.rds")
transc_gn_dirty <- readRDS("Transcribed_gillnet_test_fishing_data_norway/data/merged.rds")

survey <- data_prep(survey_dirty, lakes)
saveRDS(survey, "Fish_status_survey_of_nordic_lakes/data/clean.rds")

transc_gn <- data_prep(transc_gn_dirty, lakes)
saveRDS(transc_gn, "Transcribed_gillnet_test_fishing_data_norway/data/clean.rds")

artsobs_dirty <- readRDS("Nordic_Species_Observation_Services/data/GBIF_download_Norge.rds")
artsobs <- data_prep(artsobs_dirty, lakes, max_dist = 30)                         
occ_matched <- artsobs_dirty
