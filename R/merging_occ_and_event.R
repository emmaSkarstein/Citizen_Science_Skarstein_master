############################################################################
# DATA PREPARATION
############################################################################


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

#---------------------------------------------------------------------------
# NORS
merge_occ_event(file_path = "NORS/data", file_type = "csv", sep = ",")

#---------------------------------------------------------------------------
# PIKE
merge_occ_event(file_path = "PIKE/data", file_type = "txt", quote = "")

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
merge_occ_event(file_path = "Transcribed_gillnet_test_fishing_data_norway/data", 
                file_type = "txt")

#---------------------------------------------------------------------------
# Citizen science observations
# See separate script.


