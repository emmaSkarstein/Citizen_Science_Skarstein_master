#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------

library(rgbif)        # for interacting with gbif
library(rgeos)
library(dplyr)        # for data-wrangling
library(rstudioapi)   # for writing in user info
library(rio)          # a swiss army knife for data I/O
library(here)         # for knowing where we are

# Note that the download functions were originally written by Anders G. Finstad.

#' Download GBIF API
#' 
#' Download occurrence data through GBIF API.
#'
#' @param download_key 
#' @param destfile_name 
#' @param n_try 
#' @param Sys.sleep_duration 
#'
#' @return Nothing, but saves data to temporary directory.
#'
#' @examples
download_GBIF_API <- function(download_key,destfile_name,n_try,Sys.sleep_duration){
  # Coffebreak version (see https://github.com/GBIF-Europe/nordic_oikos_2018_r/blob/master/s3_gbif_demo/Download_gbif.md)
  
  message("Attempting to download data from GBIF with ", n_try," attempts at ", Sys.sleep_duration, " second intervals.")
  start_time <- Sys.time()
  n_try_count <- 1
  
  download_url <- paste("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1],sep="")
  
  try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                    quiet=TRUE, mode="wb"),silent = TRUE)
  
  while (inherits(try_download, "try-error") & n_try_count < n_try) {   
    Sys.sleep(Sys.sleep_duration)
    n_try_count <- n_try_count+1
    try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                      quiet=TRUE, mode="wb"),silent = TRUE)
    message("trying... Download link not ready. Time elapsed (min): ",
        round(as.numeric(paste(difftime(Sys.time(),start_time, 
                                        units = "mins"))),2))
  }
}

#' Download lakefish
#'
#' @param latin_name a character string, latin name of wanted species.
#' @param n_try numeric, number of download tries, default 10.
#'
#' @return Nothing, but saves occurrence data and key to "data" folder in current working directory.
#' @export
#'
#' @examples
download_lakefish_dataset <- function(latin_names, n_try = 10, file_marker = NA, 
                                      key_dataset = NA, key_tax = NA){
  #-------------------------------------------------------------------------------
  # Register credentials 
  #-------------------------------------------------------------------------------
  message("Register credentials\n")
  
  options(gbif_user = rstudioapi::askForPassword("my gbif username"))
  options(gbif_email = rstudioapi::askForPassword("my registred gbif e-mail"))
  options(gbif_pwd = rstudioapi::askForPassword("my gbif password"))
  
  
  #-------------------------------------------------------------------------------
  # Set search parameters and get download KEY
  #-------------------------------------------------------------------------------
  message("Setting search parameters and getting download key...")
  
  keys = rep(NA, length(latin_names))
  # Find a taxonkey - get list of gbif keys to filter download
  if (is.na(key_tax)){
    for(i in 1:length(latin_names)){
      key <- name_suggest(q = toString(latin_names[i]), rank = 'species')$data$key[1] 
      keys[i] = key
    }
  }
  else {
    keys <- key_tax
  }
  
  #-------------------------------------------------------------------------
  # Send download request
  #-------------------------------------------------------------------------
  message("Sending download request...")
  
  # Spawn download request - careful, there is a max limit on simultanious downloads 
  # per user allowed by the GBIF API. 
  download_key <- occ_download(
    pred('datasetKey', key_dataset), 
    pred_in('taxonKey', keys),
    pred("hasCoordinate", TRUE),
    type = "and") %>% 
    occ_download_meta
  
  # For overview of your downloads, see https://www.gbif.org/user/download . 
  # The preparation will of the data may take some time.
  temp <- tempdir()
  
  download_GBIF_API(download_key = download_key, 
                    destfile_name = paste0(temp,"/tmp.zip"),
                    n_try = n_try, Sys.sleep_duration = 30)
  message("------IGNORE THE WARNING MESSAGES------")
  
  
  #--------------------------------------------------------------------------
  # Read in the occurrence data
  #--------------------------------------------------------------------------
  message("Reading in the occurrence data...")
  
  occ <- rio::import(unzip(paste0(temp,"/tmp.zip"), files = "occurrence.txt"))
  
  # Create "data"-folder if this doesn't exist
  if (!dir.exists(here::here("Nordic_Species_Observation_Services/data"))){ 
    dir.create(here::here("Nordic_Species_Observation_Services/data"))
  }
  
  # Set the file-marker (string to be added to file-name) to keys if not given
  if (is.na(file_marker)){
    file_marker <- keys
  }
  
  # Save the occurrence data to a file
  message("Saving occurrence data as ", paste0("GBIF_download_", file_marker, ".rds\n"))
  saveRDS(occ, here::here("data", paste0("GBIF_download_", file_marker, ".rds")))
  
  unlink(temp)
  unlink(here::here("GBIF_download", "occurrence.txt"))
  
  
  # Write download key and citation to .rds files for reference (also available on https://www.gbif.org/user/download)
  saveRDS(download_key,here::here("Nordic_Species_Observation_Services/data", paste0("GBIF_download_key_", file_marker, ".rds")))
  
  # CITE YOUR DATA!!! 
  citation <- paste0("GBIF Occurrence Download https://doi.org/", 
                     download_key[2], " accessed via GBIF.org on ", 
                     Sys.Date())
  
  message("Your ", latin_names, "-data has now been downloaded.")
  return(list(citation = citation, key = keys))
}
