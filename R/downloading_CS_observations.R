##-------------------------------------------------------------------------------
# Downloading fish from Artsobservasjoner and Artobservasjoner
##-------------------------------------------------------------------------------


source('R/Download_lakefish_functions.R')
#fish_names <- readRDS('fish_names.rds')
fish_names <- c("Salmo trutta", "Perca fluviatilis", "Salvelinus alpinus", "Esox lucius")

#setwd(paste0(getwd(), "/Norwegian_Species_Observation_Service"))

download_lakefish_dataset(fish_names, n_try = 100, file_marker = 'Norge', 
                          key_dataset = 'b124e1e0-4755-430f-9eab-894f25a9b59c')


# Obs: This downloads the same species for Sweden. 
# The species list is based on the most common NORWEGIAN fish.

#download_lakefish_dataset(fish_names, n_try = 100, file_marker = 'Sverige', 
#                          key_dataset = '38b4c89f-584c-41bb-bd8f-cd1def33e92f',  
#                          key_tax = NA)

