
# install.packages("psych")
library(psych)

setwd("Citizen_Science_Skarstein_master")

source("R/loading_map_obs_covs.R")

source("R/Model_visualization_functions.R")


jpeg("figs/pairs_plot.jpg")

# Looking at correlations
Use <- c("decimalLatitude", "decimalLongitude","eurolst_bio10", "log_area", 
         "log_perimeter", "log_catchment", "SCI", "distance_to_road", "HFP")


pairs.panels(covariateData[,Use], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = FALSE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

dev.off()
