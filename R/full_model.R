# Final model 

setwd("Citizen_Science_Skarstein_master")

# Loading observations and covariates
source("R/loading_map_obs_covs.R")

# Loading functions to fit models
source("R/Model_fitting_functions.R")

# Loading functions to visualize models
source("R/Model_visualization_functions.R")

# Setting fish species
#fish_sp <- "trout"
#fish_sp <- "perch"
#fish_sp <- "char"
fish_sp <- "pike"

if(fish_sp == "trout"){
  lat_name <- "Salmo trutta"
}else if(fish_sp == "perch"){
  lat_name <- "Perca fluviatilis"
}else if(fish_sp == "char"){
  lat_name <- "Salvelinus alpinus"
}else{
  lat_name <- "Esox lucius"
}

message(paste0("Selecting species: ", fish_sp,"\n"))
single_survey_df <- Data_survey_df %>% filter(grepl(lat_name, species))
single_artsobs_df <- Data_artsobs_df %>% filter(grepl(lat_name, species))

single_survey <- SpatialPointsDataFrame(coords = single_survey_df[,c("decimalLongitude","decimalLatitude")], 
                                       data = single_survey_df[,c("occurrenceStatus","species")],
                                       proj4string = Projection)
single_artsobs <- SpatialPointsDataFrame(coords = single_artsobs_df[,c("decimalLongitude","decimalLatitude")], 
                                        data = single_artsobs_df[,c("occurrenceStatus","species")],
                                        proj4string = Projection)

message("Making stacks...\n")
stks <- MakeStacks(data_structured = single_survey, data_unstructured = single_artsobs,
                    env_covariates = env_covariates, all_covariates = Covariates, Mesh = Mesh)

stk.survey <- stks$survey
stk.artsobs <- stks$artsobs
stk.ip <- stks$ip
stk.pred <- stks$pred

Use <- c("decimalLongitude","decimalLatitude", "log_area", 
         "log_catchment", "eurolst_bio10", "SCI")
Use_CS <- c(Use, "distance_to_road", "HFP")

formula2 <- MakeFormula(cov_names = Use, second_sp_field = TRUE, overdispersion = FALSE)
#formula2 <- MakeFormula(cov_names = NULL, second_sp_field = TRUE, overdispersion = FALSE)
formula2

message("Fitting model... \n")
model_final <- FitModelCustom(stk.survey, stk.artsobs, stk.ip, stk.pred$stk,
                            Formula = formula2, mesh = Mesh$mesh)


saveRDS(model_final, paste0("R/output/model_", fish_sp, ".RDS"))

message("Done and model saved!")


