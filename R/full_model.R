# Final model 

setwd("Citizen_Science_Skarstein_master")

# Loading observations and covariates
source("R/loading_map_obs_covs.R")

# Loading functions to fit models
source("R/Model_fitting_functions.R")

# Loading functions to visualize models
source("R/Model_visualization_functions.R")

message("Making stacks...\n")
stks <- MakeStacks(data_structured = trout_survey, data_unstructured = trout_artsobs,
                   env_covariates = env_covariates, all_covariates = Covariates, Mesh = Mesh)

stk.survey <- stks$survey
stk.artsobs <- stks$artsobs
stk.ip <- stks$ip
stk.pred <- stks$pred

Use <- c("decimalLongitude","decimalLatitude", "log_area", 
         "log_catchment", "eurolst_bio10", "SCI")
Use_CS <- c(Use, "distance_to_road", "HFP")

formula4 <- MakeFormula(cov_names = Use_CS, second_sp_field = TRUE, overdispersion = FALSE)
#formula4 <- MakeFormula(cov_names = Use_CS, second_sp_field = TRUE)

message("Fitting model... \n")
model_final <- FitModelTest(stk.survey, stk.artsobs, stk.ip, stk.pred$stk,
                            Formula = formula4, mesh = Mesh$mesh, predictions = TRUE)
#saveRDS(model_final, "R/output/model_final.RDS")
saveRDS(model_final, "R/output/model_final_2.RDS")
message("Done and model saved!")


