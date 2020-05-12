# Whole procedure

setwd("Citizen_Science_Skarstein_master")

# Load map, covariates and observations

source("R/loading_map_obs_covs.R")
source("R/Model_fitting_functions.R")


RunSpeciesModels <- function(species, survey, artsobs){
  message("Making stack...")
  stk <- MakeStacks(data_structured = survey, data_unstructured = artsobs,
                    covariates = Covariates, Mesh = Mesh)
  
  # Choose from 
  # "decimalLatitude", "decimalLongitude",
  # "area_km2", "perimeter_m", "distance_to_road", 
  # "eurolst_bio10", "catchment_area_km2", "SCI", "HFP"
  Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
           "eurolst_bio10", "SCI")
  Use_CS <- c(Use, "distance_to_road", "HFP")
  
  formula1 <- MakeFormula(cov_names = Use, second_sp_field = FALSE)
  formula2 <- MakeFormula(cov_names = Use, second_sp_field = TRUE)
  formula3 <- MakeFormula(cov_names = Use_CS, second_sp_field = FALSE)
  formula4 <- MakeFormula(cov_names = Use_CS, second_sp_field = TRUE)
  
  message("Model1...")
  model1 <- MakeModel(stk, formula1, Mesh)
  pred1 <- MakePred(stk$pred, model1)
  
  message("Model2...")
  model2 <- MakeModel(stk, formula2, Mesh)
  pred2 <- MakePred(stk$pred, model2)
  
  message("Model3...")
  model3 <- MakeModel(stk, formula3, Mesh)
  pred3 <- MakePred(stk$pred, model3)
  
  message("Model4...")
  model4 <- MakeModel(stk, formula4, Mesh)
  pred4 <- MakePred(stk$pred, model4)
  
  return(list(model1 = model1, pred1 = pred1, model2 = model2, pred2 = pred2, 
              model3 = model3, pred3 = pred3, model4 = model4, pred4 = pred4, 
              species))
  
}

pike_models <- RunSpeciesModels("pike", pike_survey, pike_artsobs)
saveRDS(pike_models, "R/output/pike_models.RDS")

trout_models <- RunSpeciesModels("trout", trout_survey, trout_artsobs)
saveRDS(trout_models, "R/output/trout_models.RDS")

char_models <- RunSpeciesModels("char", char_survey, char_artsobs)
saveRDS(char_models, "R/output/char_models.RDS")

perch_models <- RunSpeciesModels("perch", perch_survey, perch_artsobs)
saveRDS(perch_models, "R/output/perch_models.RDS")


# plot(pike_models$pred2)
# plot(trout_models$pred2)
# plot(char_models$pred2)
# plot(perch_models$pred2)
# 
# 
# PlotSpatialFields(pike_models$model2)
# PlotSpatialFields(trout_models$model2)
# PlotSpatialFields(char_models$model2)
# PlotSpatialFields(perch_models$model2)


# Now looking only at trout:
trout.stk <- MakeStacks(data_structured = trout_survey, data_unstructured = trout_artsobs, 
                        covariates = Covariates, Mesh = Mesh)

Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
         "eurolst_bio10", "SCI")
Use_CS <- c(Use, "distance_to_road", "HFP")

formula5 <- MakeFormula(cov_names = Use, second_sp_field = TRUE, overdispersion = TRUE)

trout_mod <- MakeModel(trout.stk, formula5, Mesh)
trout_pred <- MakePred(trout.stk$pred, trout_mod)

plot(trout_pred)
PlotSpatialFields(trout_mod, biasfield = TRUE)



