# Full model fitting and validation


# Model validation
# Training and test sets

library(blockCV)
library(foreach)
library(doParallel)

setwd("Citizen_Science_Skarstein_master")

# Loading observations and covariates
source("R/loading_map_obs_covs.R")

# Loading functions to fit models
source("R/Model_fitting_functions.R")

# Loading functions to visualize models
source("R/Model_visualization_functions.R")



# DIVIDE INTO TRAINING AND TEST FOLDS FOR MODEL VALIDATION
k <- 5
sb <- spatialBlock(speciesData = trout_survey, # sf or SpatialPoints
                   species = "occurrenceStatus", # the response column (binomial or multi-class)
                   #rasterLayer = validation_raster, # a raster for background (optional)
                   theRange = 150000, # size of the blocks in meters
                   k = k, # number of folds
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = FALSE)

# SELECTING TRAINING AND TEST SETS
survey_traintest <- TrainTest(sb, trout_survey, sb$k)
artsobs_traintest <- TrainTest(sb, trout_artsobs, sb$k)


# Setting up parallel backend
cl <- parallel::makeForkCluster(detectCores())
parallel <- TRUE
if(parallel){
  doParallel::registerDoParallel(cl)
}else{
  foreach::registerDoSEQ()
}


modelList <- foreach::foreach(i = 1:k) %dopar% {
  
  survey_train <- do.call("rbind", unlist(sapply((1:k)[-i], function(s) survey_traintest[[1]][[s]])))
  artsobs_train <- do.call("rbind", unlist(sapply((1:k)[-i], function(s) artsobs_traintest[[1]][[s]])))
  survey_test <- survey_traintest[[1]][[i]]
  # artsobs_test: we don't use the CS data for testing.
  
  # MAKE STACKS
  # (note: strictly speaking intergation and prediction stack could have been made 
  #  outside the loop, but don't thing they matter so much)
  stks <- MakeStacks(data_structured = survey_train, data_unstructured = artsobs_train,
                     covariates = Covariates, Mesh = Mesh)

  stk.survey <- stks$survey
  stk.artsobs <- stks$artsobs
  stk.ip <- stks$ip
  stk.pred <- stks$pred
  
  stk.test <- MakeTestStack(survey_test, Covariates, Mesh)
  
  # CONSTRUCT FORMULAS 
  Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
           "eurolst_bio10", "SCI")
  Use_CS <- c(Use, "distance_to_road", "HFP")
  
  formula1 <- MakeFormula(cov_names = Use, second_sp_field = FALSE)
  formula2 <- MakeFormula(cov_names = Use, second_sp_field = TRUE)
  formula3 <- MakeFormula(cov_names = Use_CS, second_sp_field = FALSE)
  formula4 <- MakeFormula(cov_names = Use_CS, second_sp_field = TRUE)
  
  # FIT MODELS
  model1 <- FitModelTest(stk.survey, stk.artsobs, stk.ip, stk.pred$stk, stk.test,
                         Formula = formula1, mesh = Mesh$mesh, predictions = TRUE)

  model2 <- FitModelTest(stk.survey, stk.artsobs, stk.ip, stk.pred$stk, stk.test,
                         Formula = formula2, mesh = Mesh$mesh, predictions = TRUE)

  model3 <- FitModelTest(stk.survey, stk.artsobs, stk.ip, stk.pred$stk, stk.test,
                         Formula = formula3, mesh = Mesh$mesh, predictions = TRUE)

  model4 <- FitModelTest(stk.survey, stk.artsobs, stk.ip, stk.pred$stk, stk.test,
                        Formula = formula4, mesh = Mesh$mesh, predictions = TRUE)
  
  # CALCULATE DIC
  resp <- survey_test$occurrenceStatus
  mod_res1 <- CalcLinPred(model1, resp)
  mod_res2 <- CalcLinPred(model2, resp)
  mod_res3 <- CalcLinPred(model3, resp)
  mod_res4 <- CalcLinPred(model4, resp)
  
  
  list(dic1 = mod_res1$deviance, dic2 = mod_res2$deviance, dic3 = mod_res3$deviance, dic4 = mod_res4$deviance)
}

parallel::stopCluster(cl)

saveRDS(modelList, "cv_output_4mods.RDS")

modelList
res <- readRDS("cv_output_4mods.RDS")

mod1_res <- res[[1]]$dic1

test <- as.matrix(sapply(res, '['))
rowMeans(test)

test <- matrix(unlist(res), ncol = 5)
rowm <- rowMeans(test)
