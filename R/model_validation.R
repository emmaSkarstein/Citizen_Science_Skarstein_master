# Model validation
# Training and test sets

library(blockCV)
library(foreach)
library(doParallel)

setwd("Citizen_Science_Skarstein_master")

source("R/loading_map_obs_covs.R")
source("R/Model_fitting_functions.R")
source("R/Model_visualization_functions.R")



CalcLinPred <- function(Model, resp){
  glm(resp ~ 1 + offset(Model$model$summary.linear.predictor[inla.stack.index(stk.test,"test")$data,"mean"]))
}

# Make stacks for cs data, integration points and predictions, since these don't vary based on train/test split
# stks <- MakeStacks(data_structured = trout_survey, data_unstructured = trout_artsobs,
#                    covariates = Covariates, Mesh = Mesh)
# 
# saveRDS(stks, "stacks.RDS")

stks <- readRDS("stacks.RDS")
stk.artsobs <- stks$artsobs
stk.ip <- stks$ip
stk.pred <- stks$pred


k <- 3
sb <- spatialBlock(speciesData = trout_survey, # sf or SpatialPoints
                   species = "occurrenceStatus", # the response column (binomial or multi-class)
                   #rasterLayer = validation_raster, # a raster for background (optional)
                   theRange = 150000, # size of the blocks in meters
                   k = k, # number of folds
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = FALSE)

# sb2 <- spatialBlock(speciesData = Data_survey,
#                     species = "occurrenceStatus",
#                     rows = 10,
#                     selection = "checkerboard")

folds <- sb$foldID



cl <- parallel::makeForkCluster(detectCores())
parallel <- FALSE
if(parallel){
  doParallel::registerDoParallel(cl)
}else{
  foreach::registerDoSEQ()
}


modelList <- foreach::foreach(i = 1:k) %dopar% {
  trainSet <- which(folds != i) # training set indices
  testSet <- which(folds == i) # testing set indices
  
  trout_train <- trout_survey[trainSet,]
  trout_test <- trout_survey[testSet,]
  
  # norway <- readRDS("R/gadm36_NOR_0_sf.rds")
  # plot(norway$geometry)
  # plot(trout_train, pch = 20, cex = 0.5, col = 'hotpink', add = TRUE)
  # plot(trout_test, pch = 20, cex = 0.5, col = 'green', add = TRUE)
  
  print("Training stack... \n")
  stk.train <- MakeStructuredStack(trout_train, Covariates, Mesh)
  print("Test stack... \n")
  stk.test <- MakeTestStack(trout_test, Covariates, Mesh)
  
  print("Fitting model... \n")
  Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
           "eurolst_bio10", "SCI")
  formula1 <- MakeFormula(cov_names = Use, second_sp_field = FALSE)
  
  Model <- FitModelTest(stk.train, stk.artsobs, stk.ip, stk.pred$stk, stk.test,
                        formula = formula1, CovNames = Use, mesh = Mesh$mesh, predictions = TRUE)
  
  resp <- trout_test$occurrenceStatus
  mod_val <- CalcLinPred(Model, resp)
  #list(Model, mod_val)
  mod_val
}

parallel::stopCluster(cl)

saveRDS(modelList, "cv_output_seq.RDS")

#res <- readRDS("cv_output.RDS")
