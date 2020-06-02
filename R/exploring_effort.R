# EXPLORING EFFORT


# Loading observations and covariates
source("R/loading_map_obs_covs.R")

# Loading functions to fit models
source("R/Model_fitting_functions.R")

# Loading functions to visualize models
source("R/Model_visualization_functions.R")


# Make stacks
norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- setdiff(norway, filter(norway, subregion == "Jan Mayen"))
Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill=TRUE, plot=FALSE, 
                  ylim=c(58,72), xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, 
                                   proj4string = Projection)

# INTEGRATION STACK -------------------------------------------------
stk.ip <- MakeIntegrationStack(mesh = Mesh$mesh, data = covariates, 
                               area = Mesh$w, tag ='ip', InclCoords=TRUE)



# ARTSOBS, UNSTRUCTURED STACK -----------------------------------------
# Finding the covariates that are closest to the observation points
NearestCovs_unstr <- GetNearestCovariate(points = trout_artsobs, covs = Covariates)
NearestCovs_unstr@data[ , "int.artsobs"] <- 1 # add intercept 

# Projector matrix from mesh to unstructured data
projmat.artsobs <- inla.spde.make.A(mesh = Mesh$mesh, loc = as.matrix(trout_artsobs@coords))

stk.artsobs0 <- inla.stack(data = list(resp = cbind(rep(1,nrow(NearestCovs_unstr)), NA),
                                      e = rep(0, nrow(NearestCovs_unstr))), # why is this zero?
                          A = list(1, projmat.artsobs), 
                          tag = "artsobs",
                          effects = list(NearestCovs_unstr@data, 
                                         list(shared_field = 1:Mesh$mesh$n, 
                                              bias_field = 1:Mesh$mesh$n, # This is for the second spatial field!
                                              id.iid = 1:Mesh$mesh$n)))

stk.artsobs1 <- inla.stack(data = list(resp = cbind(rep(1,nrow(NearestCovs_unstr)), NA),
                                       e = rep(1, nrow(NearestCovs_unstr))),
                           A = list(1, projmat.artsobs), 
                           tag = "artsobs",
                           effects = list(NearestCovs_unstr@data, 
                                          list(shared_field = 1:Mesh$mesh$n, 
                                               bias_field = 1:Mesh$mesh$n, # This is for the second spatial field!
                                               id.iid = 1:Mesh$mesh$n))) 

stk.artsobs_area <- inla.stack(data = list(resp = cbind(rep(1,nrow(NearestCovs_unstr)), NA),
                                           e=exp(rep(NearestCovs_unstr@data$log_area))),
                           A = list(1, projmat.artsobs), 
                           tag = "artsobs",
                           effects = list(NearestCovs_unstr@data, 
                                          list(shared_field = 1:Mesh$mesh$n, 
                                               bias_field = 1:Mesh$mesh$n, # This is for the second spatial field!
                                               id.iid = 1:Mesh$mesh$n))) 


stks <- MakeStacks(data_structured = trout_survey, data_unstructured = trout_artsobs,
                   covariates = Covariates, Mesh = Mesh)

stk.survey <- stks$survey
stk.ip <- stks$ip
stk.pred <- stks$pred

Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
         "eurolst_bio10", "SCI")
Use_CS <- c(Use, "distance_to_road", "HFP")
formula4 <- MakeFormula(cov_names = Use_CS, second_sp_field = TRUE)

model4_0 <- FitModelTest(stk.survey, stk.artsobs0, stk.ip, stk.pred$stk,
                       Formula = formula4, mesh = Mesh$mesh, predictions = TRUE)
model4_1 <- FitModelTest(stk.survey, stk.artsobs1, stk.ip, stk.pred$stk,
                         Formula = formula4, mesh = Mesh$mesh, predictions = TRUE)
model4_area <- FitModelTest(stk.survey, stk.artsobs_area, stk.ip, stk.pred$stk,
                         Formula = formula4, mesh = Mesh$mesh, predictions = TRUE)
saveRDS(model4_0, "R/output/model_0.RDS")
saveRDS(model4_1, "R/output/model_1.RDS")
saveRDS(model4_area, "R/output/model_area.RDS")
saveRDS(stk.pred, "R/output/stkpred.RDS")

resp <- trout_survey$occurrenceStatus

model4_0$model$dic$deviance.mean
model4_1$model$dic$deviance.mean
model4_area$model$dic$deviance.mean

Pred_0 <- SpatialPixelsDataFrame(points=stk.pred$predcoords, 
                               data=model4_0$predictions, 
                               proj4string=Projection)
Pred_1 <- SpatialPixelsDataFrame(points=stk.pred$predcoords, 
                                 data=model4_1$predictions, 
                                 proj4string=Projection)
Pred_area <- SpatialPixelsDataFrame(points=stk.pred$predcoords, 
                                data=model4_area$predictions, 
                                proj4string=Projection)

plot(Pred_0)
plot(Pred_1)
plot(Pred_area)
