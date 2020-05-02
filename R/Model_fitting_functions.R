# Model fitting functions


# PointedSDMs plus second spatial field

# Based on procedure from Emily Simmonds

# To fit integrated SDMs based on multiple data sets, with a second spatial field that describes variation 
# unique to the citizen science data.

# Uses Simpson approach for PP data
# Binomial model for PA data
# Using cloglog

library(ggplot2)
library(sf) # for sf objects
library(plyr)
library(dplyr) # for smoother dataframe-manipulation
library(here) # for cleaner filepath-handling
library(ggmap) # also for nice maps
library(maps)
library(RColorBrewer)
library(PointedSDMs)
library(sp)
library(spatstat)
library(maptools)
library(INLA)
library(reshape2)
library(rgeos)
library(fields)
library(viridis)







MakeStacks <- function(data_structured, data_unstructured, covariates, Mesh){

  # INTEGRATION STACK -------------------------------------------------
  stk.ip <- MakeIntegrationStack(mesh = Mesh$mesh, data = covariates, 
                                 area = Mesh$w, tag ='ip', InclCoords=TRUE)
  
  
  # SURVEY, STRUCTURED STACK -------------------------------------------
  # This depends on observation input. It also takes quite some time to match covariates to observation locations.
  NearestCovs_str <- GetNearestCovariate(points = data_structured, covs = covariates)
  NearestCovs_str@data[ , "int.survey"] <- 1 # add intercept 
  
  # Projector matrix from mesh to unstructured data
  projmat.str <- inla.spde.make.A(mesh = Mesh$mesh, loc = as.matrix(data_structured@coords))
  
  # If presences are Boolean, reformat
  if(is.logical(data_structured@data[,"occurrenceStatus"])) {
    data_structured@data[,"occurrenceStatus"] <- as.integer(data_structured@data[,"occurrenceStatus"])
    data_structured@data[,"Ntrials"] <- rep(1, nrow(data_structured@data))
  }
  
  stk.survey <- inla.stack(data=list(resp = cbind(NA, data_structured@data[,"occurrenceStatus"] ), 
                                    Ntrials = data_structured@data[,"Ntrials"]), 
                          A = list(1, projmat.str), 
                          tag = "survey",
                          effects = list(NearestCovs_str@data, list(str_field=1:Mesh$mesh$n)))

  
  
  # ARTSOBS, UNSTRUCTURED STACK -----------------------------------------
  # Finding the covariates that are closest to the observation points
  NearestCovs_unstr <- GetNearestCovariate(points = Data_artsobs, covs = Covariates)
  NearestCovs_unstr@data[ , "int.artsobs"] <- 1 # add intercept 
  
  # Projector matrix from mesh to unstructured data
  projmat.artsobs <- inla.spde.make.A(mesh = Mesh$mesh, loc = as.matrix(Data_artsobs@coords))
  
  stk.artsobs <- inla.stack(data = list(resp = cbind(rep(1,nrow(NearestCovs_unstr)), NA),
                                        e = rep(0, nrow(NearestCovs_unstr))),
                            A = list(1, projmat.artsobs), 
                            tag = "artsobs",
                            effects = list(NearestCovs_unstr@data, 
                                           list(unstr_field = 1:Mesh$mesh$n, 
                                                bias_field = 1:Mesh$mesh$n))) # This is for the second spatial field!
  
  # PREDICTIONS -----------------------------------------------------------
  Nxy.scale <- 0.1 # use this to change the resolution of the predictions
  Nxy <- round(c(diff(norway.poly@bbox[1,]), diff(norway.poly@bbox[2,]))/Nxy.scale)
  stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=covariates, 
                                 tag='pred', boundary=norway.poly)
  
  # Return list of all stacks
  return(list(ip = stk.ip, survey = stk.survey, artsobs = stk.artsobs, pred = stk.pred))
}



# FITTING MODEL --------------------------------------------------------------------

MakeFormula <- function(cov_names, second_sp_field = FALSE){
  intercepts <- "int.survey + int.artsobs - 1"
  env_effects <- paste(cov_names, collapse = ' + ')
  spatial_effects <- "f(unstr_field, model = Mesh$spde) + 
                    f(i, copy = 'unstr_field', fixed = TRUE)"
  
  if(second_sp_field){
    spatial_effects <- paste(spatial_effects, "+ f(bias_field, model = Mesh$spde)")
  }
  
  formula1 <- as.formula(paste(c("resp ~ 0 ", intercepts, env_effects, spatial_effects), collapse = " + "))
  formula1
}


MakeModel <- function(..., formula, Mesh){
  
  NorwegianModel <- FitModel(...,
                             formula = formulaJ,
                             mesh = Mesh$mesh,
                             predictions = TRUE,
                             dic = TRUE,
                             spat.ind = NULL) # About this: I've included the spatial fields in the formula, so this should be fine, 
  # but it may be possible to remove this and remove two of the fields in the formula?
  
  Projection <- CRS("+proj=longlat +ellps=WGS84")
  
  Pred <- SpatialPixelsDataFrame(points = stk.pred$predcoords, 
                                 data = NorwegianModel$predictions, 
                                 proj4string = Projection)
  Pred@data$precision <- Pred@data$stddev^-2
  
  return(list(fitted_model = NorwegianModel, prediction = Pred))
}














