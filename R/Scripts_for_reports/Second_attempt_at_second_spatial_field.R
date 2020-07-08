
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


setwd("Citizen_Science_Skarstein_master")

source("R/Model_fitting_functions.R")

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

# MAP ---------------------------------------------------------------------------------------------------------
norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- dplyr::setdiff(norway, filter(norway, subregion == "Jan Mayen"))
Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill=TRUE, plot=FALSE, 
                  ylim=c(58,72), xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, 
                                   proj4string = Projection)



# LOADING DATA AND COVARIATES ---------------------------------------------------------------------------------

# Covariates
covariateData <- readRDS("data/environmental_covariates.RDS")
covariateData <- covariateData[complete.cases(covariateData$decimalLatitude,covariateData$decimalLongitude,covariateData$area_km2,covariateData$HFP),]
covariateData <- covariateData %>% mutate(log_area = log(area_km2), log_catchment = log(catchment_area_km2)) %>% select(-c(ebint, no_vatn_lnr, eb_waterregionID))

head(covariateData)

# Choose from 
# "decimalLatitude", "decimalLongitude",
# "area_km2", "perimeter_m", "distance_to_road", 
# "eurolst_bio10", "catchment_area_km2", "SCI", "HFP"
Use <- c("decimalLongitude","decimalLatitude", "log_area", 
         "log_catchment", "eurolst_bio10", "SCI")
Use_CS <- c(Use, "distance_to_road", "HFP")

Covariates <- SpatialPointsDataFrame(coords = covariateData[,c("decimalLongitude","decimalLatitude")],
                                     data = covariateData[,Use_CS], 
                                     proj4string = Projection)
#Covariates@data <- data.frame(apply(Covariates@data, 2, scale))  # scale the covariates


# Observations
Data_survey_df <- readRDS("Fish_status_survey_of_nordic_lakes/data/clean.rds") %>% 
  filter(grepl(lat_name, species))
trout_survey <- SpatialPointsDataFrame(coords = Data_survey_df[,c("decimalLongitude","decimalLatitude")], 
                                       data = Data_survey_df[,c("occurrenceStatus","species")],
                                       proj4string = Projection)

Data_artsobs_df <- readRDS("Nordic_Species_Observation_Services/data/clean.rds") %>% 
  filter(grepl(lat_name, species))
trout_artsobs <- SpatialPointsDataFrame(coords = Data_artsobs_df[,c("decimalLongitude","decimalLatitude")], 
                                        data = Data_artsobs_df[,c("occurrenceStatus","species")],
                                        proj4string = Projection)




# INTEGRATION STACK --------------------------------------------------------------------------------------------
Meshpars <- list(cutoff=0.08, max.edge=c(1, 3), offset=c(1,1))
Mesh <- MakeSpatialRegion(data=NULL, bdry=norway.poly, meshpars=Meshpars,
                          proj = Projection)
plot(Mesh$mesh)



stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=Covariates, 
                               area=Mesh$w, tag='ip', InclCoords=TRUE)



# SURVEY, STRUCTURED STACK ---------------------------------------------------------------------------------------------
stk.survey <- MakeBinomStack(observs = trout_survey, data = Covariates, 
                             mesh=Mesh$mesh, presname="occurrenceStatus",  
                             tag="survey", InclCoords=TRUE)
# Note that when using 'MakeBinomStack' here, the spatial effect in stk.survey is just called "i", 
#  while in the unstructured data stack we called the corresponding effect 'shared_field'. 



# ARTSOBS, UNSTRUCTURED STACK -------------------------------------------------------------------------------------------
# Finding the covariates that are closest to the observation points
NearestCovs_unstr <- GetNearestCovariate(points = trout_artsobs, covs = Covariates)
NearestCovs_unstr@data[ , "int.artsobs"] <- 1 # add intercept 

# Projector matrix from mesh to unstructured data
projmat.artsobs <- inla.spde.make.A(mesh = Mesh$mesh, loc = as.matrix(trout_artsobs@coords))

stk.artsobs <- inla.stack(data = list(resp = cbind(rep(1,nrow(NearestCovs_unstr)), NA),
                                      e = rep(0, nrow(NearestCovs_unstr))),
                          A = list(1, projmat.artsobs), 
                          tag = "artsobs",
                          effects = list(NearestCovs_unstr@data, 
                                         list(shared_field = 1:Mesh$mesh$n, 
                                              bias_field = 1:Mesh$mesh$n))) # This is for the second spatial field!


# PREDICTIONS ----------------------------------------------------------------------------------------------------------
Nxy.scale <- 0.1 # use this to change the resolution of the predictions
Nxy <- round(c(diff(norway.poly@bbox[1,]), diff(norway.poly@bbox[2,]))/Nxy.scale)
stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=Covariates, 
                               tag='pred',boundary=norway.poly)
saveRDS(stk.pred, "R/output/stkpred.RDS")




# FITTING MODEL -------------------------------------------------------------------------

stck <- inla.stack(stk.survey, stk.artsobs, stk.ip, stk.pred$stk)

#mesh <- inla.spde2.matern(Mesh$mesh)
mesh.shared <- INLA::inla.spde2.pcmatern(Mesh$mesh,
                                  prior.range = c(20, 0.1),
                                  prior.sigma = c(0.1, 0.01))
mesh.bias <- INLA::inla.spde2.pcmatern(Mesh$mesh,
                                         prior.range = c(20, 0.1),
                                         prior.sigma = c(0.1, 0.01))

# First specifying the formula components
intercepts <- "int.survey + int.artsobs - 1"
env_effects <- paste(Use, collapse = ' + ')
spatial_effects <- "f(shared_field, model = mesh.shared) + 
                    f(i, copy = 'shared_field', fixed = TRUE) + 
                    f(bias_field, model = mesh.bias)"

formulaJ <- as.formula(paste(c("resp ~ 0 ", intercepts, env_effects, spatial_effects), collapse = " + "))

formulaJ


# Fit model including predictions
mod <- inla(formulaJ, family=c('poisson','binomial'),
            control.family = list(list(link = "log"), list(link = "cloglog")),
            data=inla.stack.data(stck), verbose=FALSE,
            control.results=list(return.marginals.random=FALSE,
                                 return.marginals.predictor=FALSE),
            control.predictor=list(A=inla.stack.A(stck), link=NULL, compute=TRUE),
            control.fixed = list(mean=0),
            Ntrials=inla.stack.data(stck)$Ntrials, E=inla.stack.data(stck)$e,
            control.compute=list(waic=FALSE, dic=FALSE))

# For predictions
id <- inla.stack.index(stck, "pred")$data
pred <- data.frame(mean=mod$summary.fitted.values$mean[id],
                   stddev=mod$summary.fitted.values$sd[id])



saveRDS(list(model = mod, predictions = pred), paste0("R/output/model_", fish_sp, ".RDS"))







