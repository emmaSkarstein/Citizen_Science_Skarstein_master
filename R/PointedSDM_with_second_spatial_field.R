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

# MAP ---------------------------------------------------------------------------------------------------------
# (nothing new here)
norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- setdiff(norway, filter(norway, subregion == "Jan Mayen"))
Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill=TRUE, plot=FALSE, 
                  ylim=c(58,72), xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, 
                                   proj4string = Projection)

# LOADING DATA AND COVARIATES ---------------------------------------------------------------------------------
# (nothing new here)

# Covariates
covariateData <- readRDS("data/environmental_covariates.RDS")
covariateData <- covariateData[complete.cases(covariateData$decimalLatitude,covariateData$decimalLongitude,covariateData$area_km2,covariateData$HFP),]
covariateData <- covariateData %>% mutate(log_area = log(area_km2)) %>% select(-c(ebint, no_vatn_lnr, eb_waterregionID))

head(covariateData)

# Choose from 
# "decimalLatitude", "decimalLongitude",
# "area_km2", "perimeter_m", "distance_to_road", 
# "eurolst_bio10", "catchment_area_km2", "SCI", "HFP"
Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
         "eurolst_bio10", "SCI")

Covariates <- SpatialPointsDataFrame(coords = covariateData[,c("decimalLongitude","decimalLatitude")],
                                     data = covariateData[,Use], 
                                     proj4string = Projection)
Covariates@data <- data.frame(apply(Covariates@data, 2, scale))  # scale the covariates

# Observations
Data_survey_df <- readRDS("Fish_status_survey_of_nordic_lakes/data/clean.rds")
Data_survey <- SpatialPointsDataFrame(coords = Data_survey_df[,c("decimalLongitude","decimalLatitude")], 
                                      data = Data_survey_df[,c("occurrenceStatus","species")],
                                      proj4string = Projection)

Data_artsobs_df <- readRDS("Nordic_Species_Observation_Services/data/clean.rds")
Data_artsobs <- SpatialPointsDataFrame(coords = Data_artsobs_df[,c("decimalLongitude","decimalLatitude")], 
                                       data = Data_artsobs_df[,c("occurrenceStatus","species")],
                                       proj4string = Projection)

# Now we have the covariates in 'Covariates', as well as two types of data sets in 'Data_survey' and 'Data_artsobs'.  




# INTEGRATION STACK --------------------------------------------------------------------------------------------
Meshpars <- list(cutoff=0.08, max.edge=c(1, 3), offset=c(1,1))
Mesh <- MakeSpatialRegion(data=NULL, bdry=norway.poly, meshpars=Meshpars,
                          proj = Projection)

stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=Covariates, 
                               area=Mesh$w, tag='ip', InclCoords=TRUE)


# SURVEY, STRUCTURED STACK ---------------------------------------------------------------------------------------------
stk.survey <- MakeBinomStack(observs = Data_survey, data = Covariates, 
                             mesh=Mesh$mesh, presname="occurrenceStatus",  
                             tag="survey", InclCoords=TRUE)
# Note that when using 'MakeBinomStack' here, the spatial effect in stk.survey is just called "i", 
#  while in the unstructured data stack we called the corresponding effect 'unstr_field'. 


# ARTSOBS, UNSTRUCTURED STACK -------------------------------------------------------------------------------------------
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


# PREDICTIONS ----------------------------------------------------------------------------------------------------------
Nxy.scale <- 0.1 # use this to change the resolution of the predictions
Nxy <- round(c(diff(norway.poly@bbox[1,]), diff(norway.poly@bbox[2,]))/Nxy.scale)
stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=Covariates, 
                               tag='pred',boundary=norway.poly)


# FITTING MODEL --------------------------------------------------------------------------------------------------------

# First specifying the formula components
intercepts <- "int.survey + int.artsobs - 1"
env_effects <- paste(Use, collapse = ' + ')
spatial_effects <- "f(unstr_field, model = Mesh$spde) + 
                    f(i, copy = 'unstr_field', fixed = TRUE) + 
                    f(bias_field, model = Mesh$spde)"

formulaJ <- as.formula(paste(c("resp ~ 0 ", intercepts, env_effects, spatial_effects), collapse = " + "))
  
NorwegianModel <- FitModel(stk.survey, stk.artsobs,
                           stk.ip, stk.pred$stk,
                           formula = formulaJ,
                           mesh = Mesh$mesh,
                           predictions = TRUE, 
                           spat.ind = NULL) # About this: I've included the spatial fields in the formula, so this should be fine, 
                                            # but it may be possible to remove this and remove two of the fields in the formula?

summary(NorwegianModel$model)
  
Pred <- SpatialPixelsDataFrame(points=stk.pred$predcoords, 
                               data=NorwegianModel$predictions, 
                               proj4string=Projection)
Pred@data$precision <- Pred@data$stddev^-2

  
# PLOTTING -----------------------------------------------------------------

Pred.plot <- plot(Pred)

plot.mean.stddev <- function(pred){
  ncolors <- 200
  greencols.fn <- colorRampPalette(brewer.pal(9, "Greens"))
  greencols <- greencols.fn(ncolors)
  bluecols.fn <- colorRampPalette(brewer.pal(9, "Blues"))
  bluecols <- bluecols.fn(ncolors)
  map.mean <- mapview::mapview(pred, zcol = c("mean"), legend = TRUE,
                               col.regions = greencols)
  map.stddev <- mapview::mapview(pred, zcol = c("stddev"), legend = TRUE, alpha = 0.3, 
                                 col.regions = bluecols)
  leafsync::sync(map.mean, map.stddev)
}
  
plot.mean.stddev(Pred)



# To plot the bias field:
random_sp_effects <- data.frame(cbind(NorwegianModel$model$summary.random$bias_field$mean,
                             NorwegianModel$model$summary.random$bias_field$sd,
                             NorwegianModel$model$summary.random$unstr_field$mean,
                             NorwegianModel$model$summary.random$unstr_field$sd,
                             Mesh$mesh$loc[,1:2]))
colnames(random_sp_effects) <- c("bias_field", "bf_sd", "shared_field", "sf_sd", "longitude", "latitude")

# bias_map_sp <- SpatialPointsDataFrame(coords = cbind(bias_map["longitude"], bias_map["latitude"]),
#                                    data = bias_map["bias_field"], 
#                                    proj4string = Projection)
# mapview::mapview(bias_map_sp, zcol = c("bias_field"), legend = TRUE,
#                              col.regions = greencols)

random_sp_effects_long <- tidyr::gather(random_sp_effects, field_type, value, bias_field:sf_sd, factor_key = TRUE)

ggplot(random_sp_effects_long) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = longitude, y = latitude, color = value), alpha = 0.6) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_viridis() +
  facet_wrap(field_type ~ .)

p <- ggplot(random_sp_effects) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

 
p1 <- p + geom_point(aes(x = longitude, y = latitude, color = bias_field), alpha = 0.6) +
  scale_color_viridis()
p1.1 <- p + geom_point(aes(x = longitude, y = latitude, color = bf_sd), alpha = 0.6) +
  scale_color_viridis()
p2 <- p + geom_point(aes(x = longitude, y = latitude, color = shared_field), alpha = 0.6) +
  scale_color_viridis()
p2.1 <- p + geom_point(aes(x = longitude, y = latitude, color = sf_sd), alpha = 0.6) +
  scale_color_viridis()

figure1 <- ggarrange(p1, p1.1, p2, p2.1, ncol = 2, nrow = 2)
figure2 <- ggarrange(p1, p2, ncol = 2)


p1
figure1

NorwegianModel$model$dic





beepr::beep(2)





