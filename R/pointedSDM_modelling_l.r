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


# MAP ----------------------------------------------------------------------
norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- setdiff(norway, filter(norway, subregion == "Jan Mayen"))

Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill=TRUE, plot=FALSE, 
                  ylim=c(58,72), xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, 
                                   proj4string = Projection)


# COVARIATES ---------------------------------------------------------------
covariateData <- readRDS("data/environmental_covariates.RDS")
covariateData <- covariateData[complete.cases(covariateData$decimalLatitude,covariateData$decimalLongitude,covariateData$area_km2,covariateData$HFP),]
covariateData <- covariateData %>% mutate(log_area = log(area_km2))

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


# DATA ---------------------------------------------------------------------
Data_survey_df <- readRDS("Fish_status_survey_of_nordic_lakes/data/clean.rds")
Data_survey <- SpatialPointsDataFrame(coords = Data_survey_df[,c("decimalLongitude","decimalLatitude")], 
                                       data = Data_survey_df[,c("occurrenceStatus","species")],
                                       proj4string = Projection)

Data_artsobs_df <- readRDS("Nordic_Species_Observation_Services/data/clean.rds")
Data_artsobs <- SpatialPointsDataFrame(coords = Data_artsobs_df[,c("decimalLongitude","decimalLatitude")], 
                                       data = Data_artsobs_df[,c("occurrenceStatus","species")],
                                       proj4string = Projection)

# MESHGRID -----------------------------------------------------------------
Meshpars <- list(cutoff=0.08, max.edge=c(1, 3), offset=c(1,1))
Mesh <- MakeSpatialRegion(data=NULL, bdry=norway.poly, meshpars=Meshpars,
                          proj = Projection)

stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=Covariates, 
                               area=Mesh$w, tag='ip', InclCoords=TRUE)


# PREDICTIONS --------------------------------------------------------------
Nxy.scale <- 0.1 # use this to change the resolution of the predictions
Nxy <- round(c(diff(norway.poly@bbox[1,]), diff(norway.poly@bbox[2,]))/Nxy.scale)
stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=Covariates, 
                               tag='pred',boundary=norway.poly)

# STACKING -----------------------------------------------------------------
stk.survey <- MakeBinomStack(observs = Data_survey, data = Covariates, 
                             mesh=Mesh$mesh, presname="occurrenceStatus",  
                             tag="survey", InclCoords=TRUE) #polynoms = norway.poly,

stk.artsobs <- MakePointsStack(presences = Data_artsobs, data = Covariates,
                               mesh = Mesh$mesh, 
                               tag = "artsobs", InclCoords = TRUE)


# MODELLING ----------------------------------------------------------------

NorwegianModel <- FitModel(stk.survey, stk.ip, stk.pred$stk, 
                           CovNames=Use, mesh = Mesh$mesh,
                           predictions = TRUE, spat.ind = NULL)

summary(NorwegianModel$model)

Pred <- SpatialPixelsDataFrame(points=stk.pred$predcoords, 
                               data=NorwegianModel$predictions, 
                               proj4string=Projection)
Pred@data$precision <- Pred@data$stddev^-2


# PLOTTING -----------------------------------------------------------------
pdf(file = "figs/second_sp_field.pdf")
Pred.plot <- plot(Pred)
dev.off()

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


beepr::beep(2)

