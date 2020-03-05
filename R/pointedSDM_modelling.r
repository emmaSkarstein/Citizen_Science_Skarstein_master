library(ggplot2)
library(sf) # for sf objects
library(plyr)
library(dplyr) # for smoother dataframe-manipulation
library(here) # for cleaner filepath-handling
library(ggmap) # also for nice maps
library(maps)

library(PointedSDMs)
library(sp)
library(spatstat)
library(maptools)
library(RColorBrewer)

# Originally written by Lyder Iversen, based on vignette by Bob o'Hara, 
# modified by Emma Skarstein


##### MAP #####
norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- setdiff(norway, filter(norway, subregion == "Jan Mayen"))
Projection <- CRS("+proj=longlat +ellps=WGS84")
norwayfill <- map("world", "norway", fill = TRUE, plot = FALSE, 
                  ylim=c(58,72), xlim=c(4,32))
IDs <- sapply(strsplit(norwayfill$names, ":"), function(x) x[1])
norway.poly <- map2SpatialPolygons(norwayfill, IDs = IDs, proj4string = Projection)

#sapply(slot(norway.poly, "polygons"), function(x) length(slot(x, "Polygons")))


##### COVARIATES #####
covariateData <- readRDS("data/environmental_covariates.RDS")
# Choose from 
# "decimal_latitude", "decimal_longitude",
# "area_km2", "perimeter_m", "distance_to_road", 
# "eurolst_bio10", "catchment_area_km2", "SCI", "HFP"
Use <- c("decimalLongitude","decimalLatitude")
Covariates <- SpatialPointsDataFrame(covariateData[,Use], 
                                     data=covariateData[,Use], 
                                     proj4string = Projection)
# Scale the covariates
Covariates@data <- data.frame(apply(Covariates@data, 2, scale))  
summary(Covariates@data)
# spplot(Covariates, layout = c(4,1), col.regions=brewer.pal(6, "Blues")[-1], key.space="right", pretty=TRUE)

##### DATA #####
data_survey <- readRDS("Fish_status_survey_of_nordic_lakes/data/matched_cleaned.rds")
relevant <- data_survey %>% 
  dplyr::select(species, occurrenceStatus) %>% as.data.frame()
data_survey <- SpatialPointsDataFrame(coords = cbind(data_survey$decimalLatitude, data_survey$decimalLongitude), 
                                       data = relevant,
                                       proj4string = Projection)
# This map doesn't really work
MapCols <- c("blue", "red", "grey70")
names(MapCols) <- c("Survey, absent", "Survey, present", "Region")
par(mar=rep(0,4))
plot(norway.poly, col=MapCols["Region"])
points(data_survey, cex=0.7, pch=19, 
       col = MapCols[c("Survey, absent", "Survey, present")][1 + data_survey@data$occurrenceStatus])
legend(norway.poly@bbox["x","min"]+0.01*diff(norway.poly@bbox["x",]),
      norway.poly@bbox["y","min"]+0.95*diff(norway.poly@bbox["y",]),
      legend = names(MapCols), fill = MapCols, cex=0.8)

##### MESHGRID #####
# This chunk (stk.ip) failes if I use more than 2 covariates
Meshpars <- list(cutoff=0.08, max.edge=c(1,3), offset=c(1,1))
Mesh <- MakeSpatialRegion(data = NULL, bdry = norway.poly, 
                          meshpars = Meshpars, proj = Projection)
stk.ip <- MakeIntegrationStack(mesh = Mesh$mesh, data = Covariates, 
                               area = Mesh$w, tag = 'ip', InclCoords = TRUE)
plot(Mesh$mesh)

##### PREDICTIONS #####
Nxy.scale <- 0.5 # use this to change the resolution of the predictions
Nxy <- round(c(diff(norway.poly@bbox[1,]), diff(norway.poly@bbox[2,]))/Nxy.scale)
stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=Covariates, 
                               tag='pred',boundary=norway.poly)

##### STACKING AND MODELLING #####
stk.survey <- MakeBinomStack(observs = data_survey, 
                             data = Covariates, 
                             mesh = Mesh$mesh, 
                             presname = "occurrenceStatus",  
                             tag = "survey", 
                             InclCoords = TRUE) #polynoms = norway.poly,

NorwegianModel <- FitModel(stk.survey, stk.ip,
                              stk.pred$stk, CovNames=Use, mesh = Mesh$mesh,
                        predictions = TRUE, spat.ind = NULL)
NorwegianModel.summary <- summary(NorwegianModel$model)$fixed
save(NorwegianModel.summary, file = "ModelSummary.RData")

Pred <- SpatialPixelsDataFrame(points=stk.pred$predcoords, data=NorwegianModel$predictions, proj4string=Projection)
Pred@data$precision <- Pred@data$stddev^-2
Pred.plot <- plot(Pred)
save.image(file = "NorwegianModelWorkspace.RData")


