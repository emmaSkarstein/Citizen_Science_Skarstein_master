################################################################################
# SETTING UP MODELS
################################################################################


library(ggplot2)
library(maps)
library(dplyr)
library(tidyverse)
library(sf)

occ_matched <- readRDS("Fish_status_survey_of_nordic_lakes/data/occ_matched.rds")

# How many lakes?
length(unique(occ_matched$waterBodyID))

##------------------------------------------------------------------------
trout <- occ_matched %>% filter(grepl('trutta', scientificName))

norway <- map_data("world", region = c("Norway(?!:Svalbard)"))

# Point map
ggplot(trout) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             alpha = 0.6, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2))) 


##-----------------------------------------------------------------------------------
# glm base R model
##-----------------------------------------------------------------------------------

model1 <- glm(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
              family = 'binomial', data = trout)
summary(model1)


##-----------------------------------------------------------------------------------
# Model fitting in INLA
##-----------------------------------------------------------------------------------

library(INLA)
# Apparently INLA needs response to be numeric 0/1 instead of factor.
trout$occurrenceStatus <- as.numeric(trout$occurrenceStatus) - 1

inla_mod <- inla(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
                 family = "binomial", data = trout, verbose = TRUE)

summary(inla_mod)

##-----------------------------------------------------------------------------------
# Environmental covariates
##-----------------------------------------------------------------------------------
environmental_covariates <- readRDS("data/environmental_covariates.RDS")

# Merge environmental covariates with observational data
trout_env <- merge(trout, environmental_covariates, by = "waterBodyID")
class(trout_env$occurrenceStatus)

# Basic INLA model with env. cov
inla_mod2 <- inla(occurrenceStatus ~ area_km2 + perimeter_m + HFP + distance_to_road,
                  family = "binomial", data = trout_env, verbose = TRUE)
summary(inla_mod2)
