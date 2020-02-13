############################################################################
# Fish status survey of nordic lakes
############################################################################

library(ggplot2)
library(maps)
library(dplyr)


data <- readRDS("Fish_status_survey_of_nordic_lakes/data/merged.rds")

# Looking at data
year_counts <- count(data, year) %>% filter(year>=1800)

ggplot(year_counts, aes(x = year, y = n)) + 
  geom_bar(stat = "identity", color="black") + 
  theme_light() + 
  ylab("Number of observations")


# We see that most of the observations are in 1996, so we select only these.

data96 <- data %>% filter(year==1996)

dataNO <- data96 %>% filter(countryCode == "NO")
dataSE <- data96 %>% filter(countryCode == "SE")
dataFI <- data96 %>% filter(countryCode == "FI")

test <- data96 %>% filter(grepl('trutta', scientificName))
length(unique(data96$waterBody))

##-----------------------------------------------------------------------------------
## Plot observations
##-----------------------------------------------------------------------------------
# Load Norway-map
norway <- map_data("world", region = "Norway(?!:Svalbard)") 

ggplot(dataNO) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             alpha = 0.6, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2))) 


##-----------------------------------------------------------------------------------
# Match each observation to closest lake
##-----------------------------------------------------------------------------------
source("R/match_to_lake.R")
occ_list <- match_to_lake(dataNO, lakes)

occ_matched <- occ_list[[1]]
occ_w_lakes <- occ_list[[2]]
saveRDS(occ_matched, "Fish_status_survey_of_nordic_lakes/data/occ_matched.rds")
saveRDS(occ_w_lakes, "Fish_status_survey_of_nordic_lakes/data/occ_w_lakes.rds")


occ_matched <- readRDS("Fish_status_survey_of_nordic_lakes/data/occ_matched.rds")
str(occ_matched)
occ_matched$waterBodyID <- as.factor(occ_matched$waterBodyID)

# How many lakes?
length(unique(occ_matched$waterBodyID))

##------------------------------------------------------------------------
trout <- occ_matched %>% filter(grepl('trutta', scientificName))

# Point map
ggplot(trout) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             alpha = 0.6, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2))) 


##-----------------------------------------------------------------------------------
# Modelling
##-----------------------------------------------------------------------------------


model1 <- glm(occurrenceStatus ~ decimalLatitude + decimalLongitude, 
              family = 'binomial', data = dataNO)

model2 <- glm(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
              family = 'binomial', data = trout)
summary(model2)



# Model fitting in INLA
library(INLA)
formula = Response ~ Temperature(model="iid")
mod.sst = inla(formula,data=inlasst,family="binomial",Ntrials=n)

inla_mod <- inla(occurrenceStatus ~ decimalLatitude, family = "binomial", data = trout, Ntrials = n)

inla_mod <- inla(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
     family = "binomial", data = trout, Ntrials = n)

class(trout$occurrenceStatus)
class(trout$decimalLatitude)
class(trout)
