############################################################################
# Fish status survey of nordic lakes
############################################################################

library(ggplot2)
library(maps)
library(dplyr)


# Load occurrence data and lakes
occ <- read.table("Fish_status_survey_of_nordic_lakes/data/occurrence.txt", header = TRUE, sep = "\t")
events <- read.table("Fish_status_survey_of_nordic_lakes/data/event.txt", header = TRUE, sep = "\t")
lakes <- readRDS("data/lake_polygons.rds")

colnames(occ)
colnames(events)
length(unique(events$eventID))
length(unique(occ$eventID))

data <- merge(occ, events, by = "eventID")

# Selecting only the variables we need
year_counts <- count(data, year) %>% filter(year>=1800)

ggplot(year_counts, aes(x = year, y = n)) + 
  geom_bar(stat = "identity", color="black") + 
  theme_light() + 
  ylab("Number of observations")



# We see that most of the observations are in 1996, so we select only these.

data96 <- data %>% filter(year==1996)

dataNO <- data96 %>% filter(countryCode = "NO")
dataSE <- data96 %>% filter(countryCode = "SE")
dataFI <- data96 %>% filter(countryCode = "FI")

test <- data96 %>% filter(grepl('trutta', scientificName))
length(unique(data96$waterBody))
##-----------------------------------------------------------------------------------
## Plot observations
##-----------------------------------------------------------------------------------
# Load Norway-map
map <- map_data("world", region = "Norway(?!:Svalbard)") 

theme_set(theme_light() + theme(aspect.ratio = .70))

ggplot(dataNO) +
  geom_map(data = map, map = map, aes(long, lat, map_id=region), 
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
saveRDS(occ_matched, "data/occ_matched.rds")
saveRDS(occ_w_lakes, "data/occ_w_lakes.rds")

str(occ_matched)
occ_matched$waterBodyID <- as.factor(occ_matched$waterBodyID)

# How many lakes?
length(unique(occ_matched$waterBodyID))

##------------------------------------------------------------------------
trout <- occ_matched %>% filter(grepl('trutta', scientificName))

ggplot(trout) +
  geom_map(data = map, map = map, aes(long, lat, map_id=region), 
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

inla_mod <- inla(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
     family = c('binomial'), data = trout)
