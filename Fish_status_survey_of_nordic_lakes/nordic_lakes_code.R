############################################################################
# Fish status survey of nordic lakes
############################################################################

library(ggplot2)
library(maps)
library(dplyr)
library(sf)
library(mapview)

data <- readRDS("Fish_status_survey_of_nordic_lakes/data/merged.rds")

#### LOOKING AT DATA ####
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

#### PLOT OBSERVATIONS #### 
# Load Norway-map
norway <- map_data("world", region = "Norway(?!:Svalbard)") 

ggplot(dataNO) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             alpha = 0.6, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2))) 


#### FINDING UNIQUE LOCATIONS ####
length(unique(dataNO$decimalLatitude))
length(unique(dataNO$decimalLongitude))
locations <- unique(dataNO[,c("decimalLatitude", "decimalLongitude")])

plot(locations)

ggplot(locations) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.6, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2))) 
# Now we need to match each of these 744 to a lake in our lake object
lakes <- readRDS("data/Norwegian_lakes.rds")

#### MATCHING TO LAKE ####
data_sf <- locations %>% 
  # Convert to sf object for easier handling. crs = Coordinate Reference System
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  # Transform coordinate system, using same system as in "lakes"
  sf::st_transform(st_crs(lakes)$epsg)

# Find closest lake
occ_with_lakes <- sf::st_join(data_sf, lakes, join = st_nearest_feature)

# Find distance to closest lake
index <- sf::st_nearest_feature(x = data_sf, y = lakes) # index of closest lake
closest_lakes <- lakes %>% slice(index) # slice based on the index
dist_to_lake <- sf::st_distance(x = data_sf, y = closest_lakes, by_element = TRUE) # get distance
occ_with_lakes$dist_to_lake <- as.numeric(dist_to_lake) # add the distance calculations to match data

distance_counts <- count(occ_with_lakes, dist_to_lake)

far_obs <- occ_with_lakes %>% filter(dist_to_lake > 0)
far_lakes <- closest_lakes %>% filter(waterBodyID %in% far_obs$waterBodyID)

# Super useful map! :
mapview(far_obs, zcol = "dist_to_lake", cex = "dist_to_lake") + 
  mapview(far_lakes, alpha = 0)


#### TROUT ####
trout <- occ_matched %>% filter(grepl('trutta', scientificName))

# Point map
ggplot(trout) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             alpha = 0.6, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2))) 


#### MODELLING ####

model1 <- glm(occurrenceStatus ~ decimalLatitude + decimalLongitude, 
              family = 'binomial', data = dataNO)

model2 <- glm(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
              family = 'binomial', data = trout)
summary(model2)



#### MODEL FITTING IN INLA ####
library(INLA)
formula = Response ~ Temperature(model="iid")
mod.sst = inla(formula,data=inlasst,family="binomial",Ntrials=n)

inla_mod <- inla(occurrenceStatus ~ decimalLatitude, family = "binomial", data = trout, Ntrials = n)

inla_mod <- inla(occurrenceStatus ~ decimalLongitude + decimalLatitude, 
     family = "binomial", data = trout, Ntrials = n)

class(trout$occurrenceStatus)
class(trout$decimalLatitude)
class(trout)
