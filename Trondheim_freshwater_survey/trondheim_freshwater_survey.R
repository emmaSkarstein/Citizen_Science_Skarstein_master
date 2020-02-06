############################################################################
# Trondheim freshwater survey data
############################################################################


library(ggplot2)
library(maps)
library(dplyr)

#----------------------------------------------------------------------------
# Data preparation
#----------------------------------------------------------------------------

# Load occurrence data and lakes
data <- read.table("Trondheim_freshwater_survey/data/occurrence.txt", header = TRUE, sep = "\t")
#lakes <- readRDS("data/lake_polygons.rds")


#----------------------------------------------------------------------------
# Exploring data
#----------------------------------------------------------------------------

year_counts <- count(data, year) 
print(year_counts)
# All observations were made in 2014

length(unique(data$waterBody))
# 18 lakes


library(ggmap)
trond_loc = c(10.1, 63.34, 10.5, 63.46)
myMap <- get_map(location=trond_loc,
                 source="stamen", maptype="watercolor", crop=FALSE)
ggmap(myMap) +
  geom_point(data = data, aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.6, size = 1, color = 'red')



