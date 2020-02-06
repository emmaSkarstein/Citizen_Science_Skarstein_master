############################################################################
# Transcribed gillnet test fishing data
############################################################################


library(ggplot2)
library(maps)
library(dplyr)

data <- readRDS("Transcribed_gillnet_test_fishing_data_norway/data/merged.rds")

#----------------------------------------------------------------------------
# Exploring data
#----------------------------------------------------------------------------

year_counts <- count(data, year)

ggplot(year_counts, aes(x = year, y = n)) + 
  geom_bar(stat = "identity") + 
  theme_light() + 
  ylab("Number of observations")


norway <- map_data("world", region = "Norway(?!:Svalbard)") 

ggplot(data) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.6, size = 1, color = 'darksalmon') + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

ggplot(data) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="#2b2b2b", fill = "white") + 
  stat_binhex(aes(x = decimalLongitude, y = decimalLatitude)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

unique(data$occurrenceStatus)
# Presence only



