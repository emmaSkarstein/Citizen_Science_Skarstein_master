##-------------------------------------------------------------------------------
# Exploring data from Artsobservasjoner and Artobservasjoner
##-------------------------------------------------------------------------------
library(ggplot2)


artsobsNO <- readRDS('Nordic_Species_Observation_Services/data/GBIF_download_Norge.rds')

# First we can remove the columns that don't have any information
not_all_na <- function(x) any(!is.na(x))
artsobsNO <- artsobsNO %>% select_if(not_all_na)

##-------------------------------------------------------------------------------
# Preliminary plots
##-------------------------------------------------------------------------------
occ <- Data_artsobs_df
plot(Data_artsobs)

# Bar plot of different species
species_counts <- count(occ, vars = species, sort = TRUE)

ggplot(species_counts[1:12,], aes(x = vars, y = n, fill = vars)) + 
  geom_bar(stat = "identity", color="black") + 
  geom_text(aes(label = n), vjust=-0.3, color="black", size=4) +
  theme_light() + 
  ylab("Number of observations") +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

# Bar plot of years
time_counts_recent <- count(occ, year)

ggplot(time_counts_recent, aes(x = year, y = n)) + 
  geom_bar(stat = "identity", color="black", position = position_stack(reverse = FALSE)) + 
  theme_light() + 
  ylab("Number of observations")

fylke_counts <- count(occ, county.x)

ggplot(fylke_counts, aes(x = county.x, y = n)) + 
  geom_bar(stat = "identity", color="black", position = position_stack(reverse = FALSE)) + 
  theme_light() + 
  ylab("Number of observations")+
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

##-------------------------------------------------------------------------------
# Looking only at Salmo trutta
##-------------------------------------------------------------------------------

trutta <- filter(occ, species == "Salmo trutta")

trutta_count <- count(trutta, year)
ggplot(trutta_count, aes(x = year, y = n)) + 
  geom_bar(stat = "identity", color="black", position = position_stack(reverse = FALSE)) + 
  theme_light() + 
  ylab("Number of observations")

# On the map:
norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)") 
# Set theme for blue ocean
theme_set(theme_light() + theme(aspect.ratio = .70, panel.background = element_rect(fill = "aliceblue")))

ggplot(trutta) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.6, size = 1, color = 'darkslategray4') + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

ggplot(trutta) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
           color="#2b2b2b", fill = "white") + 
  stat_binhex(aes(x = decimalLongitude, y = decimalLatitude)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())


#--------------------------------------------------------------------------
# Who made these observations?
#--------------------------------------------------------------------------

recordedBy_count <- count(occ, recordedBy, sort = TRUE)
institutionCode_count <- count(occ, institutionCode, sort = TRUE)
collectionCode_count <- count(occ, collectionCode, sort = TRUE)
datasetName_count <- count(occ, datasetName, sort = TRUE)

head(recordedBy_count)
institutionCode_count
collectionCode_count
datasetName_count

olehakon <- filter(occ, recordedBy == "Ole-HÃ¥kon Heier")

ggplot(olehakon) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.6, size = 1, color = 'darkslategray4') + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())
