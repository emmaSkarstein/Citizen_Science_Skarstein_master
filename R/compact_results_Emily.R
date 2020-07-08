
# Really compact results for Emily's presentation


# LIBRARIES
library(ggplot2)
library(sf)
library(sp)
library(ggpubr)
library(dplyr)
library(fishualize)
library(hexbin)
library(tidyr)
library(knitr)
library(summarytools)
library(pander)
library(maps)
library(maptools)
library(PointedSDMs)
library(viridis)


# DATA AND MODELS ----
mod <- readRDS("R/output/model_final.RDS")
dev_list <- readRDS("R/output/cv_output_4modse0.RDS")

source("R/Model_visualization_functions.R")
source("R/loading_map_obs_covs.R")

# VISUALIZE DATA ----

empty_theme_map <- theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  axis.text = element_blank()
)

norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- dplyr::setdiff(norway, filter(norway, subregion == "Jan Mayen"))

# POINTS ON HEX MAP ----
artsobs_hex <- ggplot(data.frame(trout_artsobs), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') +
  geom_hex() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.8, size = 1) +
  scale_fill_gradient(low = "#ecd0d0", high = "#433535") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle("Artsobservasjoner")

survey_hex <- ggplot(data.frame(trout_survey), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') +
  geom_hex() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, 
                 shape = occurrenceStatus, size = occurrenceStatus), 
             alpha = 0.8) +
  scale_shape_manual(values=c(1, 16), name = "", labels = c("Absence", "Presence")) +
  scale_size_manual(values = c(2, 1)) +
  guides(size = FALSE) +
  scale_fill_gradient(low = "#ecd0d0", high = "#433535") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle("Fish status survey of nordic lakes")

survey_hex + artsobs_hex
ggsave("figs/hex_point_maps.png", width = 8, height = 4)


# POINTS ONLY, MINIMAL FOR SCHEMATIC REPRESENTATION ----

ggplot(data.frame(trout_artsobs), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='black', fill = 'grey93') + coord_quickmap() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             alpha = 0.8, size = 1) +
  theme_bw() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
ggsave("minimal_points_artsobs.png")

ggplot(data.frame(trout_survey), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='black', fill = 'gray93') + coord_quickmap() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, 
                 color = occurrenceStatus), alpha = 0.8, show.legend = FALSE) +
  scale_color_manual(values=c("red", "black"), name = "", labels = c("Absence", "Presence")) +
  theme_bw() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) 
ggsave("minimal_points_survey.png")

#--------------
artsobs_hex <- ggplot(data.frame(trout_artsobs), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') + coord_quickmap() +
  geom_hex() +
  scale_fill_viridis(option = "magma", direction = -1) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle("Artsobservasjoner (citizen science observations)")

survey_hex <- ggplot(data.frame(trout_survey), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') + coord_quickmap() +
  geom_hex() +
  scale_fill_viridis(option = "magma", direction = -1) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle("Fish status survey of nordic lakes")

survey_hex + artsobs_hex
ggsave("figs/hex_maps.pdf", width = 12, height = 6)

artsobs_hex
ggsave("figs/artsobs_hex.png", width = 6, height = 6)

# MODEL RESULTS ----
dev_list %>% unlist() %>% matrix(ncol = 5) %>% rowMeans()

spat_fields_df <- proj_random_field(mod$model, sp_polygon = norway.poly, mesh = Mesh$mesh)

spat_fields <- spat_fields_df %>% gather(key = statistic, value = value, mean:sd)

# New labels for statistics variable
statistic.labs <- c("Mean", "Standard deviation")
names(statistic.labs) <- c("mean", "sd")

# New labels for field variable
field.labs <- c("Second spatial field (PO data only)", "First spatial field (both data sets)")  
names(field.labs) <- c("bias", "shared")

ggplot(spat_fields) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') +
  coord_quickmap() + 
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_viridis(option = "inferno", direction = -1) +
  facet_grid(rows = vars(statistic), cols = vars(field),
             labeller = labeller(field = field.labs, statistic = statistic.labs)) +
  theme_bw() +
  theme(axis.title = element_blank()) 

ggsave("figs/spatial_fields.png", width = 6, height = 5.3)

summary(mod$model)

# Prediction
pred_df <- data.frame(stk.pred$predcoords, prediction = mod$predictions) %>% 
  gather(key = statistic, value = value, prediction.mean:prediction.stddev)
ggplot(pred_df) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') +
  coord_quickmap() + 
  geom_raster(aes(x = X, y = Y, fill = value)) +
  scale_fill_viridis(option = "inferno", direction = -1)  +
  facet_grid(~statistic) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  ggtitle("Prediction")

ggplot(filter(pred_df, statistic == "prediction.mean")) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') +
  coord_quickmap() + 
  geom_raster(aes(x = X, y = Y, fill = value)) +
  scale_fill_viridis(option = "inferno", direction = -1)  +
  theme_bw() +
  theme(axis.title = element_blank()) +
  ggtitle("Prediction")



