
library(sf)
library(sp)
library(ggplot2)
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

library(patchwork)
library(inlabru)
library(colorspace)

source("R/Model_visualization_functions.R")
source("R/loading_map_obs_covs.R")

# Setting fish species
fish_sp <- "trout"
#fish_sp <- "perch"
#fish_sp <- "char"
#fish_sp <- "pike"

if(fish_sp == "trout"){
  lat_name <- "Salmo trutta"
}else if(fish_sp == "perch"){
  lat_name <- "Perca fluviatilis"
}else if(fish_sp == "char"){
  lat_name <- "Salvelinus alpinus"
}else{
  lat_name <- "Esox lucius"
}


norway <- ggplot2::map_data("world", region = "Norway(?!:Svalbard)")
norway <- dplyr::setdiff(norway, filter(norway, subregion == "Jan Mayen"))

# Showing mesh
ggplot() + 
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color=NA, fill = NA) + coord_quickmap() + 
  gg(Mesh$mesh, int.color = "darkorange2", edge.color = "gray20") +
  theme_bw() +
  theme(axis.title = element_blank())


ggsave("figs/meshplot.pdf")



# Looking at the observations

## Observation map


p2 <- ggplot(data.frame(trout_artsobs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = "grey93") + coord_quickmap() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
             color = "darkorange2", size = 0.5) +
  theme_bw() +
  theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Artsobservasjoner")

p1 <- ggplot(data.frame(trout_survey)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = "grey93") + coord_quickmap() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             size = 0.5) +
  scale_color_manual(values=c("cyan4", "darkorange2"), labels = c("Absent", "Present")) + 
  guides(colour = guide_legend(override.aes = list(size=2)))+
  theme_bw() +
  theme(axis.title = element_blank(), legend.title = element_blank(), 
        legend.position = "left", axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) +
  ggtitle("Fish status survey")

hex2 <- ggplot(data.frame(trout_artsobs), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') + coord_quickmap() +
  geom_hex() +
  scale_fill_continuous_sequential(palette = "Teal") +
  theme_bw() +
  theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 

hex1 <- ggplot(data.frame(trout_survey), aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray93', fill = 'gray93') + coord_quickmap() +
  geom_hex() +
  scale_fill_continuous_sequential(palette = "Teal") +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = "left") 

(p1 + p2)/
  (hex1 + hex2)

ggsave(paste0("figs/pointhex_", fish_sp, ".pdf"), height = 6, width = 8)



# Hex map of all species
all_sp <- bind_rows(Trout = data.frame(trout_artsobs), Perch = data.frame(perch_artsobs), 
                    Char = data.frame(char_artsobs), Pike = data.frame(pike_artsobs), 
                    .id = "species")

ggplot(all_sp, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='gray80', fill = 'gray80') + coord_quickmap() +
  geom_hex() +
  #geom_polygon(data = norway, aes(long, lat, group = group), color = "black", fill  = NA) +
  scale_fill_continuous_sequential(palette = "Teal") +
  facet_wrap(vars(species), nrow = 1) +
  theme_bw() +
  theme(axis.title = element_blank(), strip.text.x = element_text(size = 14)) 

#ggsave("figs/hex_all_sp.pdf", width = 9, height = 3)
ggsave("figs/hex_all_sp.pdf", height = 4, width = 10)

# Point maps of survey all species
all_sp_survey <- bind_rows(Trout = data.frame(trout_survey), Perch = data.frame(perch_survey), 
                           Char = data.frame(char_survey), Pike = data.frame(pike_survey), 
                           .id = "species")

ggplot(all_sp_survey, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = "grey93") + coord_quickmap() +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = occurrenceStatus), 
             alpha = 0.8, size = 0.3) +
  scale_color_manual(values=c("cyan4", "darkorange2"), labels = c("Absent", "Present")) + 
  facet_wrap(vars(species), nrow = 1) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme_bw() +
  theme(axis.title = element_blank(), strip.text.x = element_text(size = 14), legend.title = element_blank()) 

ggsave("figs/points_all_sp.pdf", height = 4, width = 10)



## Number of observations per year
top_four <- Data_artsobs_df %>% filter(species %in% c("Perca fluviatilis", "Salmo trutta", "Salvelinus alpinus", "Esox lucius"))
time_counts <- dplyr::count(top_four, year, species)

ggplot(time_counts, aes(x = year, y = n, fill = species)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  scale_fill_viridis_d() + 
  geom_vline(xintercept = 1996, linetype = "dashed", color = "black", size = 1)

ggsave("figs/timeline.pdf", height = 2, width = 7)




# Explanatory variables

#Covariates <- readRDS("R/output/Covariates.RDS")
Cov_long <- tidyr::gather(data.frame(Covariates), key = variable, value = value, area_km2:log_catchment)


## Explanatory variables on a map
plot_exp_var <- function(var){
  ggplot(Cov_long %>% dplyr::select(variable, value, decimalLatitude, decimalLongitude) %>% 
           filter(variable==var)) +
    geom_polygon(data = norway, aes(long, lat, group = group), 
                 color = "grey", fill = "grey") + coord_quickmap() +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = value),
               alpha = 0.8, size = 0.2) +
    scale_color_continuous_sequential(palette = "OrRd") +
    facet_grid(cols = vars(variable), labeller = labeller(variable = cov.labs)) +
    theme_bw() +
    theme(axis.title = element_blank(), legend.title = element_blank(),
          strip.text.x = element_text(size = 14))
}

Cov_names <- unique(Cov_long$variable)
Use_p <- c("log_area", "log_catchment", "eurolst_bio10", "SCI", "log_perimeter",
           "distance_to_road", "HFP")
Use_interesting <- c("eurolst_bio10", "distance_to_road", "HFP")

cov.labs <- c("Log area", "Log catchment", "Temperature", 
              "Shoreline complexity index", "Log perimeter", 
              "Distance to road", "Human footprint index")
names(cov.labs) <- Use_p

var_plots <- lapply(Use_interesting, plot_exp_var)

(var_plots[[1]] + var_plots[[2]] + var_plots[[3]]) 


#ggsave("figs/covariates_on_map_interesting.pdf", height = 4, width = 10)
ggsave("figs/covariates_on_map_interesting.png", height = 4, width = 10)

## Explanatory variables histograms

ggplot(Cov_long %>% filter(variable %in% Use_p), aes(x = value)) +
  geom_histogram(fill = "cyan4", color = "cyan4") +
  facet_wrap(~variable, scales = 'free_x', nrow = 2, labeller = labeller(variable = cov.labs)) +
  theme_bw() + 
  theme(axis.title = element_blank())

ggsave("figs/covariate_histograms.pdf", width = 7, height = 4)







