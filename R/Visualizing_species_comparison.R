# Comparing species

# Loading covariates and observations
source("R/loading_map_obs_covs.R")

# Model visualization functions
source("R/Model_visualization_functions.R")

# From "full_paralell_CV.R":
res_cv <- readRDS("R/output/cv_output_4modse0.RDS")


library(patchwork)
library(RColorBrewer)


# From "full_model.R":
model_trout <- readRDS("R/output/model_trout.RDS")
model_perch <- readRDS("R/output/model_perch.RDS")
model_char <- readRDS("R/output/model_char.RDS")
model_pike <- readRDS("R/output/model_pike.RDS")

# Prediction stack:
stk.pred <- readRDS("R/output/stkpred.RDS")


# ------------------------------------------

# PLOTTING POSTERIOR PREDICTIONS ----
Pred_trout <- prediction_df(stk.pred = stk.pred, model = model_trout)
Pred_perch <- prediction_df(stk.pred = stk.pred, model = model_perch)
Pred_char <- prediction_df(stk.pred = stk.pred, model = model_char)
Pred_pike <- prediction_df(stk.pred = stk.pred, model = model_pike)

Pred <- bind_rows(Trout = Pred_trout, Perch = Pred_perch, 
                         Char = Pred_char, Pike = Pred_pike, .id = "species")

# New labels for species variable
species.labs <- c("Brown trout", "European perch", "Arctic char", "Northern pike")  
names(species.labs) <- c("Trout", "Perch", "Char", "Pike")

limit_mean <- max(abs(Pred %>% filter(statistic == "mean") %>% select(value))) * c(-1, 1)
limit_sd <- max(abs(Pred %>% filter(statistic == "stddev") %>% select(value))) * c(0, 1)

mean_p <- ggplot(Pred %>% filter(statistic == "mean")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_continuous_sequential(palette = "PuBuGn") +
  facet_grid(rows = vars(species), 
             labeller = labeller(species = species.labs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = "left",
        strip.background = element_blank(), strip.text.y = element_blank(),
        legend.title = element_blank()) +
  ggtitle("Mean")


sd_p <- ggplot(Pred %>% filter(statistic == "stddev")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_continuous_sequential(palette = "Reds 3")  +
  facet_grid(rows = vars(species), 
             labeller = labeller(species = species.labs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), legend.title = element_blank()) + 
  ggtitle("Standard deviation")

mean_p + sd_p

ggsave("figs/posterior_predictions_all_species.pdf", width = 6.6, height = 10)




# PLOTTING SPATIAL FIELDS ----
spat_fields_trout <- make_random_field_df(model_trout, sp_polygon = norway.poly, 
                                    mesh = Mesh$mesh)
spat_fields_perch <- make_random_field_df(model_perch, sp_polygon = norway.poly, 
                                          mesh = Mesh$mesh)
spat_fields_char <- make_random_field_df(model_char, sp_polygon = norway.poly, 
                                          mesh = Mesh$mesh)
spat_fields_pike <- make_random_field_df(model_pike, sp_polygon = norway.poly, 
                                          mesh = Mesh$mesh)
spat_fields <- bind_rows(Trout = spat_fields_trout, Perch = spat_fields_perch, 
                         Char = spat_fields_char, Pike = spat_fields_pike, .id = "species")



# New labels for field variable
field.labs <- c("Second spatial field (PO data only)", "First spatial field (both data sets)")  
names(field.labs) <- c("bias", "shared")

# New labels for species variable
species.labs <- c("Brown trout", "European perch", "Arctic char", "Northern pike")  
names(species.labs) <- c("Trout", "Perch", "Char", "Pike")


shared_all_sp <- ggplot(spat_fields %>% filter(statistic == "mean", field == "shared")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  #scale_fill_viridis(option = "magma", direction = -1) +
  scale_fill_continuous_sequential(palette = "BuPu")  +
  facet_grid(rows = vars(species), 
             labeller = labeller(species = species.labs, field = field.labs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = "left",
        strip.background = element_blank(), strip.text.y = element_blank(),
        legend.title = element_blank()) +
  ggtitle("First spatial field", subtitle = "Shared for both data sets")


bias_all_sp <- ggplot(spat_fields %>% filter(statistic == "mean", field == "bias")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  #scale_fill_viridis(option = "viridis", direction = -1) +
  scale_fill_continuous_sequential(palette = "GnBu")  +
  facet_grid(rows = vars(species), 
             labeller = labeller(species = species.labs, field = field.labs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), legend.title = element_blank()) + 
  ggtitle("Second spatial field", subtitle = "Citizen science data only")

shared_all_sp + bias_all_sp

ggsave("figs/spatial_fields_all_species.pdf", width = 6.6, height = 11)

# PLOTTING HYPERPARAMETERS ----

hyperpar_df <- function(model){
  shared <- dplyr::bind_rows(theta1 = as.data.frame(model$model$marginals.hyperpar[[1]]),
                             theta2 = as.data.frame(model$model$marginals.hyperpar[[2]]),
                             .id = "theta")
  bias <- dplyr::bind_rows(theta1 = as.data.frame(model$model$marginals.hyperpar[[3]]),
                           theta2 = as.data.frame(model$model$marginals.hyperpar[[4]]),
                           .id = "theta")
  hyperpars <- dplyr::bind_rows(shared_field = shared, bias_field = bias, .id = "field")
  hyperpars
}

trout_theta <- hyperpar_df(model_trout)
perch_theta <- hyperpar_df(model_perch)
char_theta <- hyperpar_df(model_char)
pike_theta <- hyperpar_df(model_pike)

all_sp_hyperpar <- dplyr::bind_rows(trout = trout_theta, 
                                    perch = perch_theta, 
                                    char = char_theta, 
                                    pike = pike_theta,
                                    .id = "species") 


theta1_shared <- ggplot(all_sp_hyperpar %>% filter(theta == "theta1", field == "shared_field")) + 
  geom_line(aes(x = x, y = y, color = species), lwd = 0.8) +
  ylab (expression(paste(pi, "(", theta, " | ", bold(y), ")"))) +
  ggtitle(expression(paste(theta[1]^{}, " - shared field"))) +
  xlim(0, 70) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

theta2_shared <- ggplot(all_sp_hyperpar %>% filter(theta == "theta2", field == "shared_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab ("") +
  ggtitle(expression(paste(theta[2]^{}, " - shared field"))) +
  xlim(0, 0.7) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

theta1_bias <- ggplot(all_sp_hyperpar %>% filter(theta == "theta1", field == "bias_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab (expression(paste(pi, "(", theta, " | ", bold(y), ")"))) +
  ggtitle(expression(paste(theta[1]^{}, " - bias field"))) +
  xlim(0, 70) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

theta2_bias <- ggplot(all_sp_hyperpar %>% filter(theta == "theta2", field == "bias_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab ("") +
  ggtitle(expression(paste(theta[2]^{}, " - bias field"))) +
  xlim(0, 0.7) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + theme(aspect.ratio=1, axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))


((theta1_shared | theta2_shared)/(theta1_bias | theta2_bias))

ggsave("figs/thetas_all_species.pdf", width = 5, height = 4)



# COMPARING DIFFERENT RANGES ----

model_lowrange <- readRDS("R/output/model_trout_lowrange.RDS")
model_10range <- readRDS("R/output/model_trout_10range.RDS")
model_100range <- readRDS("R/output/model_trout_100range.RDS")

spat_fields_lowrange <- make_random_field_df(model_lowrange, sp_polygon = norway.poly, 
                                          mesh = Mesh$mesh)
spat_fields_10range <- make_random_field_df(model_10range, sp_polygon = norway.poly, 
                                          mesh = Mesh$mesh)
spat_fields_100range <- make_random_field_df(model_100range, sp_polygon = norway.poly, 
                                         mesh = Mesh$mesh)

spat_fields <- bind_rows(lowrange = spat_fields_lowrange, midrange = spat_fields_10range, 
                         highrange = spat_fields_100range, .id = "range")


shared_all_sp <- ggplot(spat_fields %>% filter(statistic == "mean", field == "shared")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_viridis(option = "magma", direction = -1) +
  facet_grid(rows = vars(range)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = "left",
        strip.background = element_blank(), strip.text.y = element_blank(),
        legend.title = element_blank()) +
  ggtitle("First spatial field", subtitle = "Shared for both data sets")


bias_all_sp <- ggplot(spat_fields %>% filter(statistic == "sd", field == "shared")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_viridis(option = "viridis", direction = -1) +
  facet_grid(rows = vars(range)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), legend.title = element_blank()) + 
  ggtitle("first spatial field", subtitle = "standard deviation")

shared_all_sp + bias_all_sp & theme(plot.margin = margin())


# ----
Pred <- prediction_df(stk.pred = stk.pred, model = model_100range)
Pred_mean <- Pred %>% filter(statistic=="mean")
Pred_sd <- Pred %>% filter(statistic=="stddev")
limit_mean <- max(abs(Pred_mean$value)) * c(-1, 1)
limit_sd <- max(abs(Pred_sd$value)) * c(0, 1)

p_mean <- ggplot(Pred_mean) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_continuous_diverging(palette = "Purple-Green", limit = limit_mean)  +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='black', fill = NA) + coord_quickmap() +
  ggtitle(label = "Mean") +
  labs(fill = element_blank()) + 
  theme_bw() + theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5))

p_sd <- ggplot(Pred_sd) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_continuous_diverging(palette = "Purple-Green", limit = limit_sd)  +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='black', fill = NA) + coord_quickmap() + 
  ggtitle(label = "Standard deviation") +
  labs(fill = element_blank()) +
  theme_bw() + theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5))

p_post <- ggarrange(p_mean, p_sd)
annotate_figure(p_post, top = text_grob(paste0(full_name, ": Predicted posterior log intensity"), 
                                        size = 14))


