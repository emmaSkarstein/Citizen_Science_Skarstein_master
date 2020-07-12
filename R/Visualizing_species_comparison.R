# Comparing species

# Loading covariates and observations
source("R/loading_map_obs_covs.R")

# Model visualization functions
source("R/Model_visualization_functions.R")

# From "full_paralell_CV.R":
res_cv <- readRDS("R/output/cv_output_4modse0.RDS")


library(patchwork)
library(RColorBrewer)
#library(ggtheme)


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

limit_mean <- max(abs(Pred %>% filter(statistic == "mean") %>% dplyr::select(value))) * c(-1, 1)
limit_sd <- max(abs(Pred %>% filter(statistic == "stddev") %>% dplyr::select(value))) * c(0, 1)

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
  ggtitle(expression(paste("First spatial field, ", xi[1]))) 


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
  ggtitle(expression(paste("Effort field, ", xi[2])))

shared_all_sp + bias_all_sp

ggsave("figs/spatial_fields_all_species.pdf", width = 6.6, height = 10)

# PLOTTING HYPERPARAMETERS ----

hyperpar_df <- function(model){
  shared <- dplyr::bind_rows(rho = as.data.frame(model$model$marginals.hyperpar[[1]]),
                             sigma = as.data.frame(model$model$marginals.hyperpar[[2]]),
                             .id = "hyperpar")
  bias <- dplyr::bind_rows(rho = as.data.frame(model$model$marginals.hyperpar[[3]]),
                           sigma = as.data.frame(model$model$marginals.hyperpar[[4]]),
                           .id = "hyperpar")
  hyperpars <- dplyr::bind_rows(shared_field = shared, bias_field = bias, .id = "field")
  hyperpars
}

trout_hyperpars <- hyperpar_df(model_trout)
perch_hyperpars <- hyperpar_df(model_perch)
char_hyperpars <- hyperpar_df(model_char)
pike_hyperpars <- hyperpar_df(model_pike)

all_sp_hyperpar <- dplyr::bind_rows(trout = trout_hyperpars, 
                                    perch = perch_hyperpars, 
                                    char = char_hyperpars, 
                                    pike = pike_hyperpars,
                                    .id = "species") 


rho_shared <- ggplot(all_sp_hyperpar %>% filter(hyperpar == "rho", field == "shared_field")) + 
  geom_line(aes(x = x, y = y, color = species, linetype = species), lwd = 0.8) +
  ylab (expression(paste(pi, "(", rho, " | ", bold(y), ")"))) +
  ggtitle(expression(paste(rho, " - shared field"))) +
  xlim(0, 50) +
  scale_color_viridis_d() +
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

sigma_shared <- ggplot(all_sp_hyperpar %>% filter(hyperpar == "sigma", field == "shared_field")) + 
  geom_line(aes(x = x, y = y, color = species, linetype = species), lwd = 0.8) +
  ylab (expression(paste(pi, "(", sigma, " | ", bold(y), ")"))) +
  ggtitle(expression(paste(sigma, " - shared field"))) +
  xlim(0, 0.9) +
  scale_color_viridis_d() + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

rho_bias <- ggplot(all_sp_hyperpar %>% filter(hyperpar == "rho", field == "bias_field")) + 
  geom_line(aes(x = x, y = y, color = species, linetype = species), lwd = 0.8) +
  ylab("")+
  ggtitle(expression(paste(rho, " - bias field"))) +
  xlim(0, 50) +
  scale_color_viridis_d()+ 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

sigma_bias <- ggplot(all_sp_hyperpar %>% filter(hyperpar == "sigma", field == "bias_field")) + 
  geom_line(aes(x = x, y = y, color = species, linetype = species), lwd = 0.8) +
  ylab ("") +
  ggtitle(expression(paste(sigma, " - bias field"))) +
  xlim(0, 0.9) +
  scale_color_viridis_d() + 
  theme_bw() + theme(aspect.ratio=1, axis.title.x = element_blank(), 
                     plot.title = element_text(size = 11))

((rho_shared | rho_bias)/(sigma_shared | sigma_bias))

ggsave("figs/thetas_all_species.pdf", width = 5, height = 4)




# DOTS AND WHISKERS PLOTS ----
Use <- c("decimalLongitude","decimalLatitude", "log_area", 
         "log_catchment", "eurolst_bio10", "SCI")
cov.labs <- c("Longitude", "Latitude", "Log area", "Log catchment", 
              "Temperature", "Shoreline complexity index")

dots_whiskers_inla(model_trout) + scale_y_discrete(labels = cov.labs) +
  ggtitle(paste0("Brown trout"))
ggsave(paste0("figs/coefficient_plot_", "trout", ".pdf"), height = 2.5, width = 5) 

dots_whiskers_inla(model_perch) + scale_y_discrete(labels = cov.labs) +
  ggtitle(paste0("European perch"))
ggsave(paste0("figs/coefficient_plot_", "perch", ".pdf"), height = 2.5, width = 5)

dots_whiskers_inla(model_char) + scale_y_discrete(labels = cov.labs) +
  ggtitle(paste0("Arctic char"))
ggsave(paste0("figs/coefficient_plot_", "char", ".pdf"), height = 2.5, width = 5)

dots_whiskers_inla(model_pike) + scale_y_discrete(labels = cov.labs) +
  ggtitle(paste0("Northern pike"))
ggsave(paste0("figs/coefficient_plot_", "pike", ".pdf"), height = 2.5, width = 5)



