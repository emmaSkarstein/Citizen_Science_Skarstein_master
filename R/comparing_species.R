# Comparing species

# Loading covariates and observations
source("R/loading_map_obs_covs.R")

# Model visualization functions
source("R/Model_visualization_functions.R")

# From "full_paralell_CV.R":
res_cv <- readRDS("R/output/cv_output_4modse0.RDS")


library(patchwork)


# From "full_model.R":
model_trout <- readRDS("R/output/model_trout.RDS")
model_perch <- readRDS("R/output/model_perch.RDS")
model_char <- readRDS("R/output/model_char.RDS")
model_pike <- readRDS("R/output/model_pike.RDS")

# Prediction stack:
stk.pred <- readRDS("R/output/stkpred.RDS")


# ------------------------------------------

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

# New labels for statistics variable
statistic.labs <- c("Mean", "Standard deviation")
names(statistic.labs) <- c("mean", "sd")

# New labels for field variable
field.labs <- c("Second spatial field (PO data only)", "First spatial field (both data sets)")  
names(field.labs) <- c("bias", "shared")

# New labels for field variable
species.labs <- c("Brown trout", "European perch", "Arctic char", "Northern pike")  
names(species.labs) <- c("Trout", "Perch", "Char", "Pike")


shared_all_sp <- ggplot(spat_fields %>% filter(statistic == "mean", field == "shared")) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_viridis(option = "magma", direction = -1) +
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
  scale_fill_viridis(option = "viridis", direction = -1) +
  facet_grid(rows = vars(species), 
             labeller = labeller(species = species.labs, field = field.labs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), legend.title = element_blank()) + 
  ggtitle("Second spatial field", subtitle = "Citizen science data only")

shared_all_sp + bias_all_sp & theme(plot.margin = margin())

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

ggplot(all_sp_hyperpar) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.5, lwd = 0.9) +
  ylab (expression(paste(pi, "(", theta, " | ", bold(y), ")"))) +
  facet_grid(vars(field, theta), scales = "free") +
  scale_color_manual(values = c("magenta4", "orange", "green")) +
  theme_bw() + theme(aspect.ratio=1)



theta1_shared <- ggplot(all_sp_hyperpar %>% filter(theta == "theta1", field == "shared_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab (expression(paste(pi, "(", theta, " | ", bold(y), ")"))) +
  ggtitle(expression(paste(theta[1]^{}, " - shared field"))) +
  #facet_wrap(~species, nrow = 3) +
  scale_color_manual(values = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())

theta2_shared <- ggplot(all_sp_hyperpar %>% filter(theta == "theta2", field == "shared_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab ("") +
  ggtitle(expression(paste(theta[2]^{}, " - shared field"))) +
  scale_color_manual(values = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())

theta1_bias <- ggplot(all_sp_hyperpar %>% filter(theta == "theta1", field == "bias_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab ("") +
  ggtitle(expression(paste(theta[1]^{}, " - bias field"))) +
  scale_color_manual(values = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) + 
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())

theta2_bias <- ggplot(all_sp_hyperpar %>% filter(theta == "theta2", field == "bias_field")) + 
  geom_line(aes(x = x, y = y, color = species), alpha = 0.8, lwd = 0.8) +
  ylab ("") +
  ggtitle(expression(paste(theta[2]^{}, " - bias field"))) +
  scale_color_manual(values = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) + 
  theme_bw() + theme(aspect.ratio=1, axis.title.x = element_blank())


(theta1_shared | theta2_shared | theta1_bias | theta2_bias) +
  plot_annotation(title = "All species, hyperparameters") &
  theme(plot.title = element_text(hjust = 0.5, size = 14))

ggsave("figs/thetas_all_species.pdf", height = 3.2)
