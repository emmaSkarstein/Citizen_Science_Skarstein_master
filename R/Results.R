# All exploration of model and results

library(ggpubr)
library(colorspace)


# Loading covariates and observations
source("R/loading_map_obs_covs.R")

# Model visualization functions
source("R/Model_visualization_functions.R")

# From "full_paralell_CV.R":
res_cv <- readRDS("R/output/cv_output_4mods_ov_F.RDS")

# Setting fish species
fish_sp <- "trout"

# From "full_model.R":
model_trout <- readRDS(paste0("R/output/model_", fish_sp, ".RDS"))

# Prediction stack:
stk.pred <- readRDS("R/output/stkpred.RDS")

if(fish_sp == "trout"){
  full_name <- "Brown trout"
}else if(fish_sp == "perch"){
  full_name <- "European perch"
}else if(fish_sp == "pike"){
  full_name <- "Arctic char"
}else{
  full_name <- "European pike"
}

#----------------------------------------------------------------------------




# OUTPUT FROM CROSS VALIDATION ----
res_cv %>% unlist() %>% matrix(ncol = 5) %>% rowMeans()




# RANDOM FIELDS ----
spat_fields_df <- proj_random_field(model_final$model, sp_polygon = norway.poly, 
                                    mesh = Mesh$mesh)
spat_fields <- spat_fields_df %>% tidyr::gather(key = statistic, value = value, mean:sd)

# New labels for statistics variable
statistic.labs <- c("Mean", "Standard deviation")
names(statistic.labs) <- c("mean", "sd")

# New labels for field variable
field.labs <- c("Second spatial field (PO data only)", "First spatial field (both data sets)")  
names(field.labs) <- c("bias", "shared")

ggplot(spat_fields) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value)) +
  scale_fill_viridis(option = "inferno", direction = -1) +
  facet_grid(rows = vars(statistic), cols = vars(field),
             labeller = labeller(field = field.labs, statistic = statistic.labs)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color="black", fill = NA) + coord_quickmap() + 
  theme_bw() +
  theme(axis.title = element_blank()) 

ggsave(paste0("figs/spatial_fields_", fish_sp, ".pdf"), width = 6, height = 5.3)




# DOTS AND WHISKERS PLOTS ----
dots_whiskers_inla(model_final)
ggsave(paste0("figs/coefficient_plot_", fish_sp, ".pdf")) 




# PREDICTIONS ----
Pred <- prediction_df(stk.pred = stk.pred, model = model_final)
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

ggsave(paste0("figs/posterior_predictions_", fish_sp, ".pdf"))

# Plotting intensity instead of log intensity (but what do we do with sd then?)

ggplot(Pred_mean) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = exp(value))) +
  #scale_fill_continuous_diverging(palette = "Purple-Green", limit = limit_mean)  +
  scale_fill_viridis(option = "inferno", direction = -1) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='black', fill = NA) + coord_quickmap() +
  theme_bw() + theme(axis.title = element_blank()) +
  ggtitle("Predicted posterior intensity (mean)")

# Plotting only mean with viridis
ggplot(Pred_mean) +
  geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = value), show.legend = FALSE) +
  scale_fill_viridis(option = "inferno", direction = -1) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color='black', fill = NA) + coord_quickmap() +
  labs(fill = element_blank()) + 
  theme_bw() + theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

ggsave("figs/minimal_prediction.png")

# POSTERIOR MARGINAL ----
shared <- dplyr::bind_rows(theta1 = as.data.frame(model_final$model$marginals.hyperpar[[1]]),
                                 theta2 = as.data.frame(model_final$model$marginals.hyperpar[[2]]),
                                .id = "theta")
bias <- dplyr::bind_rows(theta1 = as.data.frame(model_final$model$marginals.hyperpar[[3]]),
                               theta2 = as.data.frame(model_final$model$marginals.hyperpar[[4]]),
                               .id = "theta")
hyperpars <- dplyr::bind_rows(shared_field = shared, bias_field = bias, .id = "field")

ggplot(hyperpars) + 
  geom_line(aes(x = x, y = y, color = theta), lwd = 0.9) +
  ylab (expression(paste(pi, "(", theta, " | ", bold(y), ")"))) +
  facet_wrap(~field, nrow = 1) +
  scale_color_manual(values = c("magenta4", "orange")) +
  geom_vline(xintercept = 0, size = 0.9, linetype = "dotted", color = "grey") +
  theme_bw() + theme(aspect.ratio=1)
ggsave(paste0("figs/posterior_marginals_", fish_sp, ".pdf")) 


theta1_shared <- ggplot(hyperpars %>% filter(theta == "theta1", field == "shared_field")) + 
  geom_line(aes(x = x, y = y), color = "magenta4", lwd = 0.9) +
  ylab (expression(paste(pi, "(", theta, " | ", bold(y), ")"))) +
  ggtitle(expression(paste(theta[1]^{}, " - shared field"))) +
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())

theta2_shared <- ggplot(hyperpars %>% filter(theta == "theta2", field == "shared_field")) + 
  geom_line(aes(x = x, y = y), color = "orange", lwd = 0.9) +
  ylab ("") +
  ggtitle(expression(paste(theta[2]^{}, " - shared field"))) +
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())

theta1_bias <- ggplot(hyperpars %>% filter(theta == "theta1", field == "bias_field")) + 
  geom_line(aes(x = x, y = y), color = "magenta4", lwd = 0.9) +
  ylab ("") +
  ggtitle(expression(paste(theta[1]^{}, " - bias field"))) +
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())

theta2_bias <- ggplot(hyperpars %>% filter(theta == "theta2", field == "bias_field")) + 
  geom_line(aes(x = x, y = y), color = "orange", lwd = 0.9) +
  ylab ("") +
  ggtitle(expression(paste(theta[2]^{}, " - bias field"))) +
  theme_bw() + theme(aspect.ratio=1, legend.position = "none", axis.title.x = element_blank())


(theta1_shared | theta2_shared | theta1_bias | theta2_bias) +
  plot_annotation(title = paste0(full_name, ": hyperparameters")) &
  theme(plot.title = element_text(hjust = 0.5, size = 14))

ggsave(paste0("figs/thetas_", fish_sp, ".pdf"))

