plot.mean.stddev <- function(pred){
  ncolors <- 200
  greencols.fn <- colorRampPalette(brewer.pal(9, "Greens"))
  greencols <- greencols.fn(ncolors)
  bluecols.fn <- colorRampPalette(brewer.pal(9, "Blues"))
  bluecols <- bluecols.fn(ncolors)
  map.mean <- mapview::mapview(pred, zcol = c("mean"), legend = TRUE,
                               col.regions = greencols)
  map.stddev <- mapview::mapview(pred, zcol = c("stddev"), legend = TRUE, alpha = 0.3, 
                                 col.regions = bluecols)
  leafsync::sync(map.mean, map.stddev)
}



# To plot the bias field:
random_sp_effects <- data.frame(cbind(NorwegianModel$model$summary.random$bias_field$mean,
                                      NorwegianModel$model$summary.random$bias_field$sd,
                                      NorwegianModel$model$summary.random$unstr_field$mean,
                                      NorwegianModel$model$summary.random$unstr_field$sd,
                                      Mesh$mesh$loc[,1:2]))
colnames(random_sp_effects) <- c("bias_field", "bf_sd", "shared_field", "sf_sd", "longitude", "latitude")

# bias_map_sp <- SpatialPointsDataFrame(coords = cbind(bias_map["longitude"], bias_map["latitude"]),
#                                    data = bias_map["bias_field"], 
#                                    proj4string = Projection)
# mapview::mapview(bias_map_sp, zcol = c("bias_field"), legend = TRUE,
#                              col.regions = greencols)

random_sp_effects_long <- tidyr::gather(random_sp_effects, field_type, value, bias_field:sf_sd, factor_key = TRUE)

ggplot(random_sp_effects_long) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  geom_point(aes(x = longitude, y = latitude, color = value), alpha = 0.6) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_color_viridis() +
  facet_wrap(field_type ~ .)

p <- ggplot(random_sp_effects) +
  geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
           color="#2b2b2b", fill = "white") + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())


p1 <- p + geom_point(aes(x = longitude, y = latitude, color = bias_field), alpha = 0.6) +
  scale_color_viridis()
p1.1 <- p + geom_point(aes(x = longitude, y = latitude, color = bf_sd), alpha = 0.6) +
  scale_color_viridis()
p2 <- p + geom_point(aes(x = longitude, y = latitude, color = shared_field), alpha = 0.6) +
  scale_color_viridis()
p2.1 <- p + geom_point(aes(x = longitude, y = latitude, color = sf_sd), alpha = 0.6) +
  scale_color_viridis()

figure1 <- ggarrange(p1, p1.1, p2, p2.1, ncol = 2, nrow = 2)
figure2 <- ggarrange(p1, p2, ncol = 2)


p1
figure1

NorwegianModel$model$dic

