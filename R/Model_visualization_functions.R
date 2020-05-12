


PlotMeanStddev <- function(pred){
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


PlotSpatialFields <- function(fitmod, biasfield = TRUE){
  random_sp_effects <- data.frame(cbind(fitmod$model$summary.random$unstr_field$mean,
                                        fitmod$model$summary.random$unstr_field$sd,
                                        Mesh$mesh$loc[,1:2]))
  
  if(biasfield){
    bias_stuff <- data.frame(cbind(fitmod$model$summary.random$bias_field$mean,
                                  fitmod$model$summary.random$bias_field$sd))
    random_sp_effects <- cbind(bias_stuff, random_sp_effects)
  }
  
  column_names <- c("shared_field", "sf_sd", "longitude", "latitude")
  if(biasfield){
    column_names <- c("bias_field", "bf_sd", column_names)
  }
  
  colnames(random_sp_effects) <- column_names
  
  p <- ggplot(random_sp_effects) +
    geom_map(data = norway, map = norway, aes(long, lat, map_id=region), 
             color="#2b2b2b", fill = "white") + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())
  
  p1 <- p + geom_point(aes(x = longitude, y = latitude, color = shared_field), alpha = 0.6) +
    scale_color_viridis()
  p1.1 <- p + geom_point(aes(x = longitude, y = latitude, color = sf_sd), alpha = 0.6) +
    scale_color_viridis()
  
  if(biasfield){
    p2 <- p + geom_point(aes(x = longitude, y = latitude, color = bias_field), alpha = 0.6) +
      scale_color_viridis()
    p2.1 <- p + geom_point(aes(x = longitude, y = latitude, color = bf_sd), alpha = 0.6) +
      scale_color_viridis()
  }
  
  if(biasfield){
    figure <- ggarrange(p1, p1.1, p2, p2.1, ncol = 2, nrow = 2)
    return(figure)
  }
  
  figure <- ggarrange(p1, p1.1, ncol = 2, nrow = 1)
  return(figure)
}

plot_obs <- function(dataset){
  nosefi <- map_data("world", region = c("Norway(?!:Svalbard)", 
                                         "Sweden", "Finland")) 
  p <- ggplot(dataset) +
    geom_polygon(data = nosefi, aes(long, lat, group = group), 
                 color="#2b2b2b", fill = "white") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), 
               color = 'hotpink4', alpha = 0.6, size = 0.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    ggtitle(deparse(substitute(dataset)))
  return(p)
}

