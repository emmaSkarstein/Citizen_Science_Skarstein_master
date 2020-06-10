# Data prep for various visualizations

source("R/loading_map_obs_covs.R")

source("R/Model_visualization_functions.R")

# Saving the cleaned data in order to load it quickly into the Rmarkdown document
saveRDS(trout_artsobs, "R/output/trout_artsobs.RDS")
saveRDS(trout_survey, "R/output/trout_survey.RDS")
saveRDS(trout_artsobs_df, "R/output/trout_artsobs_df.RDS")
saveRDS(trout_survey_df, "R/output/trout_survey_df.RDS")
saveRDS(Covariates, "R/output/Covariates.RDS")

norway <- readRDS("R/gadm36_NOR_0_sf.rds")
plot(norway$geometry)
plot(trout_artsobs, pch = 20, cex = 0.5, col = 'hotpink', add = TRUE)

par(mfrow = c(1,2))
norway <- readRDS("R/gadm36_NOR_0_sf.rds")
plot(norway$geometry)
plot(trout_artsobs, pch = 20, cex = 0.5, col = 'hotpink', add = TRUE)

plot(norway$geometry)
plot(trout_survey, pch = 20, cex = 0.5, col = 'hotpink', add = TRUE)

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

#font_add_google(name = "Pragati Narrow", family = "pragati-narrow")




model_final <- readRDS("R/output/model_final_1.RDS")



spat_fields_df <- proj_random_field(model_final$model, sp_polygon = norway.poly, mesh = Mesh$mesh)

ggplot(drop_na(spat_fields_df)) +
  geom_polygon(data = norway, aes(long, lat, group = group), 
               color=trutta_colors[11], fill = trutta_colors[11]) +
    geom_raster(aes(x = decimalLongitude, y = decimalLatitude, fill = mean)) +
  scale_fill_fish(option = "Salmo_trutta", begin = 0.52, end = 0.9) +
  empty_theme_map








r0 <- diff(range(bbox(norway.poly)[1,]))/diff(range(bbox(norway.poly)[2,]))
prj <- inla.mesh.projector(Mesh$mesh, xlim=bbox(norway.poly)[1,],
                           ylim=bbox(norway.poly)[2,],
                           dims=c(200*r0, 200))

m.spat <- inla.mesh.project(prj, model_final$model$summary.ran$bias_field$mean)
sd.spat <- inla.mesh.project(prj, model_final$model$summary.ran$bias_field$sd)
proj.points <- SpatialPoints(prj$lattice$loc)
proj4string(proj.points) <- Projection 
proj4string(norway.poly) <- Projection
ov <- sp::over(proj.points, norway.poly)
sd.spat[is.na(ov)] <- m.spat[is.na(ov)] <- NA

spat_coords <- expand.grid(x = prj$x, y = prj$y)
bias_df <- data.frame(decimalLongitude = spat_coords$x, decimalLatitude = spat_coords$y,
                      mean = as.vector(m.spat), sd = as.vector(sd.spat))


bias_mean <- SpatialPixelsDataFrame(points=spat_coords, 
                                    data=data.frame(bias_mean = as.vector(m.spat)), 
                                    proj4string=Projection)
plot(bias_mean)


par(mfrow=c(1,2), mar=c(0,0,0,0))
image.plot(x=prj$x, y=prj$y, z=m.spat, asp=1,
           xlab='', ylab='', axes=FALSE, horizontal=TRUE)
plot(norway.poly, add=TRUE)
image.plot(x=prj$x, y=prj$y, z=sd.spat, asp=1,
           xlab='', ylab='', axes=FALSE, horizontal=TRUE)
plot(norway.poly, add=TRUE)
