semivar = function(model, data, lat, lon){
  
  # simulate Randomized Quantile residuals from
  # the fitted model using the DHARMa package
  res <- simulateResiduals(model,
                           integerResponse = TRUE,
                           plot = FALSE)
  
  # collect residuals into a SpatialPointsDataFrame
  # using functions from the sp package
  
  dat <- data.frame(res = residuals(res), 
                    x = data[[lon]],
                    y = data[[lat]])
  
  coordinates(dat) <- ~ x + y
  proj4string(dat) <- CRS("+init=epsg:4326")
  
  dat <- spTransform(dat, CRS("+init=epsg:7854 +units=km"))
 
  # compute the empirical semi-variogram
  vario_emp <- variogram(res ~ 1, data = dat)
  
  # compute the theoretical semi-variogram using the spherical kernel 
  vario_sph <- fit.variogram(vario_emp, vgm("Sph"))
  
  # return a plot of the semi-variogram models with important 
  # estimates (sill and range) printed in the title
  vario_fitted <- show.vgms(min = min(vario_emp$dist), 
                            max = max(vario_emp$dist), 
                            models = "Sph",
                            range = vario_sph$range[2],
                            sill = vario_sph$psill[2],
                            nugget = vario_sph$psill[1],
                            plot = FALSE)
  vario_fitted$semivariance[1] <- vario_sph$psill[1]
  
  # create a complex ggplot object called 'p';
  # first we plot the distances (dist) and empirical 
  # semi-variance estimates (gamma) as a scatterplot using geom_point()
  p <- ggplot(vario_emp, aes(dist, gamma)) + 
    geom_point(size = 2) + 
    
    # next add the fitted semi-variogram model as a line
    geom_line(data = vario_fitted, 
              aes(distance, semivariance),
              linetype = 2,
              linewidth = 0.5) +
    
    # show a vertical line at the range estimate
    geom_vline(xintercept = vario_sph$range[2],
               linetype = 2) +
    
    # give an informative, dynamic title
    ggtitle(paste0('Range = ',
                   round(vario_sph$range[2],
                         2),
                   ' km; Sill = ',
                   round(sum(vario_sph$psill),
                         3))) +
    
    # further tidying of the plot
    theme_classic() +
    labs(x = 'Lag distance (km)',
         y = 'Semi-variance') +
    scale_y_continuous(limits = c(0, 
                                  max(c(vario_fitted$semivariance,
                                        vario_emp$gamma))))
  
  # return the plot
  return(p)
} 
