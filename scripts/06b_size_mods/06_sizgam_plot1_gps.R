# Details ----
#' 06_sizgam_plot_gps.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Mar 2025
#' - modified 2025-12-28 so it runs independently
#' Content: 
#' + make plot
#' Output:
#' + gps map for TL est
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/help_map.R')
theme_set(mytheme)

sizgam <- readRDS('data/rds/sits_sizemod.RDS')
glimpse(sizgam)

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat, NetFree), as.factor))

#GPS Map----
#lets just look at the key x3 vars + location
modgps <- gam(logTL2 ~ Cap_Year + 
                + Species_NB 
              + te(GPS_long, GPS_lat), 
              family = gaussian,
              data = sizgam, 
              method = 'ML')

summary(modgps)
draw(modgps)

draw(modgps, select = "te(GPS_long,GPS_lat)") + 
  ylab("Latitude") + 
  xlab("Longitude")

sawdat <- smooth_estimates(modgps) %>% 
  filter(.smooth == "te(GPS_long,GPS_lat)") %>% # Getting just the tensor spline and nothing else
  dplyr::select(GPS_long, GPS_lat, .estimate) %>% # Get just the variables we want
  distinct() # Avoid duplicate data, if any exist
head(sawdat)

plotqld <- st_as_sf(qld0)

ggplot(sawdat) +
  geom_raster(aes(x = GPS_long, y = GPS_lat, 
                  fill = exp(.estimate + modgps$coefficients[1] + 
                               modgps$coefficients[2]*2024 + #this year
                               modgps$coefficients[3]*0 #AC
                  ))
  ) + 
  geom_sf(data = plotqld, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient(low = "darkblue", high = "darkred") +
  labs(fill = "Total Length", 
       x = "Longitude (º)",
       y = "Latitude (º)") +
  geom_contour(aes(x = GPS_long, y = GPS_lat, z = exp(.estimate + modgps$coefficients[1] + 
                                                        modgps$coefficients[2]*2024 + #this year
                                                        modgps$coefficients[3]*0)), 
               col = "grey50") 

ggplot(sawdat) +
  geom_raster(aes(x = GPS_long, y = GPS_lat, 
                  fill = exp(.estimate + modgps$coefficients[1] + 
                               modgps$coefficients[2]*1990 + #this year
                               modgps$coefficients[3]*0 #AC
                  ))
  ) + 
  geom_sf(data = plotqld, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient(low = "darkblue", high = "darkred") +
  labs(fill = "Total Length", 
       x = "Longitude (º)",
       y = "Latitude (º)") +
  geom_contour(aes(x = GPS_long, y = GPS_lat, z = exp(.estimate + modgps$coefficients[1] + 
                                                        modgps$coefficients[2]*1990 + #this year
                                                        modgps$coefficients[3]*0)), 
               col = "grey50") 

