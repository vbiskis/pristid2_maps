## Details ----
#' 01_kde_figs.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Mar 2024 
#' Content: 
#' + final kde maps for pub
#' -----------

source('helpers/help_plot.R')
source('helpers/help_map.R')
pacman::p_load('eks', 'colorspace', 'readxl')
set_theme(mytheme)

sitsawc <- read_xlsx('data/processed/sitsawc.xlsx')

#Sep Hotspots----
#lets loop it
species_list <- c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")
hotplots <- list()  # To store the generated plots

for (species in species_list) {
  # Subset data for each species
  species_data <- sitsawc[sitsawc$Species_NB == species, c("GPS_lat", "GPS_long")]
  
  if (nrow(species_data) > 0) {
    #this is gross but I can think of no other way
    if (species == "A. cuspidata") {
      x_zoom <- range(species_data$GPS_long) + c(-1, 2)
      y_zoom <- range(species_data$GPS_lat) + c(-1.5, 2.5)
      legpos = c(0.5, 0.5)
      loc = "bl"
    } else if (species == "P. clavata") {
      x_zoom <- range(species_data$GPS_long) + c(-0.25, 1)
      y_zoom <- range(species_data$GPS_lat) + c(-0.5, -0.5)
      legpos = c(0.5, 0.5)
      loc = "bl"
    } else if (species == "P. pristis") {
      x_zoom <- range(species_data$GPS_long) + c(-0.5, 0.5)
      y_zoom <- range(species_data$GPS_lat) + c(-0.5, 1)
      legpos = c(0.5, 0.5)
      loc = "br"
    } else { #greens
      x_zoom <- range(species_data$GPS_long) + c(-1, 0)
      y_zoom <- range(species_data$GPS_lat) + c(0, 3)
      legpos = c(0.5, 0.5)
      loc = "bl"
    }
    
    saw_sf <- st_as_sf(species_data, coords = c("GPS_long", "GPS_lat"), crs = 4326)
    kde <- st_kde(saw_sf)

    # Create the KDE plot
    species_plot <- ggplot() +
      geom_sf(data = qld0, fill = "white", color = "black") +
      geom_sf(data = st_get_contour(kde, cont = c(5, 25, 50, 75, 95)), aes(fill=label_percent(contlabel))) +
      scale_fill_discrete_sequential(palette = "SunsetDark", alpha = 0.6)  + 
      coord_sf(xlim = x_zoom, ylim = y_zoom, expand = FALSE) +
      labs(x = "Longitude (°E)", y = "Latitude (°S)", fill = "Density") + 
      scale_x_continuous(
        breaks = seq(
          floor(min(x_zoom)),
          ceiling(max(x_zoom)),
          by = round((diff(x_zoom) / 3) * 2) / 2)) + #only room for 3 labs on the x
      scale_y_continuous(
        breaks = seq(
          floor(min(y_zoom)),
          ceiling(max(y_zoom)),
          by = round((diff(y_zoom) / 4) * 2) / 2)) +
      theme(axis.text = element_text(size = 12),  
            axis.title = element_text(size = 14),
            legend.position.inside = legpos, 
            legend.box.background = element_blank(),
            legend.box = "horizontal",
            legend.justification = c("left", "bottom"),
            plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +
      ggspatial::annotation_scale(location = loc, width_hint = 0.25, 
                                    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                                    text_cex = 0.8, text_family = "optima",
                                  line_width = 1.5) 
    
    # Save each plot in the list
    hotplots[[species]] <- species_plot
  }
}

#save em
ACKD <- hotplots[["A. cuspidata"]] + theme(legend.position = "none")
PCKD <- hotplots[["P. clavata"]] + theme(legend.position = "none")
PPKD <- hotplots[["P. pristis"]] + theme(legend.position = "none")
PZKD <- hotplots[["P. zijsron"]] + theme(legend.position = c(0.75, 0.5))

spechot <- (ACKD + theme(axis.title.x = element_blank())) +
           (PCKD + theme(axis.title.x = element_blank(), 
                         axis.title.y = element_blank())) +
           (PPKD + theme(axis.title.x = element_text(margin = margin(t = 10)))) +
           (PZKD + theme(axis.title.x = element_text(margin = margin(t = 10)), 
                         axis.title.y = element_blank())) +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 13, family = "Optima"))

ggsave(
  "figs/fig5/fig5_hotplots.png",
  plot = spechot,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)



