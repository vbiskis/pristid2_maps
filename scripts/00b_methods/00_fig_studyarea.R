# Details ----
#' 00_fig_study_area.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025-12-09
#' Content: 
#' + simpler study area including regions
#' Output: 
#' + study area figure for methods
#' -----------

source('helpers/help_map.R')
source('helpers/help_plot.R')
theme_set(mytheme)

# plotting region----
# 1. Manually specify the order

regions_map$RName <- factor(regions_map$Region, 
                                    levels = c("TS", "WCY", 
                                               "GoC", "FNQ", 
                                               "CEQ", "SEQ"))

bathomes <- st_read('gis/bathomes.shp') %>%
  st_transform(QLD_crs) %>%
  st_make_valid() #bc some angry geos

bathqld <- st_intersection(bathomes, grid0)
head(bathqld)

ggplot() +
  geom_sf(data = bathqld %>%
            filter(Depth_ID %in% c(18)), 
          aes(alpha = Depth_ID), fill = NA, color = 'grey70', 
          show.legend = FALSE) +
  geom_sf(data = bathqld %>%
            filter(Depth_ID %in% c(20)), 
          aes(alpha = Depth_ID), fill = NA, color = 'grey50', 
          show.legend = FALSE) +
  geom_sf(data = regions_map %>% filter(RName == 'TS'), 
          fill = "white", alpha = 0.75) +
  geom_sf(data = qld, fill = "white") +
  geom_sf(data = regions_map, 
          aes(fill = RName), alpha = 0.5) +
  geom_sf(data = regions_map, fill = NA, color = "grey25") +
  scale_fill_met_d(name = 'Moreau') +
  pilot::theme_pilot(legend_title_color = "black", legend_text_color = "black") + # just a bit extra
  labs(fill = "Region") +
  theme(legend.position = c(0.9, 0.7),
        plot.margin = unit(c(0, 5, 5, 5), "pt"))

ggsave(
  "figs/fig2/fig2_regions.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 5,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
