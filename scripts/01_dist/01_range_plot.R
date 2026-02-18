# Details ----
#' 01_range_plot.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Sep 2024
#'  + updated 12 Dec 2025 to include helper funx
#'    and removed redundant lines of code across scripts
#' Dependencies:
#' + mcp_map 
#' + built in original script and brought over (can be used mult things now)
#' Content: 
#' + range calculation BETWEEN time periods
#' + not included in manuscript, visualisation of mcp change w time
#' -----------

source("helpers/help_map.R")
source("helpers/help_plot.R")
theme_set(mytheme)
mcp_map <- readRDS('data/rds/mcp_map.rds')

mcp_plot <- ggplot() + 
  annotation_map_tile('cartolight') +
  geom_spatial_point(crs = 4326) +
  facet_grid(~ST) +
  geom_sf(data = mcp_map, aes(fill = ID), alpha = 0.3) +
  scale_fill_manual(name = "Species", labels = spec_labs_ks, values = spec_col_ks) +
  scale_x_continuous(n.breaks = 5) +
  theme(strip.background = element_rect(color = "grey80"),
        strip.text = element_text(family = "Optima", face = "plain",
                                  size = 12, margin = margin(5, 0, 5, 0))) +
  labs(x = "Longitude", y = "Latitude") 

ggsave(
  "figs/thesis_vs/v2/table3.2/alt_range_vis.png",
  plot = mcp_plot,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
