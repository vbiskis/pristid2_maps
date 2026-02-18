# Details ----
#' help_map.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - Split to sep script 2025-12-09 (embarrassing)
#' Content: General settings for mapping
#'  avoid repetition
#' -----------

pacman::p_load('dplyr', 'tidyverse', 
               'terra', 'sf', 'sp', 
               'rnaturalearth', 'maps', 'mapsf',
               'adehabitatHR',
               'tmap', 'tmaptools', 'devtools',
               'ggspatial', 'ggthemes', 
               'geodata', 'geosphere')

QLD_crs <- crs("EPSG:4326")
aus <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")%>%
  st_transform(QLD_crs)
studyarea <- ext(c(137, 154, -29, -10))
qldbox <- st_bbox(c(xmin = 137.5, xmax = 154, ymin = -28.5, ymax = -9), crs = QLD_crs)

grid1 <- st_make_grid(qldbox, n = c(16, 18)) #1x1 degree grid
qld1 <- st_intersection(aus, grid1)
grid0 <- st_make_grid(qldbox, n = c(1, 1)) #no grid
qld0 <- st_intersection(aus, grid0)

qld <- readRDS('data/rds/qld_simple.rds') # simple qld underlay
regions <- readRDS('data/rds/regions.rds') # regions with all borders
regions_map <- readRDS('data/rds/regions_map.rds') # regions clipped at x and y axis

# extra mapping cols----

spec_all_labels <- c(
  expression(italic("P. clavata")),
  expression(italic("P. pristis")),
  expression(paste(italic("P. clavata"), " or ", italic("P. pristis"))),
  expression(italic("A. cuspidata")), 
  expression(italic("P. zijsron")),
  expression(paste(italic("P. zijsron"), " or ", italic("A. cuspidata"))),
  expression(italic("Pristis sp.")),
  expression("Pristidae")
)

