# Details ----
#' 00_set_maps.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - Split to sep script 2025-12-09
#' Content: Create basemaps for use in figs
#' Output: 
#' + regions.rds
#' -----------

pacman::p_load('dplyr', 'tidyverse', 
               'terra', 'sf', 'sp', 
               'rnaturalearth', 'maps',
               'tmap', 'tmaptools', 'devtools',
               'ggspatial', 'ggthemes', 
               'geodata', 'geosphere')

#make qld map----
QLD_crs <- crs("EPSG:4326")
aus <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")%>%
  st_transform(QLD_crs)
qldbox <- st_bbox(c(xmin = 137.5, xmax = 154, ymin = -28.5, ymax = -9), crs = QLD_crs)
qld_poly <- st_as_sfc(qldbox)
qld <- st_intersection(st_geometry(aus), qld_poly)

plot(qld) #looks good
saveRDS(qld, 'data/rds/qld_simple.rds')

#make region map----
regions <- st_read('gis/AllBasins.shp')
head(regions)

#check----
regions <- regions %>% 
  dplyr::select(BASIN_NAME, BASIN_NUMB,
                FEATURETYP, NAME, layer) %>% 
  mutate(Region = case_when(layer == "NEQ-Basin" ~ "FNQ",
                            layer == "CQ-Basin" ~ "CEQ",
                            layer == "SEQ-Basins" ~ "SEQ",
                            layer == "TorresStrait" ~ "TS",
                            layer == "WCY-Basin" ~ "WCY",
                            layer == "GOC-Basin" ~ "GoC",
                            .default = layer)) %>% 
  dplyr::select(-layer)

regions <- st_transform(regions, QLD_crs)
regions_map <- st_intersection(regions, qld_poly)

ggplot() +
  geom_sf(data = regions_map, 
          aes(fill = Region), color = "black") #check boundaries

# and good!
# has TS in there but clips the western and southern boundary

saveRDS(regions, 'data/rds/regions.rds')
saveRDS(regions_map, 'data/rds/regions_map.rds') #and save you for later too!


