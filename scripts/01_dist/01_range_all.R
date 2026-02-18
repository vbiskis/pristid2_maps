# Details ----
#' 01_range_all.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 6 Mar 2024
#' + updated 12 Dec 2025 to include helper funx
#'    and removed redundant lines of code across scripts
#' Content: 
#' + range calculation
#' + maxD
#' Note: These values are overall, not split by time period.
#' -----------

#Load em up----
source("helpers/help_map.R")
source("helpers/help_plot.R")
theme_set(mytheme)

sitsawks <- readxl::read_xlsx("data/processed/sitsawks.xlsx") 

# Range Size----
## maxD----
sitsawks$Species_NB <- as.character(sitsawks$Species_NB)

#changing sightings to sf object
species_coords <- cbind(sitsawks$Species_NB, sitsawks$GPS_lat, sitsawks$GPS_long)
pts.sp <- as.data.frame(species_coords) 
pts.sp <- st_as_sf(pts.sp, coords = c("V3", "V2")) 
st_crs(pts.sp) <- st_crs("+proj=longlat +datum=WGS84")  # Tell it what CRS it IS
utm_crs <- st_crs("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
pts.sp <- st_transform(pts.sp, crs = utm_crs) 
xy <- st_coordinates(pts.sp) # get coords
sp <- unique(pts.sp$V1) # and list of species

maxD <- vector(length=length(sp))
for (s in 1:length(sp)) {
  # get the coordinates for species 's's
  p <- xy[pts.sp$V1 == sp[s], ]
  # distance matrix
  d <- as.matrix(dist(p))
  # ignore the distance of a point to itself
  diag(d) <- NA
  # get max value
  maxD[s] <- max(d, na.rm=TRUE)
}

maxD_species <- data.frame(species = sp, 
                           max_distance = maxD)
maxD_species <- maxD_species %>% 
  mutate(maxD = signif(max_distance / 1000, 3))

# AC greatest range
# then PZ
# PP
# PC tiny

#this is of course not the entire Aussie range for these species, but would represent southern limit; 
#also noting that nothing between NT/QLD GOC for 1000s of ks

## convex Hull----
xy1 <- st_as_sf(data.frame(lon = xy[,1], lat = xy[,2]),
                coords = c("lon", "lat"), crs = st_crs(pts.sp))

mapview::mapview(pts.sp) #pretty

hulls <- pts.sp %>%
  group_by(V1) %>% #thats spec col name
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()

# visualise
mapview::mapview(list(pts.sp, hulls))

## MCP----
pts_sputm <- as(pts.sp, "Spatial")
saw.mcp_km <- mcp(pts_sputm, percent = 100) #that's all of them! 
saw.mcp_km 

# would be useful to do regionally - i.e. for fragmented pops
hrs <- mcp.area(pts_sputm, percent = seq(50, 100, by = 5),
                unin = "m",
                unout = "km2")
summary(hrs)

# Transform the point and MCP objects. 
saw.mcp <- spTransform(saw.mcp, CRS("+proj=longlat"))
saw.mcp_sf <- st_as_sf(saw.mcp) # here I am again doing something but not entirely understanding why
saw.mcp_sf
pts.sp

#Now put them on a map
rangemap <- ggplot() + 
  annotation_map_tile('cartolight') +
  geom_spatial_point(crs = 4326) +
  geom_sf(data = saw.mcp_sf, aes(fill = id), alpha = 0.3) +
  geom_sf(data = pts_sf, aes(colour = V1), show.legend = TRUE) +  
  scale_color_manual(name = "Species", labels = spec_labs_ks, values = spec_col_ks) +  # Define manual legend
  scale_fill_manual(name = "Species", labels = spec_labs_ks, values = spec_col_ks, guide = "none") +  # Remove legend for polygons
  labs(x = "Longitude", y = "Latitude")


