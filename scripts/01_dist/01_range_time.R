# Details ----
#' 01_range_time.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Sep 2024
#'  + updated 12 Dec 2025 to include helper funx
#'    and removed redundant lines of code across scripts
#' Content: 
#' + range calculation BETWEEN time periods
#' + gen trends/vals, not stats
#' + produces mcp_map.rds used in tests (updated 12 Dec to itemise scripts)
#' -----------

source("helpers/help_map.R") # for spatial

sitsawks <- readxl::read_xlsx("data/processed/sitsawks.xlsx") 
theme_set(mytheme)
utm_crs <- st_crs("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

# pre + post----
## maxD----
max_dists <- list()
subtypes <- unique(sitsawks$Sub_Type)

for (st in subtypes) {
  subtype_data <- sitsawks[sitsawks$Sub_Type == st, ]
  
  pts <- st_as_sf(subtype_data, coords = c("GPS_long", "GPS_lat"))
  st_crs(pts) <- st_crs("+proj=longlat +datum=WGS84")
  pts <- st_transform(pts, crs = utm_crs) 
  xy <- st_coordinates(pts)
  
  #put this IN the loop Nikki
  species <- unique(pts$Species_NB)
  max_dist <- vector(length = length(species))
  
  for (s in 1:length(species)) {
    p <- xy[pts$Species_NB == species[s], ]
    d <- as.matrix(dist(p))
    diag(d) <- NA                          # Ignore the distance of a point to itself!!!
    max_dist[s] <- max(d, na.rm = TRUE)    # Get max value
  }
  
  if (st == "recent") {   #Want them as 2 sep columns soooo
    maxD_rec <- data.frame(
      species = species,
      maxD_rec = max_dist
    )
  } else if (st == "historic") {
    maxD_hist <- data.frame(
      species = species,
      maxD_hist = max_dist #how pretty
    )
  }
}

maxD_comp <- merge(maxD_hist, maxD_rec, by = "species") # Voila!
#PP same, #PZ and AC decreased
#now the question of course - is it significant?

## MCP----
pts.sp1 <- data.frame(
  Species = sitsawks$Species_NB,
  Sub_Type = sitsawks$Sub_Type,
  Lat = sitsawks$GPS_lat,
  Long = sitsawks$GPS_long
)

subtypes <- unique(pts.sp1$Sub_Type)
species_list <- unique(pts.sp1$Species)
mcp_df_fin <- data.frame()

species_list <- sort(species_list)
for (sp in species_list) { #this is a pattern I like and I'm sticking to it
  for (st in subtypes) {
    subtype_pts <- pts.sp1[pts.sp1$Sub_Type == st 
                           & pts.sp1$Species == sp, ]
    
    if (nrow(subtype_pts) > 0) {
      subtype_pts$ID <- paste(sp)
      
      pts.spm <- st_as_sf(subtype_pts, coords = c("Long", "Lat"))
      st_crs(pts.spm) <- st_crs("+proj=longlat +datum=WGS84")
      pts.utm <- st_transform(pts.spm, crs = utm_crs) # and make the mcp a value that means something 
      pts.sputm <- as(pts.utm, "Spatial")       # Convert to SPDF (or else it yells again)

      mcp <- mcp(pts.sputm[, "ID"], percent = 100,
                 unin = "m", unout = "km2") #all this for you buddy
      
      mcp_df <- data.frame(      # Output is disgusting, lets fix it
        ID = rownames(mcp@data), # cool new trick
        ST = st,
        Area = mcp@data$area,
        Geometry = I(st_as_sf(mcp)$geometry) # don't totally get the I still
      )
      
      for (i in 1:nrow(mcp_df)) {
        mcp_df_fin <- rbind(mcp_df_fin, data.frame(
          ID = mcp_df$ID[i],
          ST = st,
          Area = mcp_df$Area[i],
          Geometry = I(mcp_df$Geometry[i])
        ))
      }
    }
  }
}

mcp_df_fin <- mcp_df_fin %>%
  mutate( 
    ST = if_else(ST == "historic", "Pre-2009", "Post-2009"),
    ST = factor(ST, levels = c("Pre-2009", "Post-2009"))
  )

mcp_map <- st_as_sf(mcp_df_fin)
saveRDS(mcp_map, "data/rds/mcp_map.rds") # for sig tests

# see vals----
mcp_summ <- mcp_df_fin %>%
  group_by(ID, ST) %>%
  summarise(Area = signif(Area, 3)) %>%
  pivot_wider(names_from = ST, values_from = Area) %>%
  arrange(ID)

