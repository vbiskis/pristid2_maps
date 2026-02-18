# Details ----
#' src_range.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Dec 2025 
#' Content:
#' + expanded/continued from 01_range_time 
#' + fns to check stat significance for diff between time periods
#' -----------

pacman::p_load('sf', 'sp', 'adehabitatHR')
utm_crs <- st_crs("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

# mcp as base of all
# jackknife for small n (historic group)
# boot for effect size
# permutation for sig
# jaccard for trend

# mcp----
calc_mcp <- function(data) {
  if(nrow(data) < 5) {   # Need at least 5 points for a polygon
    return(NA)
  }
  
  pts <- st_as_sf(data, coords = c("GPS_long", "GPS_lat"))
  st_crs(pts) <- st_crs("+proj=longlat +datum=WGS84")
  pts_utm <- st_transform(pts, crs = utm_crs)  # Use your crs in m
  pts_sp <- as(pts_utm, "Spatial")
  
  # Calculate MCP
  tryCatch({
    mcp_result <- mcp(pts_sp, percent = 100,
                      unin = "m", unout = "km2")
    return(mcp_result@data$area[1])  # Returns area in m²
  }, error = function(e) {
    return(NA)  # If it fails, return NA
  })
}

# jackknife----
jack_mcp <- function(species_name, time_period) {
  cat(sprintf("\n[Jacknife] Running %s...", species_name))
  
  sp_data <- sitsawks[sitsawks$Species_NB == species_name & 
                        sitsawks$Sub_Type == time_period, ]
  
  n <- nrow(sp_data)
  jack_mcps <- numeric(n)
  
  for(i in 1:n) { # Remove observation i
    jack_data <- sp_data[-i, ]     
    jack_mcps[i] <- calc_mcp(jack_data)
  }
  
  jack_se <- sqrt((n-1)/n * sum((jack_mcps - mean(jack_mcps))^2))   # Jacknife SE
  
  return(list(
    mean_mcp = mean(jack_mcps),
    se = jack_se
  ))
}

# bootstrap----
boot_mcp <- function(species_name, n_boot = 1000) {
  cat(sprintf("\n[Bootstrap] Running %s...", species_name))
  
  # Get data
  pre_data <- sitsawks[sitsawks$Species_NB == species_name & 
                         sitsawks$Sub_Type == "historic", ]
  post_data <- sitsawks[sitsawks$Species_NB == species_name & 
                          sitsawks$Sub_Type == "recent", ]
  
  # Observed
  obs_pre <- calc_mcp(pre_data)
  obs_post <- calc_mcp(post_data)
  obs_diff <- obs_post - obs_pre
  
  # Bootstrap
  boot_diffs <- numeric(n_boot)
  for(i in 1:n_boot) {
    boot_pre <- pre_data[sample(nrow(pre_data), replace = TRUE), ]
    boot_post <- post_data[sample(nrow(post_data), replace = TRUE), ]
    
    boot_diffs[i] <- calc_mcp(boot_post) - calc_mcp(boot_pre)
  }
  
  # Results
  p_val <- mean(abs(boot_diffs) >= abs(obs_diff))
  
  return(list(
    species = species_name,
    obs_diff = obs_diff / 1e6,  # km²
    p_value = p_val,
    ci_lower = quantile(boot_diffs, 0.025, na.rm = TRUE) / 1e6,
    ci_upper = quantile(boot_diffs, 0.975, na.rm = TRUE) / 1e6
  ))
}

# permutation----
perm_mcp <- function(species_name, n_perm = 1000) {

  cat(sprintf("\n[Permutation] Running %s...", species_name))
  
  sp_data <- sitsawks[sitsawks$Species_NB == species_name, ]
  
  # Observed
  obs_pre <- calc_mcp(sp_data[sp_data$Sub_Type == "historic", ])
  obs_post <- calc_mcp(sp_data[sp_data$Sub_Type == "recent", ])
  obs_diff <- obs_post - obs_pre
  
  # Permutation
  perm_diffs <- numeric(n_perm)
  for(i in 1:n_perm) {
    shuffled <- sp_data
    shuffled$Sub_Type <- sample(sp_data$Sub_Type)  # Shuffle labels
    
    perm_pre <- calc_mcp(shuffled[shuffled$Sub_Type == "historic", ])
    perm_post <- calc_mcp(shuffled[shuffled$Sub_Type == "recent", ])
    perm_diffs[i] <- perm_post - perm_pre
  }
  
  p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
  
  return(list(
    species = species_name,
    p_value = p_val
  ))
}

# jaccard----
jacc_mcp <- function(species_name) {
  cat(sprintf("\n[Jaccard] Running %s...", species_name))
  
  # Get MCP polygons from mcp_map object
  pre_poly <- mcp_map[mcp_map$ID == species_name & mcp_map$ST == "Pre-2009", ]
  post_poly <- mcp_map[mcp_map$ID == species_name & mcp_map$ST == "Post-2009", ]
  
  if(nrow(pre_poly) == 0 | nrow(post_poly) == 0) return(NA)
  
  # Calculate Jaccard
  intersection <- st_intersection(pre_poly, post_poly)
  union_area <- st_area(st_union(pre_poly, post_poly))
  
  jaccard <- as.numeric(st_area(intersection) / union_area)
  
  return(jaccard)
}