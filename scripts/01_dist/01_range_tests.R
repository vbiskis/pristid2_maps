# Details ----
#' 01_range_tests.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Dec 2025 
#' Content:
#' + expanded from 01_range_all 
#' + check stat significance for diff between time periods
#' ++ get diff between species
#' Dependencies:
#' mcp_map.rds
#' src_range.R <- functions for calcs
#' -----------

source("helpers/help_stats.R") # for main packages (e.g. read_xlsx, dplyr)
sitsawks <- readxl::read_xlsx("data/processed/sitsawks.xlsx") 
mcp_map <- readRDS('data/rds/mcp_map.rds')

source("helpers/functions/src_range.R") # hes got the spat libraries included
# already in src code but in case you need to change!
utm_crs <- st_crs("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

# run tests all at same time----
get_range <- function(species_name) {
  cat(sprintf("\n=== Analyzing %s ===\n", species_name))
  
  # 1. Jacknife
  jack_pre <- jack_mcp(species_name, "historic")
  jack_post <- jack_mcp(species_name, "recent")
  
  # 2. Bootstrap
  boot <- boot_mcp(species_name, n_boot = 1000)
  
  # 3. Permutation
  perm <- perm_mcp(species_name, n_perm = 1000)
  
  # 4. Jaccard
  jacc <- jacc_mcp(species_name) # in hindsight this name is v sim to above
  
  # Compile results
  results <- list(
    species = species_name,
    jacknife_pre = jack_pre,
    jacknife_post = jack_post,
    permutation_p = perm$p_value,
    bootstrap_diff = boot$obs_diff,
    bootstrap_ci = c(boot$ci_lower, boot$ci_upper),
    bootstrap_p = boot$p_value,
    jaccard = jacc
  )
  
  return(results)
}

# Let's be lazy and run for all species
# Range report----
range_res <- lapply(unique(sitsawks$Species_NB), get_range)

results_df <- do.call(rbind, lapply(range_res, function(x) {
  data.frame(
    Species = x$species,
    Pre_MCP = x$jacknife_pre$mean_mcp,
    Pre_SE = x$jacknife_pre$se,
    Post_MCP = x$jacknife_post$mean_mcp,
    Post_SE = x$jacknife_post$se,
    Change = x$bootstrap_diff,
    CI_lower = x$bootstrap_ci[1],
    CI_upper = x$bootstrap_ci[2],
    Perm_p = x$permutation_p,
    Boot_p = x$bootstrap_p,
    Jaccard = x$jaccard,
    Significant = ifelse(x$permutation_p < 0.05, "***", "ns")
  )
}))

# not sig! likely artifact of sampling!

# maxD?----

pw_dist <- function(species_name, time_period) {
  
  data <- sitsawc %>%
    filter(Species_NB == species_name, 
           Sub_Type == time_period,
           !is.na(GPS_long), !is.na(GPS_lat))
  
  # Transform to UTM
  pts <- st_as_sf(data, coords = c("GPS_long", "GPS_lat"), crs = 4326)
  pts_utm <- st_transform(pts, crs = utm_crs)
  coords <- st_coordinates(pts_utm)
  
  # All pairwise distances
  distances <- as.vector(dist(coords)) / 1000  # km
  
  return(distances)
}

# Run t-tests
species_list <- c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")
maxD_test <- data.frame()

for(species in species_list) {
  
  hist_dist <- pw_dist(species, "historic")
  rec_dist <- pw_dist(species, "recent")
  
  test <- t.test(hist_dist, rec_dist)
  
  maxD_test <- rbind(maxD_test, data.frame(
    Species = species,
    Mean_Historic = mean(hist_dist),
    Mean_Recent = mean(rec_dist),
    p_value = test$p.value
  ))
}

print(maxD_test) #not really what iwant... well leave it

