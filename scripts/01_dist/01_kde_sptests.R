# Details ----
#' 01_kde_sptests.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Sep 2024
#' Content: 
#' + stats tests for density
#' -----------

source('helpers/help_map.R')
source('helpers/help_stats.R')
sitsawc <- read_xlsx('data/processed/sitsawc.xlsx')

library(MASS) #some extras
library(KernSmooth)

##KDE----
###All Years----

species_list <- c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")

kdeplots <- list()  # To store the generated plots

for (species in species_list) {
  # Subset data for each species
  species_dat <- sitsawc[sitsawc$Species_NB == species, c("GPS_lat", "GPS_long")]
  
  speckde <- kde2d(species_dat$GPS_long, species_dat$GPS_lat)
  filled.contour(speckde$x, speckde$y, speckde$z, color.palette = terrain.colors, 
                 main = paste("Kernel Density Estimation -", species),
                 xlab = "Longitude", ylab = "Latitude")

  # Save each plot in the list
  kdeplots[[species]] <- recordPlot() 
  }

#check invidivual
kdeplots[["A. cuspidata"]] #she just loves loops now
kdeplots[["P. clavata"]]
kdeplots[["P. pristis"]]
kdeplots[["P. zijsron"]]

##Sep by time----

###Contour again----
sep_kdeplots <- list()

for (species in species_list) {
  # Subset data for each species
  species_data <- sitsawc[sitsawc$Species_NB == species, c("GPS_lat", "GPS_long", "Sub_Type")]
  subtypes <- unique(species_data$Sub_Type)
  
  par(mfrow = c(1, 2),
      mar = c(3,3,3,3))
  for (st in subtypes) {
    # Subset data by subtype
    subtype_data <- species_data[species_data$Sub_Type == st, ]
    speckde <- kde2d(subtype_data$GPS_long, subtype_data$GPS_lat)
    filled.contour(speckde$x, speckde$y, speckde$z, color.palette = terrain.colors, 
                 main = paste("Kernel Density Estimation -", species, paste(":"), st),
                 xlab = "Longitude", ylab = "Latitude")
  }
    # Visualize contour plots
    sep_kdeplots[[species]] <- recordPlot() 
}

###Density----
kde_lat_list<- list()
kde_long_list<- list()

par(mfrow = c(2, 2))

for (species in species_list) {
  # Subset again
  species_data <- sitsawc[sitsawc$Species_NB == species, c("GPS_lat", "GPS_long", "Sub_Type")]
  subtypes <- unique(species_data$Sub_Type)
  
  for (st in subtypes) {
    # Subset data by subtype
    subtype_data <- species_data[species_data$Sub_Type == st, ]
    kde_lat <- density(subtype_data$GPS_lat)
    kde_long <- density(subtype_data$GPS_long)
    
    # Store KDEs in lists
    kde_lat_list[[paste(species, "-", st, sep = "_")]] <- kde_lat
    kde_long_list[[paste(species, "-", st, sep = "_")]] <- kde_long
    
    # Visualize KDE plots
    plot(kde_lat, main = paste("KDE - Latitude - Species:", species, ":", st))
    plot(kde_long, main = paste("KDE - Longitude - Species:", species, ":", st))
  }
}

#Stats----
#okay so we're seeing some of the same general shapes, 
#but some interesting differences for AC and PZ
#are they sig?

##KS test----

#can she go for a loop one more time:
ksres <- list()

for (species in species_list) {
  species_data <- sitsawc[sitsawc$Species_NB == species, c("GPS_lat", "GPS_long", "Sub_Type")]
  subtypes <- unique(species_data$Sub_Type)
  
  # Assume exactly two subtypes
  rec <- species_data[species_data$Sub_Type == subtypes[1], ]
  hist <- species_data[species_data$Sub_Type == subtypes[2], ]
  
  # Perform KS tests
  kslat <- ks.test(rec$GPS_lat, hist$GPS_lat)
  kslong <- ks.test(rec$GPS_long, hist$GPS_long)
    
  # Store ks in pretty list
  ksres[[species]] <- data.frame(
    Species = species,
    Lat_D = kslat$statistic,
    Lat_pval = kslat$p.value,
    Long_D = kslong$statistic,
    Long_pval = kslong$p.value
  )
}

#2 Sample Kolmogorov-Smirnov test
ksres_df <- do.call(rbind, ksres)
print(ksres_df)

# all east coast shifts, nothing latitude-wise significant
# not what I was expecting for AC, but maybe more resilient to change 
# not enough data for PC (only in Weipa)
# PP sign change in both lat and long - likely due to survey effort
# suggests a range contn from PZ over 30 yrs

