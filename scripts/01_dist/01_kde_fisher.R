# Details ----
#' 01_kde_fisher.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Dec 2025 
#' Content: *New. as a comparison for spec KDE
#' + check bias/beh of fishers
#' -----------

source('helpers/help_map.R')
source('helpers/help_stats.R')

library(MASS)
library(KernSmooth)

sitsawc <- read_xlsx('data/processed/sitsawc.xlsx')

# using all data: unfiltered
allsubs <- read_xlsx('data/Sub_Fishing.xlsx')

allsubs <- allsubs %>% 
  mutate(Sub_Type = case_when(Cap_Year <= 2009 ~ "historic",
                              Cap_Year > 2009 ~ "recent",
                              .default = Sub_Type),
         LAT = as.numeric(LAT),
         LONG = as.numeric(LONG)) %>%  #that'll do it
  filter(!is.na(LONG), !is.na(LAT))

# calculate effort density (all subs pooled)
hist_fish <- allsubs %>%
  filter(Sub_Type == "historic")

rec_fish <- allsubs %>%
  filter(Sub_Type == "recent")

fisher_kdeh <- kde2d(hist_fish$LONG, hist_fish$LAT)
fisher_kder <- kde2d(rec_fish$LONG, rec_fish$LAT)

# check him
par(mfrow = c(1, 2))
filled.contour(fisher_kdeh$x, fisher_kdeh$y, fisher_kdeh$z,
               color.palette = terrain.colors,
               main = "Fisher Effort - Historic",
               xlab = "Longitude", ylab = "Latitude")
filled.contour(fisher_kder$x, fisher_kder$y, fisher_kder$z,
               color.palette = terrain.colors,
               main = "Fisher Effort - Recent",
               xlab = "Longitude", ylab = "Latitude")

# looks good!

# corr tests----
species_list <- c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")

fisher_change <- data.frame()

for (species in species_list) {
  
  # Species data
  sp_hist <- sitsawc %>%
    filter(Species_NB == species, Sub_Type == "historic",
           !is.na(GPS_long), !is.na(GPS_lat))
  
  sp_rec <- sitsawc %>%
    filter(Species_NB == species, Sub_Type == "recent",
           !is.na(GPS_long), !is.na(GPS_lat))
  
  # Fisher effort data
  fish_hist <- allsubs %>%
    filter(Sub_Type == "historic", !is.na(LONG), !is.na(LAT))
  
  fish_rec <- allsubs %>%
    filter(Sub_Type == "recent", !is.na(LONG), !is.na(LAT))
  
  if(nrow(sp_hist) < 5 | nrow(sp_rec) < 5) next
  
  # Common grid for ALL periods
  xlim <- range(c(sp_hist$GPS_long, sp_rec$GPS_long, 
                  fish_hist$LONG, fish_rec$LONG))
  ylim <- range(c(sp_hist$GPS_lat, sp_rec$GPS_lat,
                  fish_hist$LAT, fish_rec$LAT))
  
  # Calculate KDEs
  sp_kde_hist <- bkde2D(cbind(sp_hist$GPS_long, sp_hist$GPS_lat),
                        bandwidth = c(0.5, 0.5), gridsize = c(50, 50),
                        range.x = list(xlim, ylim))
  
  sp_kde_rec <- bkde2D(cbind(sp_rec$GPS_long, sp_rec$GPS_lat),
                       bandwidth = c(0.5, 0.5), gridsize = c(50, 50),
                       range.x = list(xlim, ylim))
  
  fish_kde_hist <- bkde2D(cbind(fish_hist$LONG, fish_hist$LAT),
                          bandwidth = c(0.5, 0.5), gridsize = c(50, 50),
                          range.x = list(xlim, ylim))
  
  fish_kde_rec <- bkde2D(cbind(fish_rec$LONG, fish_rec$LAT),
                         bandwidth = c(0.5, 0.5), gridsize = c(50, 50),
                         range.x = list(xlim, ylim))
  
  # get the changes***
  sp_change <- as.vector(sp_kde_rec$fhat - sp_kde_hist$fhat)
  fish_change <- as.vector(fish_kde_rec$fhat - fish_kde_hist$fhat)
  
  # test if species change correlates with fisher change
  change_cor <- cor.test(sp_change, fish_change, family = "pearson")
  
  fisher_change <- rbind(fisher_change, data.frame(
    Species = species,
    Change_Correlation = as.numeric(change_cor$estimate),
    P_value = change_cor$p.value
  ))
}

print(fisher_change)
