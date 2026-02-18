# Details ----
#' 06_sizgam_pc.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-27
#' Content: 
#' + applying automated script to built multiple gams simultaneously
#' Output:
#' final model for P. clavata TL 
#' ++ not usable, n too small/isolated
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/functions/src_gam_fxns.R')
theme_set(mytheme)

#source('helpers/help_map.R')
#source('helpers/functions/src_autocorr.R')

sizgam <- readRDS('data/rds/sits_sizemod.RDS')
glimpse(sizgam)

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat), as.factor))

# stats----

csize <- sizgam %>% 
  filter(Species_NB == "P. clavata") 
glimpse(csize)

csize %>% 
  group_by(Gear) %>% 
  dplyr::summarise(n = n(),
                   meanTL = mean(Size_Final),
                   minYr = min(Cap_Year),
                   maxYr = max(Cap_Year),
                   proxRF = mean(dist_Cl_RF),
                   distmouth = mean(transout))

## pick up here when cleaning code----

modc1 <- gam(logTL2 ~ s(Cap_Year, k = 8),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc1) #13.8 already
draw(modc1)

modc2 <- gam(logTL2 ~ s(Cap_Mo, bs = "cc"),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc2) #nooope

modc3 <- gam(logTL2 ~ s(Cap_Year, k = 8) + s(transout),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc3) #same

modc4 <- gam(logTL2 ~ s(Cap_Year, k = 8) + s(MG_dist),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc4) #14.9
draw(moda9) #not sig

modc5 <- gam(logTL2 ~ s(Cap_Year, k = 8) + s(Salt_dist),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc5) #nope
draw(modc5) 

modc6 <- gam(logTL2 ~ s(Cap_Year, k = 8) + s(Cap_Mo, bs = "cc"),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc6) #no and no
draw(modc6) 

modc7 <- gam(logTL2 ~ s(Cap_Year, k = 8) + NetFree,
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc7) #no way, only a couple points found outside net free zones
draw(modc7) 

modc8 <- gam(logTL2 ~ s(popadj),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc8) #huh okay, pop better indicator
#includes change with time so in a very small area I guess that makes sense
draw(modc8)

modc9 <- gam(logTL2 ~ s(dist_Cl_RF),
             family = gaussian,
             data = csize, 
             method = 'ML')

summary(modc9) #no no
draw(modc9)

csize$Gear
modc10 <- gam(logTL2 ~ Gear,
              family = gaussian,
              data = csize, 
              method = 'ML')

summary(modc10)
appraise(modc10) #ew
overview(modc10)

modc11 <- gam(logTL2 ~ Gear + s(Cap_Year, k = 8),
              family = gaussian,
              data = csize, 
              method = 'ML')

summary(modc11)
plot_predictions(modc11, condition = c("Cap_Year", "Gear"))
anova(modc10, modc11)
#no need for a gam - linear

modc12 <- gam(logTL2 ~ Gear + s(Cap_Year, by = Gear, k = 8),
              family = gaussian,
              data = csize, 
              method = 'ML')
summary(modc12)
plot_predictions(modc12, condition = c("Cap_Year", "Gear"))
anova(modc10, modc12)
