# Details ----
#' 7_fategam_map.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-31
#' ++ just relevant today overall!
#' Content: 
#' + make plot
#' Output:
#' + gps map
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/help_map.R')
theme_set(mytheme)

fatedf <- readRDS('data/rds/sits_fatemod.RDS')

fategam <- fatedf %>% 
  mutate(Species_NB = if_else(Species_NB == 'Pristis sp.', 'Pristidae', Species_NB),
         Species = if_else(Species_NB == 'Pristidae', NA, Species_NB), # removing unk species
         Gear3 = case_when(Gear2 == 'Sighting Only' | is.na(Gear2) ~ NA, # only using animals that are caught
                           Gear2 %in% c('Cast Net', 'Line') ~ 'Rec Gear',
                           .default = 'Comm Gear')) # just looking at actual catches

fategam$Species <- as.factor(fategam$Species)
fategam$Gear3 <- as.factor(fategam$Gear3)
glimpse(fategam)

# curr risk map ----
modFact <- gam(Fate ~ s(Cap_Year, by = NetFree) + Gear3
               + s(logpop) + s(logaDNm), # reasons why
               family = binomial,
               data = fategam, 
               method = 'ML')

summary(modFact)
gratia::draw(modFact)

modGPS <- gam(Fate ~ s(Cap_Year, by = NetFree) + Gear3
              + te(GPS_long, GPS_lat), # visualisation
                  family = binomial,
                  data = fategam, 
                  method = 'ML')

summary(modGPS)
gratia::draw(modGPS)

anova(fatmod, modGPS) # essentially equal!

acfate <- fategam %>% filter(Species == 'A. cuspidata') %>% 
  mutate(Maturity = if_else(Age_Class == 'adult', 'mature', 'immature'))
acfate$Maturity <- as.factor(acfate$Maturity)

modAC <- gam(Fate ~ s(Cap_Year, by = NetFree) # + Gear3
              # + te(logpop, logaDNm) # backbone of chosen model
              + te(GPS_long, GPS_lat, by = Maturity),
              family = binomial,
              data = acfate, 
              method = 'ML')
summary(modAC)
gratia::draw(modAC)

##recent----
fateproj <- expand.grid(
  GPS_long = seq(qldbox["xmin"], qldbox["xmax"], length.out = 100),
  GPS_lat = seq(qldbox["ymin"], qldbox["ymax"], length.out = 100)
)

summary(fategam$logpop)
summary(fategam$logaDNm)

fateproj$Gear3 <- "Comm Gear"
fateproj$Cap_Year <- 2023
fateproj$NetFree <- 0
fateproj$logpop <- 3.189 # median
fateproj$logaDNm <- 2.522 # median

preds <- predict(modGPS, 
                     newdata = fateproj, 
                     type = "link", 
                     se.fit = TRUE)

fateproj$.estimate <- preds$fit
fateproj$se <- preds$se.fit
fateproj$alpha <- scales::rescale(1/fateproj$se, to = c(0.2, 1))

#alright moment of truth!
ggplot(fateproj) +
  geom_raster(aes(x = GPS_long, y = GPS_lat, 
                  fill = plogis(.estimate), 
                  alpha = alpha)) + 
  geom_sf(data = qld0, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(colors = met.brewer("Hiroshige"),
                       limits = c(0, 1)) +
  labs(fill = "Likelihood of Survival",
       x = "Longitude (º)",
       y = "Latitude (º)",
       alpha = "1/Standard Error") +
  geom_contour(aes(x = GPS_long, y = GPS_lat, 
                   z = plogis(.estimate)),
               col = "grey50", linewidth = 0.5,
               breaks = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "pt"))

## plot!----
ggsave(
  "figs/fig11/fig11_fatemap.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
