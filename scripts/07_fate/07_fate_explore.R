# Details ----
#' 07_fate_explore.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 31 Mar 2025
#' - modified 2025-12-30
#' Content: 
#' + pulled out the clustering tests from original
#' ++ (3d_Fate_Trees) previously contained glm tests and gam comp.
#' ++ repetitive across mult scripts
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
theme_set(mytheme)

fatedf <- readRDS('data/rds/sits_fatemod.RDS')

#Check Mod Selection-----
fatedf <- predictors %>% 
  filter(!is.na(Alive2)) %>%
  mutate(Fate = case_when(Alive2 == TRUE ~ 1, 
                          .default = 0),
         Gear = case_when(Gear %in% c("Other", "Unknown") ~ NA,
                          .default = Gear),
         Gear = fct_collapse(Gear, ComNet = c('Gillnet', 'Prawn Net')),
         Region = fct_collapse(Region, CSEQ = c('CQ', 'SEQ'), 
                               WCY = c('WCY', 'TS'))) %>% 
  mutate(across(where(is.character) & !matches("SUB"), factor))

fatedf$Alive2 <- factor(fatedf$Alive2, levels = c(TRUE, FALSE), c("Alive", "Dead"))
#gf = goodfit(fatedf$Fate, type = "binomial") #duh

str(fatedf)
fatedf$Gear <- droplevels(fatedf$Gear)
table(fatedf$Region, fatedf$Species_NB)
table(fatedf$Gear, fatedf$Species_NB)

glimpse(fateks)
#no gear bc duh
#cant really include clavata same way you cant include gear
#as we have learned

#first up no int
AlMod1 <- glm(Alive2 ~ Cap_Year + Region +
                logprotg + logaDNm + logpop, 
                family = binomial(link = "logit"), 
                data = fateks)

summary(AlMod1)
par(mfrow = c(2,3))
visreg(AlMod1)
glm.diag.plots(AlMod1)               

drop1(AlMod1, test = 'Chi')
modsal1 <- step(AlMod1)
summary(modsal1) 
plot(modsal1)

#maximal from gam
modsal2 <- glm(Alive2 ~ Cap_Year + GPS_lat*logpop +
                 + logaDNm*logTL2, 
               family = binomial(link = "logit"), 
               data = fateks)

drop1(modsal2, test = 'Chi')
modsal3 <- step(modsal2)
summary(modsal3) #much smaller dataset - end up with size, year, lat
#used lat instead of Region

modsal4 <- glm(Alive2 ~ logaDNm*Cap_Year + GPS_lat*logpop, 
               family = binomial(link = "logit"), 
               data = fateks)

drop1(modsal4, test = 'Chi')
modsal5 <- step(modsal4)
summary(modsal5) 

#get year, pop lat again

likegam <- glm(Alive2 ~ Cap_Year + logaDNm*logpop, 
               family = binomial(link = "logit"), #lets just see if aDN stays
               data = fateks)

summary(likegam)
drop1(likegam)
stepgam <- step(likegam)
summary(stepgam) #doesnt rate them as sig (not linear!, but keeps em)

AIC(modsal1, modsal3, modsal5, likegam, stepgam)

#keep seeing trends:
#TL but it cuts dataset
#so no interactions between vars for glm
#gear/species obvs
#cap year
#gps lat > region
#pop
#gam > glm

performance::model_performance(stepgam) 
performance::model_performance(modsal5) #better

#cheeky factor tree----

ggcorr(fatedf, label = TRUE)
#now we get a better picture
#trawling is pos corr with protections and population
#days netted is in the tropics

fatescl <- fateks %>% 
  dplyr::select(Alive2, Cap_Year, Region, Species_NB, GPS_lat, GPS_long,
         Gear, logaDNm, logmavDNm, logTL2, logpop, logRF, NetFree) %>% 
  mutate_if(is.numeric, scale)
  
str(fatescl)
#no species bc clavata throws it!
fattree <- ctree(Alive2 ~ Cap_Year
                 + Species_NB
                 + Gear
                 + logTL2 
                 + Region 
                 + GPS_lat
                 + logaDNm
                 + logpop
                 #+ NetFree 
                 + logRF 
                 , data = fatescl) 

plot(fattree, gp = gpar(fontsize = 7)) #same as we saw before time + gear + pop
current_plot <- recordPlot()
tiff("FatePred.tiff", width = 9, height = 5, units = "in", res = 300)
replayPlot(current_plot)
dev.off()

##Trophies----
TMod1 <- glm(Sawless2 ~ Cap_Year + Region + Species_NB + 
               NetFree + Gear + logaDNm + logpop, 
             family = binomial(link = "logit"), 
             data = fateks)

summary(TMod1)
glm.diag.plots(TMod1)  

drop1(TMod1, test = 'Chi')
modstrop1 <- step(TMod1)
summary(modstrop1) #ooh - gear, pop, dn & year
glm.diag.plots(modstrop1)

TMod2 <- glm(Sawless2 ~ Cap_Year + Species_NB*logTL2 + GPS_lat*logpop +
             logaDNm + logRF, 
              family = binomial(link = "logit"), 
              data = fateks) 

summary(TMod2) 
glm.diag.plots(TMod2)               

drop1(TMod2, test = 'Chi')
modstrop2 <- step(TMod2)
summary(modstrop2)
modstrop2
glm.diag.plots(modstrop2) #ya ends up being lat and TL

sawgam <- gam(Sawless2 ~ s(Cap_Year) + s(logTL2, by = Species_NB) + s(logpop)
              + s(GPS_lat) + Gear + logaDNm, #put em all in
              family = binomial,
              data = fateks, 
              method = 'ML')

summary(sawgam)
gam.check(sawgam)
appraise(sawgam)

library(MuMIn)
options(na.action = "na.fail")
sawgam2 <- dredge(sawgam)

#for sawless its time size and pop! 
fateks$Sawless <- factor(fateks$Sawless2, levels = c(TRUE, FALSE), 
                            labels = c("Trophy Taken", "No Trophy"))
fatesaw <- fateks
fatesaw <- fatesaw %>% 
  filter(!is.na(Sawless))

fatescl <- fatesaw %>% 
  dplyr::select(Sawless, Cap_Year, Region, Species_NB, GPS_lat, GPS_long,
                Gear, logaDNm, logmavDNm, logTL2, logpop, logRF, logprotg) %>% 
  mutate_if(is.numeric, scale)

sawtree <- ctree(Sawless ~ Cap_Year + 
                   logaDNm + 
                   logRF + 
                   logpop + 
                   logTL2 + 
                   Species_NB, 
                 data = fatescl)
sawtree #no log pop here - just a look

plot(sawtree, gp = gpar(fontsize = 7)) #still true when scaled

