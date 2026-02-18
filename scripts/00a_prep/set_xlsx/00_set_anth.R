# Details ----
#' 00_set_anth.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 10 Dec 2024
#' - modified 2025-12-13
#' Content: 
#' + processing xlsx (original, as in src_pred.R)
#' + but now produces xlsx, to be called directly
#' ++ just the anthro factors really ~ does not contain enviro details (sets in set_mods)
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl')

# always start with----
sitsaw <- readxl::read_xlsx("data/processed/sitsaw.xlsx") 

#Prep DFs----
#newdf with all anth factors
sitsfate <- sitsaw %>%
  subset(select = -c(2, 8, 10:14, 20, 24, 27)) #just fate/TL info

#population
sitspop <- readxl::read_xlsx("data/SubsPop.xlsx")
sitspop$popadj <- (sitspop$popadj + 1)
sitspop$logpop <- log(sitspop$popadj, base = 10)
#making pop a summed number rather than dens 1 may be more interpretative. 
#i.e. 1 person/50 km, 10 ppl/50km, 100 ppl/50 km, 1000 ppl/50 km etc.

sitspop$logpop[is.infinite(sitspop$logpop) & sitspop$logpop < 0] <- #if its Inf and -
  min(sitspop$logpop[!is.infinite(sitspop$logpop)], na.rm = TRUE) 
#replace with the minimum val in the dataset thats is NOT (!) Inf
sitspopsimp <- sitspop %>%
  subset(select = c("SUB","popadj", "logpop"))
#now we clean up protections and net fishing effort

# with simplified and merged fishing effort / protections
sifishsimp <- readxl::read_xlsx("data/SubsFP_processed.xlsx")
sizpred <- sitsfate %>% #from ModelSRC
  right_join(sifishsimp, by = c('SUB')) %>% #from 3_0_Fix/Prep etc.
  right_join(sitspopsimp, by = c('SUB')) #from just now!

#oof need to log rec effort and protections as well
sizpred <- sizpred %>% 
  mutate(logprotg = log(ProtAdd + 1), #protections at the rel year, with duplicates
         logRF = log(meanTrips + 1), #rel rec fishing effort for whole time period
         slopeDN = case_when(slopnDN < 10 ~ NA, #rel rec fishing effort for whole time period
                             .default = slopeDN)) %>%  #only if 10 years of fishing reported
  dplyr::select(-slopnDN) #just for filtering, remove

#plus all his children
sizpredc <- sizpred[(sizpred$Confirmed %in% TRUE),] #confirmed only

#fill some missing data by avgs
sizpredc <- sizpredc %>%
  group_by(Species_NB, Age_Class) %>%
  mutate(logTL2 = case_when(is.na(logTL) ~ mean(logTL, na.rm = TRUE), #fill missing vals with means 
                            .default = logTL),
         logTL2 = case_when(is.na(Age_Class) & is.na(Size_Final) |
                              is.na(Size_Final) & Species_NB %in% "Pristidae" ~ NA,  
                            .default = logTL2), #dont want to be using this for unk species level 
         Age_Class = case_when(Species_NB %in% "Pristis sp." & 
                                 Size_Final <=250 & Size_Final >= 120 ~ "juvenile",
                               .default = Age_Class)) %>% 
  ungroup() %>% 
  mutate(logaDT = case_when((is.na(logaDT) | logaDT <= 1) & !is.na(logaDTm) ~ logaDTm,
                            .default = logaDT)) #fill in the ones from npf

#also need some things that impact life history

predictors <- sizpredc[, c("SUB", "Cap_Year", "Cap_Mo", "Sub_Type",
                            "Species_NB", "Region",
                            "GPS_lat", "GPS_long",
                            "Gear",
                            "logaDT", "logmavDNm", "logaDNm", #mouth is working better than in the location
                            "slopeDN", #trend in fishery area
                            "logRF", "dist_Cl_RF", 
                            "logpop", "popadj", 
                            "ProtAdd", "logprotg", #just counts of trawl + net
                            "NetFree", "TrawlFree", #simple 0/1
                            "NetRank", "Wprot", #from rules list anthro
                            "protcurr", #current (Today)
                            "logTL2", "Size_Final", "Age_Class", #how big
                            "Alive2", "Sawless2"
                            )]

sitsiz <- sizpredc[!is.na(sizpredc$logTL2), ] #size known
sizpredks <- sitsiz[!(sitsiz$Species_NB %in% c("Pristidae", 'Pristis sp.')),]

write_xlsx(predictors, "data/processed/sitsmods.xlsx")
write_xlsx(sitsiz, "data/processed/sitsiz.xlsx")
write_xlsx(sizpredks, "data/processed/sizpredks.xlsx") # that's me!
