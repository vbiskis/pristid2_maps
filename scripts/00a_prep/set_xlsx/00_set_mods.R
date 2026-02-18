# Details ----
#' 00_set_mods.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-22
#' Content: 
#' + processing xlsx (original, as in 3c_Fate_GAM_1Set.R & 3b_Size_GAM_1Set.R)
#' + pulls in anthro vars (pt 2 size/fate) + enviros (pt 1 species)
#' + but now produces xlsx x 2, to be called directly for modelling
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl')

sitsvars <- readxl::read_xlsx("data/processed/sitsvars.xlsx") # get enviro vars
sitsmods <- readxl::read_xlsx("data/processed/sitsmods.xlsx") # anthro factors

# for enviro tests----

sitsvars <- sitsvars %>% 
  dplyr::select(SUB, Species_NB, everything()) %>% 
  left_join(sitsmods %>% 
              dplyr::select(SUB, Gear)) %>% 
  mutate(Gear = if_else(Gear %in% c("Other", "Unknown"), NA, Gear),
         across(where(is.character) & !matches("SUB"), factor),
         LocType = factor(LocType,
                           levels = 0:9, 
                           labels = c("Nearshore", "Foreshore", "Embayment",
                                      "Distrib. Mouth", "Basin Mouth",
                                      "Distrib. Main", "Basin Downstream",
                                      "Trib. Mouth", "Trib. Main", "Waterhole")),
         transout = if_else(Bord_In == 0, -1*sqrt(DTO), sqrt(DTO)),
         logoutlet = log(DTO + 0.05),
         logDB = log10(DB_area),
         logSB = log10(SB_area),
         logPrec = log(Precipitation + 1),
         logflat = log(Flat_dist + 0.05),
         logmg = log(MG_dist + 0.05),
         logsalt = log(Salt_dist + 0.05))

glimpse(sitsvars) # v nice how much!
sitsvars$Gear <- droplevels(as.factor(sitsvars$Gear))
habdf <- sitsvars[!(sitsvars$Species_NB %in% c("Pristidae", "Pristis sp.")), ]
habdf$Species_NB <- droplevels(as.factor(habdf$Species_NB))
mostacc <- habdf[!(habdf$GPS_type %in% c("3", "4")),] # perfect

write_xlsx(mostacc, "data/processed/sits_envmod.xlsx") # only accurate locations (not regional)

# for anthro tests----

sum(is.na(sitsmods$Alive2)) # not many in here (10)
sum(is.na(sitsmods$logTL2)) # ah lot more (88)
sum(is.na(sitsmods$Gear)) # ah lot more (88)
# modify and filter separately

modgams <- sitsmods %>% # lets begin
  mutate(Fate = if_else(Alive2 == TRUE, 1, 0),
         Alive2 = factor(Alive2, levels = c(TRUE, FALSE), labels = c("Alive", "Dead")),
         Gear = if_else(Gear %in% c("Other", "Unknown"), NA, Gear),
         Gear2 = fct_collapse(Gear, Net = c('Gillnet', 'Prawn Net')),
         Gear2 = factor(Gear2, levels = c("Cast Net", "Line", "Net", "Sighting Only")), #for plotting
         Region = fct_collapse(Region, CSEQ = c('CQ', 'SEQ'), WCY = c('WCY', 'TS')),
         across(where(is.character) & !matches("SUB"), factor),
         across(c(Gear, Gear2), droplevels),
         NetRank = ordered(NetRank, levels = c(0, 0.333, 0.667, 1, 1.333, 1.667, 2),
                           labels = rep(c("low", "medium", "high"), c(2, 3, 2)))) %>% 
  left_join(sitsvars %>% dplyr::select(SUB, wetdry, logPrec, Tmax, logDB, # and grab these as well!
                                     transout, logoutlet, MG, Salt, Flat,
                                     STRM_ORDER, logsalt, logmg, logflat), by = "SUB")

## checks----
table(modgams$Region, modgams$Species_NB)
table(modgams$Gear, modgams$Species_NB)
table(modgams$Gear2, modgams$Species_NB)

## filters----
modgams <- modgams %>% 
  mutate(across(c(MG, Salt, Flat, TrawlFree, NetFree, Sub_Type), as.factor),
      # across(c(TrawlFree, NetFree), as.logical), # no good for gams, but excellent for plots
         Gear = factor(Gear, levels = c('Cast Net', 'Line', 'Gillnet', 'Prawn Net', 
                                        'Sighting Only')))

fatedf <- modgams %>% filter(!is.na(Alive2))
glimpse(fatedf)

saveRDS(fatedf, "data/rds/sits_fatemod.RDS")

sizgam <- modgams %>% 
  filter(!Species_NB %in% c("Pristidae", "Pristis sp."), # no point without species
         !is.na(logTL2),
         !is.na(Gear2)) %>% 
  mutate(Species_NB = droplevels(Species_NB))
glimpse(sizgam)

saveRDS(sizgam, "data/rds/sits_sizemod.RDS")
