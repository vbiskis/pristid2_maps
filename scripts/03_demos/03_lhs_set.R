# Details ----
#' 03_lhs_set.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - New new 2025-12-13
#' Content: 
#' + setting a df that can be called for demography analysis
#' ++ just a buncha filtering
#' -----------

pacman::p_load('dplyr', 'tidyverse', 
               'readxl', 'writexl')
              
sitsspl <- readxl::read_xlsx("data/processed/sitsspl.xlsx") 
sitsmods <- readxl::read_xlsx("data/processed/sitsmods.xlsx") 

sitsmods$Species_NB <- as.factor(sitsmods$Species_NB)
sitsmods$Species_NB <- droplevels(sitsmods$Species_NB)

sitslhs <- sitsmods %>% 
  mutate(Gear2 = fct_collapse(Gear, Net = c("Gillnet", "Prawn Net"), #collapsed for net or no
                              Line = c("Line", "Cast Net"),
                              Other = c("Unknown", "Sighting Only", "Other")),
         LHS = factor(Age_Class, levels = c('YOY', "juvenile", "sub-adult", "adult")),
         YOY = if_else(LHS == "YOY", 1, 0),
         Juv = if_else(LHS %in% c('YOY', 'juvenile'), 1, 0),
         Maturity = if_else(LHS == "adult", "Mature", "Immature"),
         ColY2 = cut(Cap_Year, # so now we got 3!
                     breaks = c(1983, 2011, 2018, 2024),
                     labels = c("1983-2010", "2011-2017", "2018-2023"),
                     right = FALSE)) %>% 
  dplyr::select(SUB, Cap_Year, ColY2, Species_NB, Size_Final, YOY, Juv, Maturity, LHS, Gear2, NetFree)

sitslhs <- sitslhs %>% 
  left_join(sitsspl %>% 
              dplyr::select(SUB, Sub_Type, Region, Dbasin, RiverName)) %>%  #k were ready!
  mutate(Dbasin = factor(Dbasin, levels = rev(c('Jardine', 'Ducie', 'Wenlock', 'Embley', #WCY
                                                'Archer', 'Watson', 'Holroyd', 'Mitchell', #NGoC
                                                'Staaten', 'Gilbert', 'Norman', 'Flinders', #GoC
                                                'Leichhardt', 'Nicholson', 'Settlement', #SGoC
                                                'Jacky Jacky', 'Olive-Pascoe', 'Stewart', 'Normanby', 'Jeannie', 'Endeavor', #CY
                                                'Mossman', 'Barron', 'Johnstone', 'Murray', 'Herbert', #FNQ
                                                'Ross', 'Burdekin', 'Don', 'Haughton', 'Proserpine', 'Pioneer', #NEQ
                                                'Plane', 'Styx', 'Waterpark', 'Fitzroy', 'Baffle', 'Mary'))),
         ECWC = if_else(Region %in% c('WCY', 'GoC'), 'W', 'E'),
         Coast = factor(ECWC, levels = c('W', 'E'))) #CEQ

sitsdemo <- sitslhs %>%
  filter(!is.na(LHS),  # need to cluster based on the number of known reported LHS
         Sub_Type == 'recent') %>%  #only want reports from the last 15 years!
  mutate(LHS = case_when(Species_NB == 'A. cuspidata' & Size_Final >= 220 ~ 'adult', # remove-sub adult for this species (not meaningful)
                         Species_NB == 'A. cuspidata' & Size_Final < 220 & Size_Final > 130 ~ 'juvenile', 
                         .default = LHS),
         Species_NB = if_else(Species_NB == "Pristis sp.", "Pristidae", Species_NB))

write_xlsx(sitslhs, "data/processed/sitslhs.xlsx")
write_xlsx(sitsdemo, "data/processed/sitsdemo.xlsx")

## counts for paercentage tests
basin_lhs <- sitsdemo %>% # by species spec
  filter(Species_NB != 'Pristidae') %>% 
  group_by(Coast, Region, Dbasin, Species_NB) %>%
  summarise(
    n_total = n(),
    prop_yoy = sum(LHS %in% c("YOY")) / n_total,
    prop_juv = sum(LHS %in% c("YOY", "juvenile")) / n_total,
    prop_imm = sum(LHS != "adult", na.rm = TRUE) / n_total,
    prop_ad = sum(LHS == "adult", na.rm = TRUE) / n_total,
    .groups = "drop"
  ) %>%
  filter(n_total > 1)

saveRDS(basin_lhs, 'data/rds/basin_lhs.RDS')

# and just individual points:
alllhsbas <- sitsdemo %>%
  filter(Species_NB != 'Pristidae') %>% 
  group_by(Species_NB, Coast, Region, Dbasin) %>% #oh filtering misses some key things!
  summarise(
    n_total = n(),
    n_yoy = sum(LHS %in% c("YOY")),
    n_juv = sum(LHS %in% c("YOY", "juvenile")),
    n_imm = sum(LHS != "adult", na.rm = TRUE),
    n_ad = sum(LHS == "adult", na.rm = TRUE),
    .groups = "drop"
  ) 

saveRDS(alllhsbas, 'data/rds/alllhsbass.RDS')
