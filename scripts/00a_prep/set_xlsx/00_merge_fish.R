# Details ----
#' 00_merge_fish.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2 Apr 2024
#' - modified 2025-12-13
#' -- moved the protection function to own script
#' Content: 
#' + processing xlsx (original, as in src_fish.R)
#' + but now produces xlsx, to be called directly
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl')

# to merge in
sitsaw <- readxl::read_xlsx("data/processed/sitsaw.xlsx") 
sitsspl <- readxl::read_xlsx("data/SubsEnv_Sptl.xlsx")

#fisheries
sitsfish <- readxl::read_xlsx("data/SubsFP.xlsx")
sitsfish %<>% 
  dplyr::mutate(GPS_type = as.factor(GPS_type),
                Sub_Type = as.factor(Sub_Type))

sitsfish <- sitsfish %>% 
  left_join(sitsspl %>% 
              dplyr::select(SUB, Mouth, DTM, DTO), by = "SUB") 

mouthgrids <- read_csv("data/output/MouthGRID.csv")

sitsfish <- sitsfish %>% 
  left_join(mouthgrids %>% 
              dplyr::select(River_Name, SubBasin, GRID_CODE) %>%  #grab grid_code in case get an NA :(
            rename(GRID_MOUTH = GRID_CODE) %>% 
              mutate(Mouth = case_when(River_Name %in% "S Burdekin" ~ "Lower Burdekin River",
                                       River_Name %in% "Jacky Jacky" ~ "Jacky Jacky Creek",
                                       River_Name %in% "Tully Inlet" ~ "Cliffdale Creek",
                                       River_Name %in% "Noosa Inlet" ~ "Noosa River",
                                       River_Name %in% "Oconnell River" ~ "O'Connell River",
                                       River_Name %in% "Water Park Creek" ~ "Waterpark Creek",
                                       River_Name %in% "Johnstone River" ~ "South Johnstone River",
                                       River_Name %in% "Mary River" ~ "Upper Mary River",
                                       .default = River_Name)), 
            by = "Mouth") %>% 
  mutate(GRID_MOUTH = case_when(Mouth %in% "North Pine River" ~ 'W37', #doing this one manually bc two pines was causing trouble
         .default = GRID_MOUTH))

sitsfish <- sitsfish %>% 
  group_by(SubBasin) %>%
  mutate(meanTrips = case_when(
    DTO*Bord_In > 30 ~ min(meanTrips, na.rm = TRUE),
    .default = meanTrips #make sure net days is only for downstream (not accurate up)
  )) %>%
  ungroup() 

# View(sitsfish[, c('Mouth', 'DTM', 'GRID_CODE', 'GRID_MOUTH')]) #check it worked

#apply a function for scoring protections
source('helpers/functions/src_assign_protlev.R')

# Apply row-wise scoring
sitsfish <- sitsfish %>%
  rowwise() %>%
  mutate(ProtAdd = assign_score(across(everything())))

summary(sitsfish$ProtAdd) #matches what I expected

#too much - main points
sifishsimp <- sitsfish %>%
  dplyr::select("SUB", "Sub_Type",  "Cap_Year", "GPS_lat", "GPS_long", "Bord_In", #basics
                    "meanTrips","dist_Cl_RF", "RBF_20km", #RF effort
                    "NetFree", "TrawlFree", "Authority",
                    "ProtAdd", "Wprot", "NetRank", #main prot measures to check
                    "ZoneType", "IUCN", "InldIUCN", #IUCN
                    "InldProt", "NoNet", "NoTrl", 
                    "IIZone", "IVZone", "FHA", "DPA", #extra net fun
                    "GRID_CODE", "GRID_MOUTH", "DaysNet") # cf effort

write_xlsx(sifishsimp, "data/processed/sitsfish1.xlsx") # to go into processing stage
