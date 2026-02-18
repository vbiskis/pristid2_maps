# Details ----
#' 00_merge_env.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 20 Sep 2024 (when updated for climate, originally created March 2024)
#' - modified 2025-12-13
#' Content: 
#' + processing xlsx (original, as in src_env.R)
#' + but now produces 4x xlsx, only as necessary
#' ++ will likely streamline/split further later for clarity
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl')

# some old friends----
sitsaw <- readxl::read_xlsx("data/processed/sitsaw.xlsx")

# water chem and flow----
sitsenv <- readxl::read_xlsx("data/SubsEnv_River.xlsx")
sitsenv <- sitsenv %>%  
  dplyr::mutate(GPS_type = as.factor(GPS_type))

## extra calcs----
sitsflow <- sitsenv %>% 
  dplyr::select(-Cap_Year) %>% 
  dplyr::right_join(sitsaw %>% 
                      dplyr::select(SUB, Confirmed, Species_NB, Cap_Year), 
                    by = c('SUB')) %>% 
  mutate(# vol
    relannflow = vol_year/vol_yearavg,
    relflowy = case_when(relannflow > 1.1 ~ "wet year",
                         relannflow < 0.9 ~ "dry year",
                         is.na(relannflow) ~ NA,
                         .default = "typical"),
    relmoflowv = vol_month/vol_moavg, #vol
    relmoflow = case_when(relmoflowv == 0 ~ watlvl_mo/watlvl_moavg, #wl
                          is.na(relmoflowv) ~ NA,
                          .default = relmoflowv),
    relflowm = case_when(relmoflow >= 1 ~ "bigger",
                         relmoflow < 1 ~ "smaller",
                         .default = NA))%>% 
  dplyr::select(SUB, Confirmed, Species_NB, Cap_Year, everything(), 
                -clos_stat_flow, -dist_stat_flow, -watlvl_mo, -watlvl_year, 
                -relmoflowv, -SSanom)

## take only relevant----
sitsenvj <- sitsflow %>%
  dplyr::select(-c(28:39, 41)) %>% 
  dplyr::right_join(sitsaw %>% 
                      dplyr::select(SUB, Region,
                             Size_Final, logTL, Est_Age, 
                             Age_Class, Maturity), 
                    by = c('SUB'))

## add in climate/season trends----
wetszns <- readxl::read_xlsx("data/output/wetszn_site.xlsx")
wetszns <- wetszns %>% 
  left_join(sitsenvj %>% 
              dplyr::select(SUB), by = "SUB") %>% 
  mutate(Season = case_when(Cap_Mo < startwet & Cap_Mo >= endwet ~ "dry",
                            Cap_Mo < startwet & Cap_Mo < endwet ~ "wet",
                            Cap_Mo >= startwet & Cap_Mo > endwet ~ "wet",
                            .default = NA)) #adjusting bc they end later

ENLN <- readxl::read_xlsx("data/enviro/ENLN.xlsx")
sitsenvj$oni <- unname(unlist(mapply(function(year, month) {
  if (!is.na(year) && !is.na(month)){
    value <- ENLN[ENLN$Year == year, month + 1]  
    return(value)
  } else {
    return(NA)
  }
}, sitsenvj$Cap_Year, sitsenvj$Cap_Mo)))

sitsenvj <- sitsenvj %>% 
  left_join(wetszns %>% 
              dplyr::select(SUB, Season, sinwd), 
            by = "SUB") %>% 
         mutate(climev = case_when(is.na(oni) ~ NA_character_,
                             oni < -0.5 ~ "LN",
                             oni > 0.5 ~ "EN", 
                             .default = "neutral"),
         ce_strth = case_when(is.na(oni) ~ NA_character_,
                              abs(oni) > 0.5 & abs(oni) <= 1 ~ "weak",
                              abs(oni) > 1 & abs(oni) <= 1.5 ~ "mod",
                              abs(oni) > 1.5 ~ "strong",
                              .default = "neutral")) %>% 
        rename(wetdry = sinwd) %>% 
  dplyr::select(SUB, Species_NB, Confirmed, TimeStamp, Region,
                Season, wetdry, everything())

# sheet 2----
# distances
sitsspl <- readxl::read_xlsx("data/SubsEnv_Sptl.xlsx")
sitsspl <- sitsspl %>% 
  dplyr::mutate(GPS_type = as.factor(GPS_type),
                SB_area = as.numeric(SB_area),
                DB_area = as.numeric(DB_area))

sitsneed <- sitsaw %>%
  subset(select = c("SUB", "Species_NB", "Sub_Type", "Confirmed",
                    "Region", "Cap_Year", "Cap_Mo", 
                    "Size_Final", "logTL", "Maturity")) #short version

sitssplj <- sitsneed %>%
  dplyr::left_join(sitsspl, by = c('SUB')) %>% 
  dplyr::select(1:3, TimeStamp, Cap_Year, Cap_Mo, Size_Final, logTL, Maturity, everything()) #add species info

sitssplj$logDB <- log(sitssplj$DB_area) #log transform these massive guys
sitssplj$logRL <- log(sitssplj$RivLg)
sitssplj %<>% 
  mutate(across(c(where(is.character),
                  -SUB),
                factor)) %>%
  dplyr::mutate(RivType = as.factor(RivType),
                LocType = as.factor(LocType),
                FlatTYPE = as.factor(FlatTYPE),
                Bord_In = as.factor(Bord_In))

##tides----
sitstemp <- readxl::read_xlsx("data/SubsEnv_Temp.xlsx")
sitstemp %<>% 
  dplyr::mutate(GPS_type = as.factor(GPS_type))

sitstp <- sitsneed %>%
  dplyr::right_join(sitstemp, by = c('SUB')) %>%
  dplyr::select(-SolarNoon, -Sunrise, -Sunset, -LOD) %>% 
  rename(fourseas = Season) %>% 
  mutate(Tide_State = case_when(
    Tide_State %in% c("low", "low tide") ~ "Low",
    Tide_State == "high" ~ "High",
    Tide_State == "incoming" ~ "Incoming",
    Tide_State == "outgoing" ~ "Outgoing",
    Tide_State == "non-tidal" ~ "Non-tidal",
  ))

sitstp <- sitstp %>% 
  left_join(sitsenvj %>% 
              dplyr::select(SUB, wetdry, Season),
            by = "SUB") 

#env models----
goodvars <- sitsenvj %>%
  dplyr::select(c(1:36, 42:44)) %>% 
  left_join(sitssplj %>% 
              dplyr::select(SUB, DTO, DTM, DTB,
                            RivLg, DB_area, SB_area,
                            Salt_dist, Flat_dist, MG_dist,
                            Salt, Flat, MG, 
                            RivType, LocType, STRM_ORDER), by = 'SUB') %>% 
  mutate(DTO_IO = case_when(Bord_In == 0 ~ DTO * -1,
                            .default = DTO))

goodvars <- goodvars %>% #need 2f_tides if you want to do this
  dplyr::left_join(sitstp %>% 
                     dplyr::select(SUB, Tide_State,
                                   TF_SS, TF_SR, Time_C1, 
                                   TFL_min, TFH_min, Tide,
                                   df_full, df_new, df_sea_ch), 
                   by = 'SUB')

##and last subset----

# just keep names simple! we don't need unconfirmed ones
sitstp <- sitstp[(sitstp$Confirmed %in% TRUE),] #confirmed only - time
sitsspl <- sitssplj[(sitssplj$Confirmed %in% TRUE),] #confirmed only - spatial
sitsenv <- sitsenvj[(sitsenvj$Confirmed %in% TRUE),] #confirmed only - enviro
sitsvars <- goodvars[(goodvars$Confirmed %in% TRUE),] #confirmed only - got em all!

write_xlsx(sitstp, "data/processed/sitstp.xlsx")
write_xlsx(sitsspl, "data/processed/sitsspl.xlsx")
write_xlsx(sitsenv, "data/processed/sitsenv.xlsx")
write_xlsx(sitsvars, "data/processed/sitsvars.xlsx")

sitsrivs <- sitsspl %>% #fix it!
  filter(!(GPS_type %in% c('3', '4'))) %>% 
  dplyr::select(SUB, Bord_In, DTB:last_col()) %>% 
  left_join(sitsenv %>% 
              dplyr::select(SUB:Temp_wat, Depth:Elev, vol_moavg:last_col()), #id have to go back and check what these even are
            by = "SUB") %>% 
  mutate(DTO_IO = case_when(Bord_In == 0 ~ DTO * -1,
                            .default = DTO)) %>% 
  dplyr::select(SUB, Species_NB:last_col(), everything())

write_xlsx(sitsrivs, "data/processed/sitsrivs.xlsx")