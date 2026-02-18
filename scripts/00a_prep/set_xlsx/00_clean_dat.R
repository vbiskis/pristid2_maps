# Details ----
#' 00_clean_dat.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - Split to two scripts 2025-12-10
#' Content: 
#' + processing xlsx
#' + sep from helper code (original, src_dist.R)
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl')

# Some new friends----

#main sawfish details
sitsaw <- readxl::read_xlsx("data/Subs1.xlsx") 
#read in full sightings with spatial data from QGIS

sitsaw %<>% 
  mutate(across(c(where(is.character),
                  -SUB),
                factor),
         GPS_type = as.factor(GPS_type),
         Sub_Type = case_when(Cap_Year < 2009 ~ 'historic',
                              Cap_Year > 2008 ~ 'recent',
                              .default = Sub_Type),
         logTL = log(Size_Final)) #prep correct structure

sitsaw <- sitsaw %>%
  mutate(Gear = case_when(Cap_Met %in% c("handline", "hookline") ~ "Line",
                          Cap_Met == "gill net" ~ "Gillnet",
                          Cap_Met == "prawn net" ~ "Prawn Net",
                          Cap_Met == "cast net" ~ "Cast Net",
                          Cap_Met == "unknown" ~ "Unknown",
                          Alive == "Sighting Only" ~ "Sighting Only",
                          TRUE ~ "Other"),
         Gear = case_when(Alive == "Dead on Beach" & Gear == "Unknown" ~ "Sighting Only",
                          .default = Gear))

sitsaw$Gear <- as.factor(sitsaw$Gear)

#all the filtered versions
sitsawc <- sitsaw[(sitsaw$Confirmed %in% TRUE),] #confirmed only
sitsawks <- sitsawc[!(sitsawc$Species_NB %in% c("Pristidae", 'Pristis sp.')),] #remove unknown species level
sitsawnp <- sitsawc[!(sitsawc$Species_NB %in% c('Pristis sp.')),] #no pristis

write_xlsx(sitsaw, "data/processed/sitsaw.xlsx")
write_xlsx(sitsawc, "data/processed/sitsawc.xlsx")
write_xlsx(sitsawks, "data/processed/sitsawks.xlsx")

