# Details ----
#' 00_set_teps.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Apr 2025
#' + updated 22 Dec to separate out the dataframe prep
#' + so it operates individually and don't need to reload a source script every time (how silly)
#' Content: 
#' + same graphic but
#' + decreasing bin # (as in analysis)
#' + and adding alpha for gear
#' -----------

#bring her in:
pacman::p_load('dplyr', 'readxl', 'writexl')

tep <- readxl::read_xlsx("data/comms/tep-interaction-data-2024-q3-update.xlsx")

# prep comms 1----
# no regions, Q only
unique(tep$Species)

tep <- tep %>% 
  mutate(Species_NB = case_when(Species == "Sawfish - Narrow" | Species == "Narrow Sawfish" |
                                  Species == "Narrow sawfish" ~ "A. cuspidata",
                                Species == "Sawfish - Dwarf" | Species == "Dwarf Sawfish" | 
                                  Species == "Dwarf sawfish" ~ "P. clavata",
                                Species == "Sawfish - Freshwater" | Species == "Sawfish - Wide" |
                                  Species == "Wide Sawfish" ~ "P. pristis",
                                Species == "Sawfish - Green" | Species == "Green Sawfish" | 
                                  Species == "Green sawfish" ~ "P. zijsron",
                                Species == "Sawfish - Unspecified" ~ "Pristidae",
                                .default = NA),
         Method = case_when(`Fishing Method` %in% c("Handline", 
                                                    "Ring Netting", 
                                                    "Potting (Crab)") ~ "Other",
                            .default = `Fishing Method`),
         Region = factor(Region, levels = c("GOC", "EC"))) %>% #or do I put wide in Pristidae?
  filter(!is.na(Species_NB))

#3 and check----

tep %>% 
  group_by(Region, Method, Species_NB) %>% 
  summarise(count = sum(`Total Interactions`), .groups = "drop")

# she looks great!

write_xlsx(tep, "data/processed/tep_intxns.xlsx")

#fate----
tep_fate <- tep %>% 
  group_by(Method, Region, Year, Species_NB, Fate) %>% 
  summarise(count = sum(`Total Interactions`), .groups = "drop") %>% 
  filter(Species_NB != "Pristidae",
         !(Species_NB == "P. clavata" & Region == "EC") & 
           !(Species_NB == "P. pristis" & Region == "EC")) %>%
  mutate(Method = factor(Method, levels = c("Gillnetting", 
                                            "Trawling",
                                            "Other"))) %>% 
  mutate_if(is.character, factor) %>%
  droplevels()

