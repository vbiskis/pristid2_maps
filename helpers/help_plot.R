# Details ----
#' help_plot.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2025 Dec 11
#' Content: General settings for plots
#' Quick access to themes, fonts and colours etc.
#' Easy, consistent visuals!
#' -----------

# libraries----
pacman::p_load('dplyr', 'tidyverse', 'readxl',
               'ggplot2', 'cowplot',
               'patchwork', 'ggpubr', 
               'grid', 'gridExtra',
               'ggfortify', 'ggmosaic',
               'pilot', 'MetBrewer')

# colours----
spec_col_fam<- c(
  "A. cuspidata" = "blue4",
  "P. clavata" = "gold2",
  "P. pristis" = "red3",
  "P. zijsron" = "darkgreen",
  "Pristidae" = "grey50"
)

spec_col_all <- c(
  "A. cuspidata" = "blue4",
  "P. clavata" = "gold2",
  "P. pristis" = "red3",
  "P. zijsron" = "darkgreen",
  "Pristis sp." = "purple",
  "Pristidae" = "grey50"
)

spec_col_ks <- c(
  "A. cuspidata" = "blue4",
  "P. clavata" = "gold2",
  "P. pristis" = "red3",
  "P. zijsron" = "darkgreen"
)

# labs----

spec_labs_fam <- c(
  expression(italic("A. cuspidata")), 
  expression(italic("P. clavata")),
  expression(italic("P. pristis")),
  expression(italic("P. zijsron")),
  expression("Pristidae")
)

spec_labs_all <- c(
  expression(italic("A. cuspidata")), 
  expression(italic("P. clavata")),
  expression(italic("P. pristis")),
  expression(italic("P. zijsron")),
  expression(italic("Pristis sp.")),
  expression("Pristidae")
)

spec_labs_ks <- c(
  "A. cuspidata" = expression(italic("A. cuspidata")), 
  "P. clavata" = expression(italic("P. clavata")),
  "P. pristis" = expression(italic("P. pristis")),
  "P. zijsron" = expression(italic("P. zijsron"))
)

# theme----
suppressWarnings({mytheme <- pilot::theme_pilot(axis_title_size = 12, legend_title_size = 12,
                              legend_title_family = "Optima", legend_text_family = "Optima",
                              facet_title_family = "Optima", facet_title_size = 12,
                              subtitle_family = "optima",
                              axis_title_family = "Optima", axis_text_family = "Optima") 
}) # my god shes loud
  
west_basins <- rev(c('Ducie', 'Wenlock', 'Embley',
                     'Archer', 'Watson', 'Mitchell',
                     'Staaten', 'Gilbert', 'Norman', 'Flinders',
                     'Leichhardt', 'Nicholson'))

east_basins <- rev(c('Olive-Pascoe', 'Stewart', 'Normanby', 
                     'Endeavor', 'Mossman', 'Barron', 'Johnstone', 
                     'Murray', 'Herbert', 'Ross', 'Burdekin', 'Don', 'Haughton', 
                     'Proserpine', 'Pioneer', 'Plane', 'Styx', 'Waterpark', 
                     'Fitzroy'))

