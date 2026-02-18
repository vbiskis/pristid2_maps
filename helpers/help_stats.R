# Details ----
#' help_stats.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - Split to sep script 2025-12-09 
#' Content: General settings for stats
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl',
               'GGally', 'partykit', 'vegan',
               'stats', 'car', 'psych', 'MASS',
               'DHARMa', 'gstat', 'marginaleffects', 
               'gratia', 'mgcv', 'e1071', 
               'performance', 'vcd')
