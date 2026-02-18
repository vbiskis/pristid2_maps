# Details ----
#' 04_mngam_2_smooth.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2 Feb 2025
#' - updated with new names for sep help fxns
#' - now operates with less dependencies/better efficiency, Dec 2025
#' Content: 
#' + check on what alt smoothing (k) does to each var
#' + pre-check for overdispersion
#' -----------

source('helpers/functions/src_mngam.R')
gammy <- readxl::read_xlsx("data/processed/gammy.xlsx") 

library(performance)
library(mclogit)

#for spec----
plots <- explore_mn_smooth(gammy, "Tmax") #k=8+
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "wetdry") #k=8+
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "oni") #k=5
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "logPrec") #k=8+
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "logoutlet") #k=3
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(glmset, "transout") #k=5
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "logDB") #k=5
do.call(grid.arrange, c(plots, ncol=2))
#interesting! because of the burdekin! 

plots <- explore_mn_smooth(gammy, "STRM_ORDER") #k=3
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "logflat") #k=5
do.call(grid.arrange, c(plots, ncol=2))

plots <- explore_mn_smooth(gammy, "logmg") #k=5
do.call(grid.arrange, c(plots, ncol=2))
#interesting! because of the burdekin! 

#Define smoothing
k3_vars <- c("logoutlet", "STRM_ORDER")
k5_vars <- c("logDB", "oni", "logflat")
k10_vars <- c("wetdry", "Tmax", "logPrec", "logmg")
factor_vars <- c("Flat", "MG")
