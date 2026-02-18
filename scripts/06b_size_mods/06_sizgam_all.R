# Details ----
#' 06_sizgam_all.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Feb 2025
#' - modified 2025-12-24
#' Content: 
#' + pulled out the clustering tests etc.
#' + built automated script to built multiple gams simultaneously
#' ++ then compare top ones
#' ++ diagnostics on sep script (06_sizgam_diag.R)
#' -----------

source('helpers/help_stats.R')
source('helpers/functions/src_gam_fxns.R')

sizgam <- readRDS('data/rds/sits_sizemod.RDS')
glimpse(sizgam)

# all model options----

size_mods <- list(
  # basic 
  base = logTL2 ~ Gear,
  gear_year = logTL2 ~ Gear + s(Cap_Year),
  gear_month = logTL2 ~ Gear + s(Cap_Mo, bs = "cc"),
  
  # species
  mod1 = logTL2 ~ Species_NB,
  mod2 = logTL2 ~ s(Cap_Year),
  mod3 = logTL2 ~ Cap_Year + Species_NB,
  mod4 = logTL2 ~ Cap_Year*Species_NB,
  mod5 = logTL2 ~ s(Cap_Year, by = Species_NB, bs = "fs") + Species_NB,
  mod6 = logTL2 ~ s(Cap_Year) + s(Cap_Mo) + Species_NB,
  
  # and season
  modmo = logTL2 ~ Cap_Year + s(Cap_Mo, by = Species_NB, bs = "cc") + Species_NB,
  modmo1 = logTL2 ~ s(Cap_Year, by = Species_NB) + s(Cap_Mo, by = Species_NB, bs = "cc") + Species_NB,
  modmo2 = logTL2 ~ Cap_Year*Species_NB + s(Cap_Mo, by = Species_NB, bs = "cc"),
  
  # distance
  mod7 = logTL2 ~ Cap_Year + Species_NB + transout,
  mod8 = logTL2 ~ Cap_Year + Species_NB + s(transout, by = Species_NB),
  mod9 = logTL2 ~ Cap_Year + Species_NB + transout + s(Cap_Mo, by = Species_NB, bs = "cc"),
  mod10 = logTL2 ~ Cap_Year + Species_NB + s(Cap_Mo, by = Species_NB, bs = "cc") + s(transout, by = Species_NB),
  mod11 = logTL2 ~ Cap_Year + Species_NB + te(Cap_Mo, transout, by = Species_NB, bs = c("cc", "tp")),
  mod12 = logTL2 ~ Cap_Year*Species_NB + te(Cap_Mo, transout, by = Species_NB, bs = c("cc", "tp")),
  mod13 = logTL2 ~ Cap_Year + Species_NB + transout + s(wetdry, by = Species_NB),
  mod14 = logTL2 ~ Cap_Year + Species_NB + te(wetdry, transout, by = Species_NB),
  
  # region
  modr1 = logTL2 ~ Cap_Year + Species_NB + s(transout) + s(GPS_lat),
  
  # enviro
  mod15 = logTL2 ~ Cap_Year + Species_NB + transout + te(logPrec, Tmax, by = Species_NB),
  mod16 = logTL2 ~ Cap_Year + Species_NB + te(transout, logPrec, by = Species_NB),
  mod17 = logTL2 ~ Cap_Year + Species_NB + transout + te(wetdry, logDB, by = Species_NB),
  mod18 = logTL2 ~ Cap_Year + Species_NB + te(Cap_Mo, logDB, bs = c("cc", "tp"), by = Species_NB),
  mod19 = logTL2 ~ Cap_Year + transout + s(wetdry, by = Species_NB) + Species_NB*MG,
  mod20 = logTL2 ~ Cap_Year + te(transout, wetdry, by = Species_NB) + Species_NB*MG,
  mod21 = logTL2 ~ Cap_Year + Species_NB + te(Cap_Mo, STRM_ORDER, by = Species_NB, bs = c("cc", "tp")),
  mod22 = logTL2 ~ Cap_Year + Species_NB*STRM_ORDER + s(Cap_Mo, by = Species_NB, bs = "cc"),
  
  # gear
  modg = logTL2 ~ Gear + Species_NB,
  modg0 = logTL2 ~ s(Cap_Year) + Gear + Species_NB,
  modg1 = logTL2 ~ s(Cap_Year) + Gear + Species_NB + transout,
  modg2 = logTL2 ~ s(Cap_Year) + Gear + Species_NB + transout + s(Cap_Mo, by = Species_NB, bs = "cc"),
  
  # anthro
  modf0 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + te(Cap_Mo, transout, by = Species_NB, bs = c("cc", "tp")),
  modf1 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + s(Cap_Mo, by = Species_NB, bs = "cc") + s(logDB),
  modf2 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + transout + s(Cap_Mo, by = Species_NB, bs = "cc") + s(logDB),
  modf3 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + transout + s(Cap_Mo, by = Species_NB, bs = "cc") + s(logDB) + s(logaDNm),
  modf4 = logTL2 ~ s(Cap_Year, by = Gear, k = 5) + Gear + s(logaDNm) + transout + s(Cap_Mo, by = Species_NB, bs = "cc") + Species_NB + s(logDB),
  modf5 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + transout + s(logpop) + s(Cap_Mo, by = Species_NB, bs = "cc") + s(logDB) + s(logaDNm),
  modf6 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + te(Cap_Mo, logDB, by = Species_NB, bs = c("cc", "tp")) + s(transout) + s(logaDNm),
  modf7 = logTL2 ~ Gear + Species_NB + s(transout) + te(Cap_Year, logaDNm) + s(Cap_Mo, by = Species_NB, bs = "cc") + s(logDB),
  modf8 = logTL2 ~ Gear + Species_NB + s(transout) + te(Cap_Year, logaDNm) + te(Cap_Mo, logDB, by = Species_NB, bs = c("cc", "tp")),
  modf9 = logTL2 ~ s(Cap_Year, k = 5) + Gear + Species_NB + te(Cap_Mo, transout, by = Species_NB, bs = c("cc", "tp")) + s(logDB) + s(logaDNm),
  modf10 = logTL2 ~ Species_NB + Gear2 + te(Cap_Mo, logDB, by = Species_NB, bs = c("cc", "tp")) +  s(transout) + s(logpop) + te(Cap_Year, logaDNm)
)

# get output----

allspec <- fit_gam(sizgam, size_mods, select = FALSE) 

allspec$comparison %>% arrange(desc(dev_expl)) # all the anthro mods
allspec$comparison %>% arrange(AIC) # and again

# Test nested comparisons
compare_nested(allspec$models, list(
  c("modf6", "modf8"), 
  c("modf3", "modf6"), # theyre sig but will explore by spec sep (not when select = FALSE)
  c("modf3", "modf9"), # close (but not really enough)
  c("modf8", "modf10"), 
  c("modf3", "modf4"), # gear intxn no
  c("modf3", "modf5"), # population no
  c("modf0", "modf3"),
  c("modf1", "modf3"),
  c("modf2", "modf3") # okay! so 3 it is!
))

# f7 worse (with intxn of year/days net)
# f4 and 5 (gear and pop) inc not sig  
# f9 not sig, dist and month
# f6 borderline (db & month)
# with this intxn included, then the addn of year/days net + pop ARE sig
# but from appraisal plots, very overfit. 

sizmodnam <- (allspec$comparison %>% arrange(AIC))$model[5] # ignoring the crazy ones
sizmod <- allspec$models[[which(map_chr(allspec$models, "name") == sizmodnam)]]$model

check_model(sizmod, "Best by AIC/Dev")

appraise(sizmod) & theme(plot.title = element_text(size = 14, family = "Optima",
                                                        margin = margin(t = 5, b = 10, unit = "pt")),
                              plot.subtitle = element_text(margin = margin(b = 5, unit = "pt")),
                              plot.margin = margin(l = 2, t = 0, b = 5, r = 10, unit = "pt"),
                              axis.title.y = element_text(margin = margin(r = 5, unit = "pt")))

# save resid plot----

ggsave(
  "figs/supps/s5/3s5_modallsiz.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

# record mod----
saveRDS(sizmod, 'data/rds/sizmod.RDS')

## also save his silly buddy:----
sizmod2 <- (allspec$comparison %>% arrange(desc(dev_expl)))$model[1] 
stackedmod <- allspec$models[[which(map_chr(allspec$models, "name") == sizmod2)]]$model
saveRDS(stackedmod, 'data/rds/modf10.RDS')

# check semivar----
## move to diag----
filt_df <- sizgam %>% 
  filter(!is.na(logaDNm), !is.na(Cap_Mo))

semivar(sizmod, dat = filt_df, lat = "GPS_lat", lon = "GPS_long")

