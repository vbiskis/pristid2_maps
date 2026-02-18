# Details ----
#' 06_sizgam_pp.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-27
#' Content: 
#' + applying automated script to built multiple gams simultaneously
#' Output:
#' final model for P. pristis TL 
#' diagnostic plots (S11)
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/functions/src_gam_fxns.R')
theme_set(mytheme)

#source('helpers/help_map.R')
#source('helpers/functions/src_autocorr.R')

sizgam <- readRDS('data/rds/sits_sizemod.RDS')
glimpse(sizgam)

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat, NetFree), as.factor))

psize <- sizgam %>% 
  filter(Species_NB == "P. pristis") 

# mods----

pp_mods <- list(
  modpg = logTL2 ~ Gear,
  modpy = logTL2 ~ s(Cap_Year), # more imp than gear
  modp0 = logTL2 ~ Gear + s(Cap_Year),
  modp1 = logTL2 ~ s(Cap_Year, by = Gear, k = 5) + Gear,
  modp2 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc"), # no gear for this one
  modp3 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(logDB),
  modp4 = logTL2 ~ s(Cap_Year) + te(Cap_Mo, logDB, bs = c("cc", "tp")),
  modp5 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(transout),
  modp6 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(transout) + s(logDB),
  modp7 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + te(logDB, transout),
  modp8 = logTL2 ~ s(Cap_Year) + te(logDB, transout), # check that month even matters
  modp9 = logTL2 ~ s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")),
  modp10 = logTL2 ~ s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB),
  modp11 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + te(logDB, transout) + s(logaDNm, k = 9),
  modp12 = logTL2 ~ Gear + s(Cap_Year) + s(Cap_Mo, bs = "cc") + te(logDB, transout),
  modp13 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + te(logDB, transout) + NetFree,
  modp14 = logTL2 ~ s(Cap_Year, by = NetFree) + s(Cap_Mo, bs = "cc") + te(logDB, transout), 
  modp15 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + te(logDB, transout) + s(logpop), # try em all
  modp16 = logTL2 ~ s(Cap_Year) + s(transout) + s(logDB),
  modp17 = logTL2 ~ Gear + s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(transout) + s(logDB),
  modp18 = logTL2 ~ s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(transout) + s(logDB) + NetFree
)

pristis_size <- fit_gam(psize, pp_mods, select = FALSE)  # okay cool thats pretty obvious then

# compare----
pristis_size$comparison %>% arrange(desc(dev_expl)) 
pristis_size$comparison %>% arrange(AIC) 
pristis_size$comparison %>% arrange(BIC) 

# Test nested comparisons
compare_nested(pristis_size$models, list(
  c("modpy", "modp0"), # year or gear'
  c("modp2", "modp3"), # adding in db etc.
  c("modp3", "modp4"), # intxn 1 - ns
  c("modp5", "modp6"), # does adding in db help? ## yes
  c("modp6", "modp7"), # intxn ## yes
  c("modp7", "modp11"), # top two, no diff
  c("modp7", "modp13"), # yes better without
  c("modp7", "modp14"), # nope
  c("modp7", "modp15") # nope
))

## top model----
best_pp_name <- (pristis_size$comparison %>% arrange(AIC))$model[7]
best_pp_mod <- pristis_size$models[[which(map_chr(pristis_size$models, "name") == best_pp_name)]]$model

check_model(best_pp_mod, "Best by AIC/BIC") 
gam.check(best_pp_mod) # nah edf not close to k'
draw(best_pp_mod) # pre-check

# plot----
appraise(best_pp_mod) & theme(plot.title = element_text(size = 14, family = "Optima",
                                                        margin = margin(t = 5, b = 10, unit = "pt")),
                              plot.subtitle = element_text(margin = margin(b = 5, unit = "pt")),
                              plot.margin = margin(l = 2, t = 0, b = 5, r = 10, unit = "pt"),
                              axis.title.y = element_text(margin = margin(r = 5, unit = "pt")))

## record mod----
saveRDS(best_pp_mod, 'data/rds/ppmod.RDS')

ggsave(
  "figs/supps/s7/s7_modpsiz.png",
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
