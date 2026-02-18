# Details ----
#' 06_sizgam_pz.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-27
#' Content: 
#' + applying automated script to built multiple gams simultaneously
#' Output:
#' final model for P. zijsron TL 
#' diagnostic plots (S12)
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

zsize <- sizgam %>% 
  filter(Species_NB == "P. zijsron") 

# mods----

pz_mods <- list(
  modzg = logTL2 ~ Gear,
  modzy = logTL2 ~ s(Cap_Year), # more imp than gear
  modz0 = logTL2 ~ s(Cap_Year) + Gear,
  modz1 = logTL2 ~ s(Cap_Year, by = Gear) + Gear,
  modz2 = logTL2 ~ s(Cap_Year) + Gear + s(Cap_Mo, bs = "cc"),
  modz3 = logTL2 ~ s(Cap_Year) + Gear + s(logDB),
  modz4 = logTL2 ~ s(Cap_Year) + Gear + s(transout),
  modz5 = logTL2 ~ s(Cap_Year) + Gear + s(transout) + s(logDB),
  modz6 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout), # def improvement
  modz7 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout) + s(Cap_Mo, bs = "cc"),
  modz8 = logTL2 ~ s(Cap_Year) + Gear + s(transout) + s(Cap_Mo, bs = "cc"),
  modz9 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")),
  modz10 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k = 5),
# modz11 = logTL2 ~ s(Cap_Year) + Gear + s(logDB) + s(Cap_Mo, bs = "cc"),
# modz12 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, logDB, bs = c("cc", "tp")),
# modz13 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, logDB, bs = c("cc", "tp")) + s(transout),
  modz14 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout) + s(logaDNm),
  modz15 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout) + NetFree, # also improvement
  modz16 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout, by = NetFree) + NetFree, 
  modz17 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout) + s(logpop),
  modz18 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB) + s(logaDNm),
  modz19 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB) + NetFree,
  modz20 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB) + NetFree + s(logaDNm),
  modz21 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, by = NetFree) +  NetFree,
  modz22 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp"), by = NetFree) + s(logDB) +  NetFree,
  modz23 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logpop, k = 3), # where it's real wiggly it just gets silly
# modz24 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logpop) + NetFree, 
  modz25 = logTL2 ~ s(Cap_Year) + Gear + s(transout) + s(Cap_Mo, bs = "cc") + s(logDB, k = 5),
  modz26 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + NetFree,
  modz27 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp"), by = NetFree) + NetFree,
  modz28 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logaDNm),
  modz29 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logaDNm) + NetFree,
  modz30 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logaDNm, by = NetFree) + NetFree,
# modz31 = logTL2 ~ s(Cap_Year) + Gear + te(Cap_Mo, transout, bs = c("cc", "tp"), by = NetFree) + NetFree + s(logpop),
  modz32 = logTL2 ~ s(Cap_Year) + Gear + te(logDB, transout, by = NetFree) + NetFree + s(Cap_Mo, bs = "cc")
)

zij_size <- fit_gam(zsize, pz_mods)  

# compare----
zij_size$comparison %>% arrange(desc(dev_expl)) 
zij_size$comparison %>% arrange(BIC) 

# Test nested comparisons
compare_nested(zij_size$models, list(
  c("modz25", "modz7"), # check interactions
  c("modz25", "modz10"), # depends on k (not when restricted)
  c("modz8", "modz9"), 
  c("modz5", "modz6"), 
  c("modz6", "modz15"), # no month models
  c("modz15", "modz16"), # not sig
  c("modz9", "modz10"), # adding db - ah when restricted not so much
  c("modz9", "modz26"), # yes def net free, even more than db
  c("modz10", "modz19"), 
  c("modz26", "modz19"),
  c("modz19", "modz22"), 
  c("modz19", "modz21"), 
  c("modz19", "modz20"), # ya but (overfits)
  c("modz27", "modz22"), # for sure
  c("modz9", "modz23") 
  ))

## top model----
best_pz_name <- (zij_size$comparison %>% arrange(desc(dev_expl)))$model[4] 
best_pz_mod <- zij_size$models[[which(map_chr(zij_size$models, "name") == best_pz_name)]]$model

# going simpler without interaction with net free, seems to be overfitting 
check_model(best_pz_mod, "Best by Dev Expl") # or whatever
gam.check(best_pz_mod) # look at k etc.
draw(best_pz_mod) # pre-check

# plot----
appraise(best_pz_mod) & theme(plot.title = element_text(size = 14, family = "Optima",
                                                        margin = margin(t = 5, b = 10, unit = "pt")),
                              plot.subtitle = element_text(margin = margin(b = 5, unit = "pt")),
                              plot.margin = margin(l = 2, t = 0, b = 5, r = 10, unit = "pt"),
                              axis.title.y = element_text(margin = margin(r = 5, unit = "pt")))

## record mod----
saveRDS(best_pz_mod, 'data/rds/pzmod.RDS')

ggsave(
  "figs/supps/s8/s8_modzsiz.png",
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

