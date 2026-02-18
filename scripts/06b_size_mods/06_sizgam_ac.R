# Details ----
#' 06_sizgam_ac.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-27
#' Content: 
#' + applying automated script to built multiple gams simultaneously
#' Output:
#' final model for A. cuspidata TL 
#' diagnostic plots (S10)
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/functions/src_gam_fxns.R')
theme_set(mytheme)

sizgam <- readRDS('data/rds/sits_sizemod.RDS')
glimpse(sizgam)

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat, NetFree), as.factor))

# stats----

asize <- sizgam %>% 
  filter(Species_NB == "A. cuspidata") 
glimpse(asize)

asize %>% 
  group_by(Gear) %>% 
  dplyr::summarise(n = n(),
                   meanTL = mean(Size_Final, na.rm = TRUE),
                   minYr = min(Cap_Year),
                   maxYr = max(Cap_Year),
                   DBsiz = mean(logDB),
                   pop = mean(popadj),
                   DN = mean(logaDNm),
                   proxRF = mean(dist_Cl_RF),
                   distmouth = mean(transout))

summary(aov(logTL2 ~ transout*STRM_ORDER, data = asize))

# mods----

ac_mods <- list(
  modab = logTL2 ~ Gear,
  moda0 = logTL2 ~ Gear + s(Cap_Year),
  moda1 = logTL2 ~ Gear + s(Cap_Mo, bs = "cc"),
  moda2 = logTL2 ~ Gear + s(Cap_Year) + s(Cap_Mo, bs = "cc"),
  moda3 = logTL2 ~ Gear + s(Cap_Mo, bs = "cc") + s(logDB, k = 5),
  moda4 = logTL2 ~ Gear + s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(logDB, k=5),
  moda5 = logTL2 ~ Gear + s(Cap_Year) + te(Cap_Mo, logDB, bs = c("cc", "tp")),
  moda6 = logTL2 ~ Gear + s(logDB, k=5) + s(Cap_Mo, bs = "cc") + s(transout),
  moda7 = logTL2 ~ Gear + s(Cap_Mo, bs = "cc") + te(logDB, transout),
  moda8 = logTL2 ~ Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5),
  moda9 = logTL2 ~ Gear + s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5),
  moda10 = logTL2 ~ Gear + s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5) + s(logaDNm),
  moda11 = logTL2 ~ Gear + s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5) + NetFree,
  moda12 = logTL2 ~ Gear + s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5) + s(logaDNm) + NetFree,
  moda13 = logTL2 ~ Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5) + s(Cap_Year, by = NetFree) + NetFree,
  moda14 = logTL2 ~ Gear + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5) + s(Cap_Year, by = NetFree) + NetFree + s(logaDNm),
  moda15 = logTL2 ~ Gear + s(Cap_Year) + te(Cap_Mo, transout, bs = c("cc", "tp")) + s(logDB, k=5) + NetFree + s(logpop),
  moda16 = logTL2 ~ Gear + s(Cap_Year) + s(Cap_Mo, bs = "cc") + s(transout) + s(logDB, k=5)
)

anoxy_size <- fit_gam(asize, ac_mods, select = FALSE)  

# ah 9 is actually best
# 13 sees some improv. but is it sig?

anoxy_size$comparison %>% arrange(desc(dev_expl)) 
anoxy_size$comparison %>% arrange(AIC) 
anoxy_size$comparison %>% arrange(BIC) 

## top model----
best_ac_name <- (anoxy_size$comparison %>% arrange(desc(dev_expl)))$model[3]
best_ac_mod <- anoxy_size$models[[which(map_chr(anoxy_size$models, "name") == best_ac_name)]]$model

check_model(best_ac_mod, "Best by Dev Expl")

# Test nested comparisons
compare_nested(anoxy_size$models, list(
  c("moda3", "moda4"), # mm doesnt actually mean anything with year?
  c("moda4", "moda16"), 
  c("moda8", "moda16"), 
  c("moda16", "moda9"), # absolutely intxn
  c("moda9", "moda10"), # not sig
  c("moda9", "moda11"), # net free is worse
  c("moda11", "moda13") # unless it's got the 
))

draw(best_ac_mod)

#ooh that's a clean appraise plot theme!
appraise(best_ac_mod) & theme(plot.title = element_text(size = 14, family = "Optima",
                                                        margin = margin(t = 5, b = 10, unit = "pt")),
                              plot.subtitle = element_text(margin = margin(b = 5, unit = "pt")),
                              plot.margin = margin(l = 2, t = 0, b = 5, r = 10, unit = "pt"),
                              axis.title.y = element_text(margin = margin(r = 5, unit = "pt")))

# record mod----
saveRDS(best_ac_mod, 'data/rds/acmod.RDS')

ggsave(
  "figs/supps/s6/s6_modasiz.png",
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
