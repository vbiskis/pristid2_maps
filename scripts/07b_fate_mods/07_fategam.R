# Details ----
#' 7_fate_gam_run.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Feb 2025
#' - modified 2025-12-24
#' Content: 
#' + build fate model
#' + spatial autocorrelation etc incl sep.
#' -----------

source('helpers/help_stats.R')
source('helpers/functions/src_gam_fxns.R')

fatedf <- readRDS('data/rds/sits_fatemod.RDS')

fategam <- fatedf %>% 
  mutate(Species_NB = if_else(Species_NB == 'Pristis sp.', 'Pristidae', Species_NB),
         Species = if_else(Species_NB == 'Pristidae', NA, Species_NB), # removing unk species
         Gear3 = case_when(Gear2 == 'Sighting Only' | is.na(Gear2) ~ NA, # only using animals that are caught
                           Gear2 %in% c('Cast Net', 'Line') ~ 'Rec Gear',
                           .default = 'Comm Gear')) # just looking at actual catches
         
fategam$Species <- as.factor(fategam$Species)
fategam$Gear3 <- as.factor(fategam$Gear3)

glimpse(fategam)

table(fategam$Region, fategam$Gear3, fategam$Sub_Type) #xpected
table(fategam$Region, fategam$Gear3, fategam$Species) 

# mod list----
fate_mods <- list(
  base = Fate ~ 1, 
  gear = Fate ~ Gear3, # (only caught!)
  year = Fate ~ s(Cap_Year),
  region = Fate ~ Region,
  spec = Fate ~ Species, # good he's absolute bottom
  
  # building now from prelim (no gear with TL)
  mod1 = Fate ~ s(logTL2) + Species, 
  mod2 = Fate ~ s(Cap_Year) + Species, # no way this one is already so legit
  mod3 = Fate ~ s(logTL2) + s(Cap_Year), 
  mod4 = Fate ~ s(Cap_Year) + s(logTL2) + Species, 
  mod5 = Fate ~ s(Cap_Year, by = Species) + s(logTL2) + Species, 
  mod6 = Fate ~ s(Cap_Year) + s(logTL2, by = Species) + Species, 
  mod7 = Fate ~ te(Cap_Year, logTL2), 
  mod8 = Fate ~ te(Cap_Year, logTL2) + Species, 

  # add anthro
  modg1 = Fate ~ s(Cap_Year) + Gear3, 
  modg2 = Fate ~ s(Cap_Year, by = Gear3) + Gear3, 
  modg3 = Fate ~ s(Cap_Year) + NetFree + Gear3, 
  modg4 = Fate ~ s(Cap_Year, by = NetFree) + NetFree + Gear3, 
  modg5 = Fate ~ s(Cap_Year) + NetFree + s(logpop) + Gear3,
  modg6 = Fate ~ s(Cap_Year, by = NetFree) + NetFree + s(logpop) + Gear3,
  modg7 = Fate ~ s(Cap_Year) + Gear3 + s(logaDNm),
  modg8 = Fate ~ s(Cap_Year) + Gear3 + s(logpop), 
  modg9 = Fate ~ s(Cap_Year) + Gear3 + s(logRF),
  modg10 = Fate ~ te(Cap_Year, logaDNm) + Gear3, 
  modg11 = Fate ~ te(Cap_Year, logpop) + Gear3,
  modg12 = Fate ~ te(Cap_Year, logRF) + Gear3,
  modg13 = Fate ~ s(Cap_Year) + s(logaDNm) + s(logRF) + Gear3, # lol RF nope
  modg14 = Fate ~ s(Cap_Year) + s(logaDNm) + s(logpop) + Gear3, 
  modg15 = Fate ~ s(Cap_Year) + te(logpop, logaDNm) + Gear3, 
  modg16 = Fate ~ s(Cap_Year, by = NetFree) + Gear3 + te(logpop, logaDNm), # best dev w/o region
  modg17 = Fate ~ s(Cap_Year, by = NetFree) + Gear3 + te(logpop, logaDNm) + Species, 
  
  # what about some of the size factors?
  mods1 = Fate ~ s(Cap_Year) + s(transout), 
  mods2 = Fate ~ s(Cap_Year, by = NetFree) + s(transout) + NetFree, 
  mods3 = Fate ~ s(Cap_Year) + s(transout) + s(logaDNm), 
  mods4 = Fate ~ s(Cap_Year, by = NetFree) + s(transout) + s(logaDNm) + NetFree, 
  mods5 = Fate ~ s(Cap_Year) + s(transout) + s(Cap_Mo, bs = "cc"), 
  mods6 = Fate ~ s(Cap_Year) + s(transout) + s(Cap_Mo, bs = "cc") + Region, 
  mods7 = Fate ~ s(Cap_Year) + s(Cap_Mo, bs = "cc", by = Species) + Region + Species, 
  mods8 = Fate ~ s(Cap_Year) + s(Cap_Mo, bs = "cc", by = Species) + Region + s(logDB),
  
  # and region
  modr1 = Fate ~ s(Cap_Year) + Region,
  modr2 = Fate ~ s(Cap_Year) + Region + Species,
  modr3 = Fate ~ s(Cap_Year) + Region + NetFree,
  modr4 = Fate ~ s(Cap_Year) + Region + Gear3,
  modr5 = Fate ~ s(Cap_Year) + Region + Gear3 + Species,
  modr6 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + NetFree,
  modr7 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + NetFree + Species,
  modr8 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logaDNm) + NetFree,
  modr9 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logpop) + NetFree,
  # modr10 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logpop) + s(logaDNm) + NetFree,
  modr11 = Fate ~ s(Cap_Year) + Region + Gear3 + te(logpop, logaDNm) + NetFree,
  modr12 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logpop) + Species + NetFree,
  modr13 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logaDNm) + Species + NetFree,
  modr14 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logaDNm) + s(logpop),
  # modr15 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logaDNm) + s(logpop) + Species,
  modr16 = Fate ~ s(Cap_Year) + Region + Gear3 + te(logpop, logaDNm),
  # modr17 = Fate ~ s(Cap_Year) + Region + Gear3 + te(logpop, logaDNm) + Species,
  # modr18 = Fate ~ s(Cap_Year) + Region + Gear3 + te(logpop, logaDNm) + Species + NetFree,
  # modr19 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logaDNm) + s(logpop) + Species + NetFree,
  modr20 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + s(logaDNm) + s(logpop) + Species,
  modr21 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + s(logaDNm) + Species,
  modr22 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + s(logpop) + Species,
  modr23 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + s(logpop),
  modr24 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + s(logpop) + s(logaDNm),
  modr25 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + te(logpop, logaDNm),
  modr26 = Fate ~ s(Cap_Year, by = NetFree) + Region + Gear3 + te(logpop, logaDNm) + Species,
  modr27 = Fate ~ s(Cap_Year) + Region + Gear3 + s(logpop) + Species
)

# run mods----
allfate <- fit_gam(fategam, fate_mods, family = binomial, select = FALSE) 

# compare----
allfate$comparison %>% arrange(desc(dev_expl)) 
allfate$comparison %>% arrange(desc(dev_expl)) %>% print(n = Inf)

compare_nested(allfate$models, list(
  c("modr22", "modr20"), # 20 better! (1 > 2 > 3)
  c("modr12", "modr22"), # 22 > 12 (2 > 4)
  c("modr27", "modr12"), # 12 = 27 (4 = 5)
  c("modr24", "modr25"), # 25 > 24 (6 > 7), best for non-spec mods
  c("modr11", "modr25"), # 25 > 11 (6 > 7 = 8)
  c("modr14", "modr24"), # 24 = 14 (8 = 9 > 10)
  c("modg16", "modr25"), # yes
  c("modr14", "modr25"), 
  c("modg15", "modg16"), # yes yes
  c("modg14", "modg15") # yesss
)) # final answer nfz intxn > region included > and population

# best mods to check -> r20 (1) > r22 (2) > r25 (6) > g16 (13)
modnam <- (allfate$comparison %>% arrange(desc(dev_expl)))$model[1] # defo! first with all sig
fatmod <- allfate$models[[which(map_chr(allfate$models, "name") == modnam)]]$model

check_model(fatmod) # ooh she all sig
gam.check(fatmod)
anova.gam(fatmod, test = "Chisq")
draw(fatmod)

saveRDS(fatmod, 'data/rds/fatmod.RDS')

appraise(fatmod) & theme(plot.title = element_text(size = 14, family = "Optima",
                                                   margin = margin(t = 5, b = 10, unit = "pt")),
                         plot.subtitle = element_text(margin = margin(b = 5, unit = "pt")),
                         plot.margin = margin(l = 2, t = 0, b = 5, r = 10, unit = "pt"),
                         axis.title.y = element_text(margin = margin(r = 5, unit = "pt")))

# save resid plot----

ggsave(
  "figs/supps/s12/s12_modfate.png",
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

##
plot_predictions(fatmod, by = "Species")
