# Details ----
#' 04_mngam_1_vars.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 30 Jan 2025
#' - duplicated/modified from glm version 2025-12-23
#' - she was pretty already, just all over the place (mult scripts ugh)
#' Content: 
#' + scaling of df/vars check prior to auto run
#' + check how n changes with vars chosen
#' -----------

pacman::p_load('readxl', 'writexl', 'GGally', 'dplyr')
maxenv <- readxl::read_xlsx("data/processed/sits_envmod.xlsx") # mn factors
sitsmods <- readxl::read_xlsx("data/processed/sitsmods.xlsx")

mostacc <- maxenv %>% 
  left_join(sitsmods %>% 
              dplyr::select(SUB, dist_Cl_RF, logRF)) %>% 
  mutate(logdRF = log(dist_Cl_RF + 0.05))

#subset----
str(mostacc)
mostacc$Species_NB <- droplevels(as.factor(mostacc$Species_NB)) # as a precaution

#check vars----
library(Boruta)
#varible selection
# set.seed(20002)
# boruta <- Boruta(Species_NB ~., data = mostacc)
# borstat <- attStats(boruta)
# want_vars <- rownames(borstat)[borstat$decision %in% 
#                                       c("Confirmed", "Tentative")]
# 
# gammy<- runit[, c("Species_NB", want_vars)]
# n_rows <- nrow(gammy)
# na_proportions <- sapply(gammy, function(x) sum(is.na(x))/n_rows)
# best_vars <- names(na_proportions[na_proportions < 0.05])
# print(na_proportions[na_proportions < 0.05])
# ggcorr(gammy, label = TRUE)

#pick vars----
these_vars <- c("Region", # all from PCA
                "wetdry", "Cap_Mo", # mult values for time (cyclic)
                "Tmax", "oni", "logPrec", # more season metrics
                "logDB", "STRM_ORDER",  #and river 
                "Salt", "Flat", "MG", #then some structure,
                "logsalt", "logflat", "logmg",
                "logdRF", "logRF",  # to test bias! 
                "transout", "logoutlet") 

# all locations 276 | 246 no PC
# most acc locations; end up with 217 | 198 no PC
gammy <- mostacc[, c("Species_NB", these_vars)] 
gammy <- na.omit(gammy) # 205 now
names(gammy)
summary(gammy[sapply(gammy, is.numeric)])

#scale for dist----
gammy$logPrec <- scale(gammy$logPrec)
gammy$Salt <- is.logical(gammy$Salt)
gammy$Flat <- is.logical(gammy$Flat)
gammy$MG <- is.logical(gammy$MG)
gammy$logsalt <- scale(gammy$logsalt)
gammy$logflat <- scale(gammy$logflat)
gammy$logmg <- scale(gammy$logmg)
gammy$STRM_ORDER <- scale(gammy$STRM_ORDER)
gammy$logDB <- scale(gammy$logDB)
gammy$Tmax <- scale(gammy$Tmax)
gammy$logoutlet <- scale(gammy$logoutlet)
gammy$logdRF <- scale(gammy$logdRF) # better bias indicator
gammy$logRF <- scale(gammy$logRF) # worse, throw out
gammy$transout <- scale(gammy$transout)

ggcorr(gammy, label = TRUE)

str(gammy)

write_xlsx(gammy, 'data/processed/gammy.xlsx')

#look for random effect----
# gammy %>%
#   dplyr::group_by(Species_NB, SubmTC) %>%
#   dplyr::summarise(Frequency = n()) %>%
#   tidyr::spread(SubmTC, Frequency)
# 
# ggplot(runit, aes(x = Species_NB, fill = Interactio)) +
#   geom_bar(position = "stack") + 
#   facet_wrap(~ Method, nrow = 3) +
#   theme_bw() +
#   labs(x = "Species", y = "Frequency") 
# 
# baseenv <- mblogit(formula = Species_NB ~ 1,
#                    data = gammy)
# 
# rand1 <- mblogit(formula = Species_NB ~ 1, 
#         random = ~ 1 | Method, #nothing is converging!
#         data = gammy)
# 
# aic.base <- AIC(logLik(baseenv))
# aic.rand1 <- AIC(logLik(rand1))
# aic.base; aic.rand1
# anova(baseenv, rand1)
