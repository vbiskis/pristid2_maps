# Details ----
#' 04_mngam_3b_run.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 31 Jan 2025
#' - separated into mult scripts for better efficiency 2025-12-23
#' Content: 
#' ++ part 1: manual model building (prev)
#' + part 2: comparison of models from glm / gam / w+w/o clavata
#' -----------

source('helpers/functions/src_mngam.R')
source('helpers/help_stats.R')

library(performance)
library(mclogit)

# go no clavata bc hes messing things up----

gammy <- readxl::read_xlsx("data/processed/gammy.xlsx") 
gammync <- gammy %>% 
  filter(Species_NB != "P. clavata") # for sep modeling
gammync$Species_NB <- as.numeric(factor(gammync$Species_NB)) - 1 #need it as 0 to 2

# lets begin!----
gam_mn_nc1 <- gam(list(Species_NB ~ s(transout, k = 5),
                       ~ s(transout, k = 5)),
                  family = multinom(K=2),
                  data = gammync)

summary(gam_mn_nc1) # ha cool - PP yes but PZ no
appraise(gam_mn_nc1)

gam_mn_nc2 <- gam(list(Species_NB ~ s(transout, k = 5) + s(logDB),
                       ~ s(transout, k = 5) + s(logDB)),
                  family = multinom(K=2),
                  data = gammync)

summary(gam_mn_nc2)
appraise(gam_mn_nc2) #and db def is! so cool

gam_mn_nc3 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logDB),
                       ~ s(transout, k = 5) + s(Tmax) + s(logDB)),
                  family = multinom(K=2),
                  data = gammync)

summary(gam_mn_nc3) #doesnt look as nice at all
appraise(gam_mn_nc3)

gam_mn_nc4 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(STRM_ORDER, k = 5) + s(logDB),
                       ~ s(transout, k = 5) + s(Tmax) + s(STRM_ORDER, k = 5) + s(logDB)),
                  family = multinom(K=2),
                  data = gammync)

summary(gam_mn_nc4)
appraise(gam_mn_nc4)
anova(gam_mn_nc3, gam_mn_nc4)
pchisq(19.067, df = 2, lower.tail = FALSE) 

gam_mn_nc5 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(logDB),
                       ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(logDB)),
                  family = multinom(K=2),
                  data = gammync)

summary(gam_mn_nc5) #stream order not significant
appraise(gam_mn_nc5) #ah scary!
anova(gam_mn_nc4, gam_mn_nc5)

gam_mn_nc6 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(logDB),
                       ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(logDB)),
                  family = multinom(K=2),
                  data = gammync)
appraise(gam_mn_nc6) #best!
summary(gam_mn_nc6) 
anova(gam_mn_nc4, gam_mn_nc6)
pchisq(6.1178, df = 2, lower.tail = FALSE) #mmmmm ya this one better

anova(gam_mn_nc6, gam_mn_nc5) #yes but fails other tests

#so the question is
plot(gammync$logMT, gammync$Species_NB) #pretty even... one option
plot(gammync$logRF, gammync$Species_NB) 

gam_mn_nc7 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(STRM_ORDER, k = 5) + s(logDB) + s(logMT),
                       ~ s(transout, k = 5) + s(Tmax) + s(STRM_ORDER, k = 5) + s(logDB) + s(logMT)),
                  family = multinom(K=2),
                  data = gammync)

summary(gam_mn_nc7) 
anova(gam_mn_nc6, gam_mn_nc7) # about the same thing, and cant go higher
anova(gam_mn_nc5, gam_mn_nc7) # nope worse
draw(gam_mn_nc7)