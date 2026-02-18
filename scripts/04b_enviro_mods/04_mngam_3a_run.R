# Details ----
#' 04_mngam_3a_run.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 31 Jan 2025
#' - separated into mult scripts for better efficiency 2025-12-23
#' Content: 
#' + part 1: manual model building, all spec
#' ++ part 2 moved: manual model building w/o clavata
#' -----------

source('helpers/functions/src_mngam.R')
source('helpers/help_stats.R')

library(performance)
library(mclogit)

gammy <- readxl::read_xlsx("data/processed/gammy.xlsx") 

# ggcorr(gammy, label = TRUE)
gammy$Species_NB <- as.numeric(factor(gammy$Species_NB)) - 1 #need it as 0 to 3

#Best mod from GLM---- 

gam_glm <- gam(list(Species_NB ~ s(Tmax) + s(STRM_ORDER, k = 5) + s(logflat) + te(wetdry, transout),
                     ~ s(Tmax) + s(STRM_ORDER, k = 5) + s(logflat) + te(wetdry, transout),
                     ~ s(Tmax) + s(STRM_ORDER, k = 5) + s(logflat) + te(wetdry, transout)),
                family = multinom(K=3),
                data = gammy)

summary(gam_glm) 
AIC(gam_glm) # wow that is better!

gam_glm2 <- gam(list(Species_NB ~ s(Tmax) + s(STRM_ORDER, k = 5) + te(wetdry, transout),
                    ~ s(Tmax) + s(STRM_ORDER, k = 5) + te(wetdry, transout),
                    ~ s(Tmax) + s(STRM_ORDER, k = 5) + te(wetdry, transout)), #and without flats
               family = multinom(K=3),
               data = gammy)

summary(gam_glm2) 
anova(gam_glm, gam_glm2) # mmm not as good! 

#Build an mnGAM----

gam_mod1 <- gam(list(Species_NB ~ s(transout),
                     ~ s(transout),
                     ~ s(transout)),
                family = multinom(K=3),
                data = gammy)

gam_mod2 <- gam(list(Species_NB ~ s(transout) + s(Cap_Mo, bs = "cc"),
                     ~ s(transout) + s(Cap_Mo, bs = "cc"),
                     ~ s(transout) + s(Cap_Mo, bs = "cc")),
                family = multinom(K=3),
                data = gammy)

summary(gam_mod1)
summary(gam_mod2)
anova(gam_mod2, gam_mod1)

AIC(gam_mod2) #already better

gam_mod3 <- gam(list(Species_NB ~ s(transout) + s(Tmax),
                     ~ s(transout) + s(Tmax),
                     ~ s(transout) + s(Tmax)),
                family = multinom(K=3),
                data = gammy)

anova(gam_mod1, gam_mod3)

gam_mod4 <- gam(list(Species_NB ~ s(transout) + s(Tmax) + s(logPrec),
                     ~ s(transout) + s(Tmax) + s(logPrec),
                     ~ s(transout) + s(Tmax) + s(logPrec)),
                family = multinom(K=3),
                data = gammy)

summary(gam_mod4)
anova(gam_mod2, gam_mod4) # temp and prec are better indicators than month
anova(gam_mod3, gam_mod4) 

gam_mod5 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5),
                     ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5),
                     ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5)),
                family = multinom(K=3),
                data = gammy)

summary(gam_mod5)
anova(gam_mod4, gam_mod5) 
pchisq(24.692, df = 4.6033, lower.tail = FALSE) # absolutely 

gam_mod6 <- gam(list(Species_NB ~ s(transout) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(Cap_Mo, bs = "cc"),
                     ~ s(transout) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(Cap_Mo, bs = "cc"),
                     ~ s(transout) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(Cap_Mo, bs = "cc")),
                family = multinom(K=3),
                data = gammy)

summary(gam_mod6)
anova(gam_mod5, gam_mod6)  # oh what! (this is the only one I didnt scale tho)
pchisq(21.062, df = 6.834, lower.tail = FALSE) # yes as well...

gam.check(gam_mod4)
gam.check(gam_mod5) # oh looks good
gam.check(gam_mod6) # maybe better tho

gam_mod7 <- gam(list(Species_NB ~ s(Tmax) + s(STRM_ORDER, k = 5) + te(logPrec, transout),
                     ~ s(Tmax) + s(STRM_ORDER, k = 5) + te(logPrec, transout),
                     ~ s(Tmax) + s(STRM_ORDER, k = 5) + te(logPrec, transout)),
                family = multinom(K=3),
                data = gammy)

anova(gam_mod5, gam_mod7)  
pchisq(21.435, df = 10.158, lower.tail = FALSE) # oh wait
gam.check(gam_mod7) # hmm

BIC(gam_mod7) # oh she complex
BIC(gam_mod6)
BIC(gam_mod5) # goin with the less complicated
BIC(gam_mod4)
BIC(gam_mod3)
BIC(gam_mod2)
BIC(gam_glm)

gam_mod8 <- gam(list(Species_NB ~ s(transout) + s(Tmax) + s(logPrec) + s(logflat) + s(STRM_ORDER, k = 5),
                     ~ s(transout) + s(Tmax) + s(logPrec) + s(logflat) + s(STRM_ORDER, k = 5),
                     ~ s(transout) + s(Tmax) + s(logPrec) + s(logflat) + s(STRM_ORDER, k = 5)),
                family = multinom(K=3), # nah he says no thats too many!
                data = gammy)

# k so we got our base multinom ~
# see how fishing pressure comes in! 
## eventually split to own....
plot(gammy$logdRF, gammy$Species_NB) 

gam_modf <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(logdRF, k = 5),
                     ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(logdRF, k = 5),
                     ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(STRM_ORDER, k = 5) + s(logdRF, k = 5)),
                family = multinom(K=3),
                data = gammy)

summary(gam_modf) 
anova(gam_mod5, gam_modf) 
pchisq(29.393, df = 7.8779, lower.tail = FALSE) # just PP bc of distance

#just fp on its own----
gam_modfpc <- gam(list(Species_NB ~ s(logdRF, k = 5),
                       ~ s(logdRF, k = 5),
                     ~ s(logdRF, k = 5)),
                family = multinom(K=3),
                data = gammy)

#so will filter out PC and then go
summary(gam_modfpc)
draw(gam_modfpc)

newdata <- data.frame(logdRF = seq(min(gammy$logdRF), max(gammy$logdRF), length = 100))
preds <- predict(gam_modfpc, newdata = newdata, type = "response")
matplot(newdata$logdRF, preds, type = "l", lty = 1, lwd = 2,
        xlab = "Distance to Boating Facility (log)", ylab = "Probability of Encounter",
        col = c("darkblue", "gold", "red3", "darkgreen" ))  # thats kinda cool tho!

#run auto----
gam_mnmodels <- get_mngam(gammy, k3_vars, k5_vars, k10_vars, threshold = 0.4, min_pred = 1, max_pred = 6)

print_models <- function(results, n = 10) {
  for(i in 1:min(n, nrow(results))) {
    cat("\nModel Set", i, "\n")
    cat("Predictors:", results$predictors[i], "\n")
    cat("Base model AIC:", round(results$aic[i], 2), "\n")
    cat("Formulas:\n", results$formula[i], "\n")
    cat("\n-------------------\n")
  }
}

print_models(gam_mnmodels)

# when all of them ~ DB area + outlet