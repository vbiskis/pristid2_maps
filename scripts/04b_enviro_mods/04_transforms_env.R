# Details ----
#' 02_transforms_env.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 23 May 2023
#' - modified 2025-12-22
#' Content: 
#' + few changes - this is just to test transformations
#' ++ will eventually want to automate these 3 scripts
#' +++ NOT the current priority Nikki don't get sucked in
#' -----------

source('helpers/help_stats.R')
sitsvars <- readxl::read_xlsx("data/processed/sitsvars.xlsx")

#this is called as a prep in PCA/Modeling
#source should be loaded there as not to override.

#First have a look at normality
names(sitsenvj)
library(e1071)
library(MASS)
library(car)
library(performance)

##Prec----
#log
ggqqplot(sitsenvj$Precipitation)
hist(sitsenvj$Precipitation, breaks = 30, main = "Dist")

powerTransform(sitsenvj$Precipitation, family = "yjPower") #close to 0 = log
boxcox(lm((Precipitation + 1) ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

sitsenvj$logPrec <- log(sitsenvj$Precipitation + 1) 
shapiro.test(sitsenvj$logPrec)
hist(sitsenvj$logPrec, breaks = 30, main = "Dist")
e1071::skewness(sitsenvj$logPrec, na.rm = TRUE)
check <- lm(logPrec ~ Species_NB, data = sitsenvj)
check_model(check) #improved, may need zero-inflated

##Tmax----
#power = 2
ggqqplot(sitsenvj$Tmax)
hist(sitsenvj$Tmax, breaks = 30, main = "Dist")
shapiro.test(sitsenvj$Tmax)

e1071::skewness(sitsenvj$Tmax, na.rm = TRUE)
powerTransform(sitsenvj$Tmax, family = "yjPower") #close to 2 = power
boxcox(lm(Tmax ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

sitsenvj$Tmaxsq <- (sitsenvj$Tmax)^2
e1071::skewness(sitsenvj$Tmaxsq, na.rm = TRUE) #slightly better
shapiro.test(sitsenvj$Tmaxsq) #improvedish

check <- lm(Tmax ~ Species_NB, data = sitsenvj) 
check_model(check) #kinda the same but w/e

##Temp_water----
#power = 2
ggqqplot(sitsenvj$Temp_wat)
hist(sitsenvj$Temp_wat, breaks = 30, main = "Dist")
shapiro.test(sitsenvj$Temp_wat) #already good

e1071::skewness(sitsenvj$Temp_wat, na.rm = TRUE)
powerTransform(sitsenvj$Temp_wat, family = "yjPower") #close to 2 = power
boxcox(lm(Temp_wat ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

sitsenvj$Temp_watsq <- (sitsenvj$Temp_wat)^2
e1071::skewness(sitsenvj$Temp_watsq, na.rm = TRUE) #slightly better
shapiro.test(sitsenvj$Temp_watsq) #improved

check <- lm(Temp_watsq ~ Species_NB, data = sitsenvj) #worse!
check_model(check) #nah leaving temp alone

##Turb_marine----
#log
ggqqplot(sitsenvj$Turb_kd490)
hist(sitsenvj$Turb_kd490, breaks = 30, main = "Dist") #looking like needs a log
shapiro.test(sitsenvj$Turb_kd490) 

e1071::skewness(sitsenvj$Turb_kd490, na.rm = TRUE)
boxcox(lm(Turb_kd490 ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #right on 0

sitsenvj$logTurbkd <- log(sitsenvj$Turb_kd490)
e1071::skewness(sitsenvj$logTurbkd, na.rm = TRUE)
shapiro.test(sitsenvj$logTurbkd) #no question

check <- lm(logTurbkd ~ Species_NB, data = sitsenvj) 
check_model(check) 
hist(sitsenvj$logTurbkd, breaks = 30, main = "Dist") #amazing!

##Turb_NTU----
#log
ggqqplot(sitsenvj$Turb_NTUmo)
hist(sitsenvj$Turb_NTUmo, breaks = 30, main = "Dist") #eeew

powerTransform(sitsenvj$Turb_NTUmo, family = "yjPower") #close to -0.5 = power?
boxcox(lm(Turb_NTUmo ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) 

sitsenvj$logTurbNTU <- log(sitsenvj$Turb_NTUmo)
sitsenvj$TurbNTUinsq <- (sitsenvj$Turb_NTUmo)^(-0.5)

e1071::skewness(sitsenvj$TurbNTUinsq, na.rm = TRUE) 
shapiro.test(sitsenvj$TurbNTUinsq) #a bit better but still left skewed 

check <- lm(logTurbNTU ~ Species_NB, data = sitsenvj) 
check_model(check) 
hist(sitsenvj$logTurbNTU, breaks = 30, main = "Dist") #bit better but still meh

##PSAL_mar----
#not ready
ggqqplot(sitsenvj$PSAL_mar)
hist(sitsenvj$PSAL_mar, breaks = 30, main = "Dist") #eeew
shapiro.test(sitsenvj$PSAL_mar) #eeew
#doesnt really matter - 312 vals dropped!

e1071::skewness(sitsenvj$PSAL_mar, na.rm = TRUE)
powerTransform(sitsenvj$PSAL_mar, family = "yjPower") #log again
#boxcox(lm(PSAL_mar ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

adjusted_logit <- function(p, eps = 0.0001) {
  p_adj <- ifelse(p == 0, eps, ifelse(p == 1, 1 - eps, p))
  return(log(p_adj / (1 - p_adj)))
}

sitsenvj$PSAL_logit <- adjusted_logit(sitsenvj$PSAL_mar)
e1071::skewness(sitsenvj$PSAL_logit, na.rm = TRUE) 
shapiro.test(sitsenvj$PSAL_logit) #best with logit trans

check <- lm(PSAL_logit ~ Species_NB, data = sitsenvj) #this isnt happening
check_model(check) #no this is terrible!
hist(sitsenvj$PSAL_logit, breaks = 30, main = "Dist") 

#O2----
#power 
ggqqplot(sitsenvj$O2_mgLmo)
hist(sitsenvj$O2_mgLmo, breaks = 30, main = "Dist") #looks like an easy fix
shapiro.test(sitsenvj$O2_mgLmo) #eeew

e1071::skewness(sitsenvj$O2_mgLmo, na.rm = TRUE)
powerTransform(sitsenvj$O2_mgLmo, family = "yjPower") #close to 3 = power
boxcox(lm(O2_mgLmo ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

sitsenvj$O2sq <- (sitsenvj$O2_mgLmo)^(2)
e1071::skewness(sitsenvj$O2sq, na.rm = TRUE) 
shapiro.test(sitsenvj$O2sq) #best with - power (even hist)

check <- lm(O2sq ~ Species_NB, data = sitsenvj) 
check_model(check) 
hist(sitsenvj$O2sq, breaks = 30, main = "Dist") #there

#Elev/Depth----
#both log
ggqqplot(sitsenvj$Elev)
hist(sitsenvj$Elev, breaks = 30, main = "Dist") #looks like a job for log again
shapiro.test(sitsenvj$Elev) #eeew

e1071::skewness(sitsenvj$Elev, na.rm = TRUE)
powerTransform(sitsenvj$Elev, family = "yjPower") #yup ~ 0
#boxcox(lm(Elev ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

min(sitsenvj$Elev[sitsenvj$Elev != 0], na.rm = TRUE)
sitsenvj$logElev <- log(sitsenvj$Elev+ 0.085)
e1071::skewness(sitsenvj$logElev, na.rm = TRUE) 
shapiro.test(sitsenvj$logElev) #best with - power (even hist)

check <- lm(logElev ~ Species_NB, data = sitsenvj) 
check_model(check) 
hist(sitsenvj$logElev, breaks = 30, main = "Dist") #still front heavy but

ggqqplot(sitsenvj$Depth)
hist(sitsenvj$Depth, breaks = 30, main = "Dist") #looks like a job for log again
shapiro.test(sitsenvj$Depth) #opposite - negative opp skew

e1071::skewness(sitsenvj$Depth, na.rm = TRUE)
powerTransform(sitsenvj$Depth, family = "yjPower") #power trans
#boxcox(lm(Depth ~ 1, data = sitsenvj), lambda = seq(-2, 2, 0.1)) #visualise

sitsenvj$logDepth <- log(-sitsenvj$Depth + 0.02) #actually great!

e1071::skewness(sitsenvj$logDepth, na.rm = TRUE) 
shapiro.test(sitsenvj$logDepth) #need to take abs and then log, so same as elev, no surprise

check <- lm(logDepth ~ Species_NB, data = sitsenvj) 
check_model(check) 
hist(sitsenvj$logDepth, breaks = 30, main = "Dist") 




