# Details ----
#' 02_transforms_flow.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 26 Jan 2025
#' - modified 2025-12-22
#' Content: 
#' + few changes - this is just to test transformations
#' ++ will eventually want to automate these 3 scripts
#' +++ NOT the current priority Nikki don't get sucked in
#' -----------

source('helpers/help_stats.R')
sitsvars <- readxl::read_xlsx("data/processed/sitsvars.xlsx")

#Flow----
names(sitsflow)
sitsflowks <- sitsflow[!(sitsflow$Species_NB %in% c("Pristidae", 'Pristis sp.')),] 
#lets have a look at something:

env_flow <- sitsflowks %>%
  dplyr::select(where(is.numeric)) 
str(env_flow)
ggcorr(env_flow) #which of these do I even want???
#all water levels are the same
#discharge and volume are the same

library(skimr)
library(e1071)
library(MASS)
library(car)
library(performance)

#Power Test----
library(pwr)
pwr.anova.test(k = 4, f = 0.25, sig.level = 0.05, power = 0.80)
pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80, type = "two.sample") #total = 63 (~30 per group)

skim(sitsflow)
sitsflow %>% 
  group_by(Species_NB) %>% #look at size etc later
  summarise(
    count = n(),
    avg_annflow = mean(relannflow, na.rm = TRUE), #only pristidae above average flowr
    avg_prec = mean(Precipitation, na.rm = TRUE),
  ) #greens like it rainy!

#pretty much same number of nas for volume and discharge

#Normality----
##Ratio----
ggqqplot(sitsflow$relannflow)
hist(sitsflow$relannflow, breaks = 30, main = "Dist") 
shapiro.test(sitsflow$relannflow)

e1071::skewness(sitsflow$relannflow, na.rm = TRUE)
powerTransform(sitsflow$relannflow, family = "yjPower") #-0.5
boxcox(lm(relannflow ~ 1, data = sitsflow), lambda = seq(-2, 2, 0.1)) 
sitsflow$logyrvol <- log(sitsflow$relannflow) #nope
sitsflow$sqrrelyfl <- sqrt(sitsflow$relannflow)

e1071::skewness(sitsflow$sqrrelyfl, na.rm = TRUE) 
shapiro.test(sitsflow$sqrrelyfl) #heaps better!!!

check <- lm(sqrrelyfl ~ Species_NB, data = sitsflow) 
check_model(check) 
hist(sitsflow$sqrrelyfl, breaks = 30, main = "Dist") #I'll take it!
#okay so they can be sqrt to adjust value. 

##Discharge----
ggqqplot(sitsflow$disch_mo)
hist(sitsflow$disch_mo, breaks = 30, main = "Dist") #eee this makes no sense - throw outlier!
shapiro.test(sitsflow$disch_mo)
z_disch <- scale(sitsflow$disch_mo)

# Remove outliers with Z-scores > 3 or < -3
sitsflow_no <- sitsflow[abs(z_disch) <= 3, ]

e1071::skewness(sitsflow_no$disch_mo, na.rm = TRUE)
powerTransform(sitsflow_no$disch_mo, family = "yjPower") #close to -0.3 = inv sqrt? absolutely mental
min(sitsflow_no$disch_mo[sitsflow_no$disch_mo != 0], na.rm = TRUE)
sitsflow_no$logdism <- log(sitsflow_no$disch_mo + 0.0004)
sitsflow_no$sqdism <- (sitsflow_no$disch_mo)^(0.5)

e1071::skewness(sitsflow_no$sqdism, na.rm = TRUE) 
shapiro.test(sitsflow_no$sqdism) #both GROSS

check <- lm(sqdism ~ Species_NB, data = sitsflow_no) 
check_model(check) 
hist(sitsflow_no$logdism, breaks = 30, main = "Dist") #looks a bit better but might wanna go with a diff flow metric
#no thanks

##Volume----
#preferred
ggqqplot(sitsflow$vol_yearavg)
hist(sitsflow$vol_yearavg, breaks = 30, main = "Dist") 
shapiro.test(sitsflow$vol_yearavg)

e1071::skewness(sitsflow$vol_yearavg, na.rm = TRUE)
powerTransform(sitsflow$vol_yearavg, family = "yjPower") #0.2
boxcox(lm(vol_yearavg ~ 1, data = sitsflow), lambda = seq(-2, 2, 0.1)) 
min(sitsflow$vol_yearavg[sitsflow$vol_yearavg != 0], na.rm = TRUE) #ah jesus
sitsflow$logvoly <- log(sitsflow$vol_yearavg, base = 10)

e1071::skewness(sitsflow$logvoly, na.rm = TRUE) 
shapiro.test(sitsflow$logvoly)

check <- lm(logvoly ~ Species_NB, data = sitsflow) 
check_model(check) 
hist(sitsflow$logvoly, breaks = 30, main = "Dist") 
#much better, but still trash
