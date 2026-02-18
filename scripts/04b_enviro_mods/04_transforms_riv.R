# Details ----
#' 02_transforms_riv.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 10 Feb 2025
#' - modified 2025-12-22
#' Content: 
#' + few changes - this is just to test transformations
#' ++ will eventually want to automate these 3 scripts
#' +++ NOT the current priority Nikki don't get sucked in
#' -----------

pacman::p_load('dplyr', 'tidyverse', 'magrittr',
               'readxl', 'writexl')

sitsvars <- readxl::read_xlsx("data/processed/sitsvars.xlsx")

#add more----
#transforming some of our heavy hitters
ggqqplot(sitsvars$Precipitation)
sitsvars$logPrec <- log(sitsvars$Precipitation + 1) 
hist(sitsvars$logPrec, breaks = 30, main = "Dist")
check <- lm(logPrec ~ Species_NB, data = sitsvars)
check_model(check) 

ggqqplot(sitsvars$DB_area)
min(sitsvars$DB_area) #no need + 1
sitsvars$logDB <- log10(sitsvars$DB_area) 
hist(sitsvars$logDB, breaks = 30, main = "Dist")
check <- lm(logDB ~ Species_NB, data = sitsvars)
check_model(check) #ee better at least

ggqqplot(sitsvars$SB_area)
min(sitsvars$SB_area) #no need + 1
sitsvars$logSB <- log10(sitsvars$SB_area) 
hist(sitsvars$logSB, breaks = 30, main = "Dist")
check <- lm(logSB ~ Species_NB, data = sitsvars)
check_model(check) #ee better at least

ggqqplot(sitsvars$vol_yearavg)
min(!is.na(sitsvars$vol_yearavg))
sitsvars$logvol <- log(sitsvars$vol_yearavg + 1) 
hist(sitsvars$logvol, breaks = 30, main = "Dist")
check <- lm(logvol ~ Species_NB, data = sitsvars)
check_model(check) #hmm may be better

ggqqplot(sitsvars$wetdry) #all good
check <- lm(wetdry ~ Species_NB, data = sitsvars)
check_model(check) 

ggqqplot(sitsvars$DTO)
hist(sitsvars$DTO_IO, breaks = 30, main = "Dist")
sitsvars$logoutlet <- log(sitsvars$DTO)
sitsvars$sqrtout <- sqrt(sitsvars$DTO)
sitsvars <- sitsvars %>% 
  mutate(transout = case_when(Bord_In == 0 ~ sqrtout*-1,
                              .default = sqrtout))
hist(sitsvars$logoutlet, breaks = 30, main = "Dist") 
check <- lm(transout ~ Species_NB, data = sitsvars) 
check_model(check) #probs best

hist(sitsvars$DTB, breaks = 30, main = "Dist")
sitsvars$sqrtbord <- sqrt(sitsvars$DTB)
sitsvars <- sitsvars %>% 
  mutate(transbord = case_when(Bord_In == 0 ~ sqrtbord*-1,
                              .default = sqrtbord))
hist(sitsvars$transbord, breaks = 30, main = "Dist")
check <- lm(transbord ~ Species_NB, data = sitsvars)
check_model(check) #mmm def bimodal thans pristis

ggqqplot(sitsvars$DTM)
sitsvars$logmouth <- log(sitsvars$DTM)
hist(sitsvars$logmouth, breaks = 30, main = "Dist")
check <- lm(logmouth ~ Species_NB, data = sitsvars)
check_model(check) #ooh this is weird what 

ggqqplot(sitsvars$RivLg)
sitsvars$logriv <- log(sitsvars$RivLg)
hist(sitsvars$logriv, breaks = 30, main = "Dist")
check <- lm(logriv ~ Species_NB, data = sitsvars)
check_model(check) 

hist(sitsvars$MG_dist, breaks = 30, main = "Dist")
sitsvars$logmg <- log(sitsvars$MG_dist + 0.05) #accuracy on the MG map
hist(sitsvars$logmg, breaks = 30, main = "Dist")
check <- lm(logmg ~ Species_NB, data = sitsvars)
check_model(check) 

hist(sitsvars$Flat_dist, breaks = 30, main = "Dist")
sitsvars$logflat <- log(sitsvars$Flat_dist + 0.05)
hist(sitsvars$logflat, breaks = 30, main = "Dist")
check <- lm(logflat ~ Species_NB, data = sitsvars) #heeeelll ya
check_model(check) 

hist(sitsvars$Salt_dist, breaks = 30, main = "Dist")
sitsvars$logsalt <- log(sitsvars$Salt_dist + 0.05)
hist(sitsvars$logsalt, breaks = 30, main = "Dist")
check <- lm(logsalt ~ Species_NB, data = sitsvars) #thats fine
check_model(check) 
  
#bring in more data for rand effects!
sitsub <- readxl::read_xlsx("data/SubmDeets.xlsx")
sitsvars <- sitsvars %>% 
  left_join(sitsub %>% 
              dplyr::select(SUB, SubmType, Interactio, SubmID, MethodRec),
            by = "SUB") %>% 
  dplyr::select(-sqrtout, -sqrtbord)

#change levels----
fcts <- c("SUB", "Tide_State",
          "relflowy", "relflowm", 
          "Season", "climev", "ce_strth",
          "SubmType", "Interactio", "SubmID", "MethodRec", 
          "RivType", "FlatTYPE")

fctr <- which(colnames(sitsvars) %in% fcts)     
sitsvars[,fctr] <- lapply(sitsvars[,fctr], factor)

totest <- sitsvars %>% 
  mutate(Tide = as.numeric(Tide),
         TFH_min = as.numeric(TFH_min),
         TFL_min = as.numeric(TFL_min),
         Salt = as.logical(Salt),
         MG = as.logical(MG),
         Flat = as.logical(Flat),
         Bord_In = as.logical(Bord_In),
         TimeStamp = as.POSIXct(TimeStamp, format="%Y-%m-%d %H:%M:%S"),
         Method = fct_collapse(MethodRec,
                               "Social" = c("FB", "IG", "Youtube", "News"),
                               "PersComm" = c("Phone", "In Person")),
         SubmTC = fct_collapse(SubmType,
                               "Comm" = c("1", "2"), #commercial gear
                               "Charter" = c("3"), #charter
                               "Rec" = c("4", "5"), #and family
                               "On Site Gov" = c("6", "7"), #all gov sighting, live on site
                               "Found" = c("9", "10", "11"), #either sighting or rem saw
                               "Research" = c("12", "13", "14"))) #field data


