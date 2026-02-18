# Details ----
#' 02_spc_tests_2gear.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Dec 2025
#' Content: 
#' + quantifying the gear effect
#' -----------

source("helpers/help_stats.R")
sits_cln <- readxl::read_xlsx("data/processed/sits_cln.xlsx") 

# for new analyses
library(nnet)

sits_cln %>% 
  group_by(Region, Gear2, ColY2) %>% 
  summarise(
    n = n()
  ) %>% 
  pivot_wider(names_from = Gear2,
              values_from = n,
              values_fill = 0)

# chi----
chisq.test(table(sits_cln$Gear2, sits_cln$ColY2)) #yes - decline over time
chisq.test(table(sits_cln$Gear2, sits_cln$Region)) # and yes - more common West side
chisq.test(table(sits_cln$Gear2, sits_cln$Species_NB)) # absolutely - never PC

# multinom----
mnmod <- multinom(Species_NB ~ Region, data = sits_cln)
Anova(mnmod)

mnmod2 <- multinom(Species_NB ~ Gear2, data = sits_cln)
Anova(mnmod2)

mnmod3 <- multinom(Species_NB ~ Region + Gear2, data = sits_cln)
Anova(mnmod3)

mnmod4 <- multinom(Species_NB ~ Region*Gear2, data = sits_cln)
Anova(mnmod4) #ah! it is!

mnmod5 <- multinom(Species_NB ~ Region*Gear2 + Sub_Type, data = sits_cln)
anova(mnmod5, mnmod4, test = "Chisq")  #ah! okay

mnmod6 <- multinom(Species_NB ~ Region + Gear2 + Sub_Type, data = sits_cln)
anova(mnmod5, mnmod6, test = "Chisq") #ya we need that int

#lets do a test taking gear into account----
spectime_vgear <- mantelhaen.test(table(sits_cln$Sub_Type, 
                                        sits_cln$Species_NB, 
                                        sits_cln$Gear2))
print(spectime_vgear)

specreg_vgear <- mantelhaen.test(table(sits_cln$Region, 
                                       sits_cln$Species_NB, 
                                       sits_cln$Gear2))
print(specreg_vgear) 

# lets go by region then:
for(reg in c("CQ", "GoC", "NEQ", "WCY")) {
  cat(sprintf("\n%s:\n", reg))
  reg_data <- sits_cln %>% filter(Region == reg)
  
  # Check if enough data
  gear_time_counts <- table(reg_data$Sub_Type, reg_data$Gear2)
  print(gear_time_counts)
  
  # Only run CMH if all cells > 1
  if(min(gear_time_counts) > 1) {
    cmh_reg <- mantelhaen.test(table(reg_data$Sub_Type,
                                     reg_data$Species_NB,
                                     reg_data$Gear2))
    cat(sprintf("  p-value: %.3f %s\n", 
                cmh_reg$p.value, 
                ifelse(cmh_reg$p.value < 0.05, "***", "ns")))
  } else {
    cat("  Insufficient data for CMH test\n")
  }
}

