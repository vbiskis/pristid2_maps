# Details ----
#' 02_spc_tests.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Sep 2024
#' Content: 
#' + chi sq tests 
#' + shifts over time PZ and PP (seen in density already)
#' -----------

source("helpers/help_stats.R")
sits_cln <- readxl::read_xlsx("data/processed/sits_cln.xlsx") 

#2D chisq-----
##species-time----
spec_cont_tbl1 <- table(sits_cln$Species_NB, sits_cln$ColY2)
print(spec_cont_tbl1)

chi_sq_res1 <- chisq.test(spec_cont_tbl1)
print(chi_sq_res1)

fisher_res1 <- fisher.test(spec_cont_tbl1, simulate.p.value=TRUE)
print(fisher_res1)

##region-time----
spec_cont_tbl2 <- table(sits_cln$Region, sits_cln$ColY2)
print(spec_cont_tbl2)

chi_sq_res2 <- chisq.test(spec_cont_tbl2)
print(chi_sq_res2)

fisher_res2 <- fisher.test(spec_cont_tbl2, simulate.p.value=TRUE)
print(fisher_res2) #ahh suggestive of an uneven increase across distribution

##spec-region----
spec_cont_tbl3 <- table(sits_cln$Species_NB, sits_cln$Region)
print(spec_cont_tbl3) #the most important one!

chi_sq_res3 <- chisq.test(spec_cont_tbl3)
print(chi_sq_res3)

fisher_res3 <- fisher.test(spec_cont_tbl3, simulate.p.value=TRUE)
print(fisher_res3) #intxns sig diff across location / time
# well that was expected

# Get means/dist----
sitsawc$logtime <- log10(sitsawc$Ysince)
ggqqplot(sitsawc$logtime) #trash
# nonpara4me

# By Spec----
AC <- sits_cln %>% 
  filter(Species_NB == "A. cuspidata")

cont_tblAC <- table(AC$Region, AC$ColY_Bin)
print(cont_tblAC)
fishA <- fisher.test(cont_tblAC)
print(fishA)

PC <- sits_cln %>% 
  filter(Species_NB == "P. clavata")

cont_tblPC <- table(PC$Region, PC$ColY_Bin)
print(cont_tblPC) # well thats

PP <- sits_cln %>% 
  filter(Species_NB == "P. pristis")

cont_tblPP <- table(PP$Region, PP$ColY_Bin)
print(cont_tblPP)
fishP <- fisher.test(cont_tblPP)
print(fishP)
# still sig, look at one at a time, bc nonpara
scPPlt <- cor.test(PP$Cap_Year, PP$GPS_lat, method = "spearman", exact = FALSE)
print(scPPlt) #nah
scPPlg <- cor.test(PP$Cap_Year, PP$GPS_long, method = "spearman", exact = FALSE)
print(scPPlg) #Ah yes, as seen before bc field work

PZ <- sits_cln %>% 
  filter(Species_NB == "P. zijsron")

cont_tblPZ <- table(PZ$Region, PZ$ColY_Bin)
print(cont_tblPZ)
fishZ <- fisher.test(cont_tblPZ)
print(fishZ)

scPZlt <- cor.test(PZ$Cap_Year, PZ$GPS_lat, method = "spearman", exact = FALSE)
print(scPZlt) #nah
scPZlg <- cor.test(PZ$Cap_Year, PZ$GPS_long, method = "spearman", exact = FALSE)
print(scPZlg) #yes
# same we saw before

# By Region----
for(reg in c("CQ", "GoC", "NEQ", "WCY")) {
  cat(sprintf("\n%s:\n", reg))
  reg_data <- sits_cln %>% filter(Region == reg)
  
  fisher_reg <- fisher.test(table(reg_data$ColY2, reg_data$Species_NB),
                            simulate.p.value = TRUE)
  
  cat(sprintf("  p-value: %.3f %s\n", 
              fisher_reg$p.value,
              ifelse(fisher_reg$p.value < 0.05, "***", "ns")))
} #nope



