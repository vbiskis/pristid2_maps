# Details ----
#' 03_lhs_nfz.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 4 Jul 2025
#' - New new 2025-12-13
#' Content: 
#' + are there more of diff species/age classes in nfz?
#' -----------

source('helpers/help_plot.R')
source('helpers/help_stats.R')
theme_set(mytheme)
library(rcompanion)

# prep----
sitslhs <- readxl::read_xlsx("data/processed/sitslhs.xlsx") 

#sitslhs$NetFree <- as.factor(sitsmods$NetFree)
#sitslhs$Species_NB <- as.factor(sitsmods$Species_NB)

# quick vis----
ggplot(sitslhs) +
  geom_bar(aes(x = Sub_Type, fill = NetFree), position = "fill") +
  facet_grid(Juv ~ Species_NB) #looks pretty constant there

sitslhs %>% 
  filter(Sub_Type == "recent") %>% 
  group_by(Species_NB, NetFree) %>% 
  summarise(
    count = n(),
    n_yoy = sum(YOY == "YOY"),
    prop_yoy = mean(YOY == "YOY"),
    prop_juv = mean(LHS %in% c('YOY', 'juvenile')),
    prop_ad = mean(Maturity == "mature"),
    .groups = "drop"
  )

ggplot(sitslhs %>% filter(Sub_Type == "recent",
                            Gear2 != "Other"), 
       aes(x = Juv, fill = NetFree)) + #ah i see
  geom_bar(position = "fill") +
  facet_grid(Gear2 ~ Species_NB) +
  labs(y = "Proportion") #oh very interesting

sitslhs %>% 
  filter(Sub_Type == "recent") %>% 
  group_by(Species_NB, NetFree) %>% 
  summarise(
    n = n(),
    n_juv = sum(LHS %in% c('YOY', 'juvenile')),
    n_yoy = sum(YOY == 'YOY'),
    prop_ad = sum(Maturity == "mature")
    )

# and stats it----
sizrec <- sitslhs %>% 
  filter(Sub_Type == "recent",
         Species_NB != "P. clavata")

sizrec$Species_NB <- droplevels(sizrec$Species_NB)
table(sizrec$LHS, sizrec$NetFree, sizrec$Species_NB)

for(sp in levels(sizrec$Species_NB)) {
  data_subset <- sizrec[sizrec$Species_NB == sp, ]
  tab <- table(data_subset$NetFree, data_subset$Juv)
  test_result <- fisher.test(ftable(tab), simulate.p.value = TRUE)
  cat(sp, "p-value:", test_result$p.value, "\n")
} #ah! pz sig!

for(sp in c("P. zijsron")) {  
  sp_data <- sizrec %>% filter(Species_NB == sp)
  
  pairwise <- pairwiseNominalIndependence(
    table(sp_data$LHS, sp_data$NetFree),
    fisher = TRUE,
    method = "fdr"
  )
  
  print(sp)
  print(pairwise)
}

for(sp in levels(sizrec$Species_NB)) {
  cat(sprintf("\n%s:\n", sp))
  
  sp_data <- sizrec %>% filter(Species_NB == sp)
  
  for(gear in c("Line", "Net")) {
    gear_data <- sp_data %>% filter(Gear2 == gear)
    
    # Fisher's test
    tab <- table(gear_data$Juv, gear_data$NetFree)
    test <- fisher.test(tab, simulate.p.value = TRUE)
    
    cat(sprintf("  %s: p = %.3f\n", gear, test$p.value))
  }
} #gear effect not sig - just splits power
