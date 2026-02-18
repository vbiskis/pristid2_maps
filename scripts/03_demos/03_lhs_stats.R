# Details ----
#' 03_lhs_stats.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - New new 2025-12-13
#' Content: 
#' + stats reporting for demography section
#' -----------

# changes through time (regional)----

sitslhs <- readxl::read_xlsx("data/processed/sitslhs.xlsx") 

lhs_time <- table(sitslhs$LHS, sitslhs$ColY2)
print(lhs_time)
fisher.test(lhs_time) #looks like a yes!

for(spec in c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")) {
  cat(sprintf("\n%s:\n", spec))
  spec_data <- sitslhs %>% filter(Species_NB == spec)
  
  fisher_reg <- fisher.test(table(spec_data$ColY2, spec_data$LHS),
                            simulate.p.value = TRUE)
  
  cat(sprintf("  p-value: %.3f %s\n", 
              fisher_reg$p.value,
              ifelse(fisher_reg$p.value < 0.05, "***", "ns")))
} 

# just for P. pristis

ggplot(data = sitslhs) +
  geom_bar(aes(x = ColY2, fill = LHS), position = 'fill') +
  facet_grid(~ Species_NB)
                                                                                                                                                 