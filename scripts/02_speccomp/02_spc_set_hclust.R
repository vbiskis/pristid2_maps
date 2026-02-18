# Details ----
#' 02_spc_set_hclust.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 30 Mar 2024
#' Content: 
#' + initial look at some clustering
#' + generate filtered/collapsed factor variables for next stats tests
#' ++ so now I don't need to make this at the start of every script...
#' -> sits_cln
#' -----------

source("helpers/help_stats.R")
library(writexl)
sitsawks <- readxl::read_xlsx("data/processed/sitsawks.xlsx") 

sits_cln <- sitsawks %>%
  filter(Region != 'TS',
         Region != 'SEQ') %>% 
  mutate(Year_Bin = cut(Cap_Year, # even 6 yr intervals rel to prots
                        breaks = c(1983, 1990, 1997, 2004, 2011, 2018, 2024),
                        right = FALSE,
                        labels = c("1983-1989", "1990-1996", "1997-2003", 
                                   "2004-2010", "2011-2017", "2018-2023")),
         ColY_Bin = cut(Cap_Year, # collapsed for n within historic time frames
                          breaks = c(1983, 1997, 2011, 2018, 2024),
                          labels = c("1983-1996", "1997-2010", "2011-2017", "2018-2023"),
                          right = FALSE),
         Gear2 = fct_collapse(Gear, Net = c("Gillnet", "Prawn Net"), #collapsed for net or no
                              Line = c("Line", "Cast Net", "Unknown", "Sighting Only", "Other")
         )
  )

sits_cln <- sits_cln %>% 
  mutate(ColY2 = cut(Cap_Year, # so now we got 3!
                     breaks = c(1983, 2011, 2018, 2024),
                     labels = c("1983-2010", "2011-2017", "2018-2023"),
                     right = FALSE),
         Region = factor(Region, 
                         levels = c("WCY", "NEQ", "GoC", "CQ")))

write_xlsx(sits_cln, "data/processed/sits_cln.xlsx") # for basic time-spatial stats tests

# resemblance clustering----
# rows are first
# column = subtype
spec_cont_tbl1 <- table(sits_cln$Species_NB, sits_cln$Sub_Type)
spec_cont_tbl1 <- spec_cont_tbl1[rowSums(spec_cont_tbl1) > 0, ]
spec_cont_tbl2 <- table(sits_cln$Region, sits_cln$Sub_Type)
spec_cont_tbl2 <- spec_cont_tbl2[rowSums(spec_cont_tbl2) > 0, ]
spec_cont_tbl3 <- table(sits_cln$Region, sits_cln$Species_NB)
spec_cont_tbl3 <- spec_cont_tbl3[rowSums(spec_cont_tbl3) > 0, ]

# species time
stSim <- vegdist(spec_cont_tbl1, method = "bray")
stClust <- hclust(stSim, method = "average")
plot(stClust, cex = .5)

# time region
rtSim <- vegdist(spec_cont_tbl2, method = "bray")
rtClust <- hclust(rtSim, method = "average")
plot(rtClust, cex = .5)

# species region
rsSim <- vegdist(spec_cont_tbl3, method = "bray")
rsClust <- hclust(rsSim, method = "average")
plot(rsClust, cex = .5)

# cool but lets build on this----
spec_cont_tbl4 <- table(sits_cln$Species_NB, sits_cln$ColY_Bin)
spec_cont_tbl4 <- spec_cont_tbl4[rowSums(spec_cont_tbl4) > 0, ]

sySim <- vegdist(spec_cont_tbl4, method = "bray")
syClust <- hclust(sySim, method = "average")
plot(syClust, cex = .5) #ya makes sense no surprise trends here

# with region - does this change across bins?
spec_cont_tbl5 <- table(sits_cln$Region, sits_cln$ColY_Bin)
spec_cont_tbl5 <- spec_cont_tbl5[rowSums(spec_cont_tbl5) > 0, ]

rySim <- vegdist(spec_cont_tbl5, method = "bray")
ryClust <- hclust(rySim, method = "average")
plot(ryClust, cex = .5) # same same!



