# Details ----
#' 06_sizgam_plot_gps.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Mar 2025
#' - modified 2025-12-28 so it runs independently
#' Content: 
#' + make plot
#' Output:
#' + gps map for TL est
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/functions/src_gam_fxns.R')
theme_set(mytheme)

source('helpers/help_map.R')

sizgam <- readRDS('data/rds/sits_sizemod.RDS')
glimpse(sizgam)

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat), as.factor),
         across(c(TrawlFree, NetFree), as.logical),
         Gear = factor(Gear, levels = c('Cast Net', 'Line', 'Gillnet', 'Prawn Net', 
                                        'Sighting Only')))

modf10 <- readRDS('data/rds/modf10.RDS')

sawnyr <- smooth_estimates(modf10) %>% 
  filter(.smooth == "te(Cap_Year,logaDNm)") %>% # Getting just the tensor spline and nothing else
  dplyr::select(Cap_Year, logaDNm, .estimate) %>% # Get just the variables we want
  distinct() # Avoid duplicate data, if any exist
head(sawnyr)

#need some avgs
sawnyr$Gear2 <- "Line"
sawnyr$transout <- 1
sawnyr$Cap_Mo <- 8
sawnyr$logDB <- 3.7
sawnyr$logpop <- 2
sawnyr$Species_NB <- "P. zijsron"

fishpred <- predict(modf10, 
                     newdata = sawnyr, 
                     type = "link", 
                     se.fit = TRUE)

sawnyr$se <- fishpred$se.fit
sawnyr$predTL <- exp(fishpred$fit)
sawnyr$alpha <- scales::rescale(1/fishpred$se, to = c(0.2, 1))

ggplot() +
  geom_raster(data = sawnyr, 
              aes(x = Cap_Year, y = logaDNm, 
                  fill = predTL, 
                  alpha = alpha #to add transparency (get it) around missing data
              )) + 
  scale_fill_gradientn(colors = rev(met.brewer("Demuth"))) +
  scale_y_continuous(breaks = log10(c(50, 100, 200, 400, 800)),
                     labels = c("50", "100", "200", "400", "800")) +
  labs(fill = "Total Length (cm)", 
       x = "Year",
       y = "Mean Days Netted at Mouth",
       alpha = "1/Standard Error") +
  geom_contour(data = sawnyr, 
               aes(x = Cap_Year, y = logaDNm,
                   z = predTL),
               col = "grey50", linewidth = 0.7) + 
  scale_shape_manual(name = "Gillnet Free",
                     values = c("cross", "square")) +
  scale_color_manual(name = "Species",
                     values = spec_col_ks,
                     labels = c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")) +
  geom_point(data = sizgam, 
             aes(x = Cap_Year, y = logaDNm, 
                 #color = Species_NB, 
                 #size = Size_Final,
                 shape = NetFree),
             #color = "black", 
             alpha = 0.5) 

ggsave(
  "figs/fig9/fig9_nfint.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
