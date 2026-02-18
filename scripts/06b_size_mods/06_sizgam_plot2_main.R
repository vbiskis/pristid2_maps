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

sizgam <- readRDS('data/rds/sits_sizemod.RDS') # the df
glimpse(sizgam)

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat, NetFree), as.factor),
         Gear = factor(Gear, levels = c('Cast Net', 'Line', 'Gillnet', 'Prawn Net', 
                                        'Sighting Only')))

sizmod <- readRDS('data/rds/sizmod.RDS') # and the model

#winning mod overall----

p1 <- plot_predictions(sizmod, condition = "transout")
p1 <- p1 +
  geom_rug(data = sizgam, aes(x = transout), alpha = 0.5, sides = "b") +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  scale_x_continuous(breaks = c(-10, -5, 0, 5, 10, 15),
                     labels = c("-100", "-25", "0", "25", "100", "225")) +
  labs(x = "Distance from Outlet",
       y = "") 

p2 <- plot_predictions(sizmod, condition = c("logaDNm"))
p2 <- p2 +
  geom_rug(data = sizgam, aes(x = logaDNm), alpha = 0.5, sides = "b") +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  scale_x_continuous(breaks = log10(c(125, 250, 500, 1000)),
                     labels = c("125", "250", "500", "1000"),
                     limits = c(log10(125), log10(1200))) +
  labs(x = "Average Annual Days Netted at Basin Mouth",
       y = "Total Length")

p3 <- plot_predictions(sizmod, condition = c("Cap_Year"))
p3 <- p3 +
  geom_rug(data = sizgam, 
           aes(x = Cap_Year), alpha = 0.5, sides = "b") +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = "Year",
       y = "Total Length") 

p5 <- plot_predictions(sizmod, condition = "logDB")
p5 <- p5 +
  geom_rug(data = sizgam, aes(x = logDB), alpha = 0.5, sides = "b") +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650"),
                     limits = c(log(100), log(350))) +
  scale_x_continuous(breaks = log10(c(1000, 10000, 100000)),
                     labels = c("1000", "10000", "100000"),
                     limits = c(log10(1000), log10(100000))) +
  labs(x = "Drainage Basin Area",
       y = "Total Length") 

p6 <- plot_predictions(sizmod, newdata = sizgam, by = "Gear")
p6 <- p6 +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  scale_x_discrete(labels = c('Cast Net', 'Line', 'Gillnet', 'Trawl', 'Sighted')) +
  labs(x = "Gear Used",
       y = "") +
  theme(axis.text.x = element_text(angle = 0))

p7 <- plot_predictions(sizmod, condition = c("Cap_Mo", "Species_NB"))
p7 <- p7 +
  geom_rug(data = sizgam, aes(x = Cap_Mo), alpha = 0.5, sides = "b") +
  scale_color_manual(name = "Species",
                     values = spec_col_ks,
                     labels = c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")) +
  scale_fill_manual(name = "Species",
                     values = spec_col_ks,
                     labels = c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron")) +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = "Month",
       y = "",
       color = "Species") +
  theme(legend.text = element_text(face = "italic", 
                                   margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
        legend.position = "bottom") 

legend <- get_legend(p7)

(p3 + p1) / (p5 + p6) / (p2 + p7 + theme(legend.position = "none")) / legend +
  plot_layout(heights = c(1, 1, 1, 0.125)) + 
  plot_annotation(tag_levels = list(c('a)', 'b)', 'c)', 'd)', 'e)', 'f)', ''))) &  
  theme(plot.tag = element_text(size = 13, family = "Optima"),
        axis.title.x = element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
        plot.margin = margin(t = 15, r = 0, b = 10, l = 0, unit = "pt"),
        plot.tag.position = c(0.2, 1.1)) 

ggsave(
  "figs/fig8/fig8_sizegam.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
