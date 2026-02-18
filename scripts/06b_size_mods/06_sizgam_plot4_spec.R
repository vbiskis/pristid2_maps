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
#' + species partial plots
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
source('helpers/functions/src_gam_fxns.R')
theme_set(mytheme)

source('helpers/help_map.R')

sizgam <- readRDS('data/rds/sits_sizemod.RDS')

sizgam <- sizgam %>% 
  mutate(across(c(MG, Salt, Flat), as.factor),
         across(c(TrawlFree, NetFree), as.logical),
         Gear = factor(Gear, levels = c('Cast Net', 'Line', 'Gillnet', 'Prawn Net', 
                                        'Sighting Only'))) # relevel just in case

moda <- readRDS('data/rds/acmod.RDS')
modp <- readRDS('data/rds/ppmod.RDS')
modz <- readRDS('data/rds/pzmod.RDS')

#Anoxy----
p1 <- plot_predictions(moda, condition = c("Cap_Mo", "transout"))
p1 <- p1 +
  geom_rug(data = sizgam %>% filter(Species_NB == "A. cuspidata"), 
           aes(x = Cap_Mo), alpha = 0.5, sides = "b") +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  scale_x_continuous(breaks = c(1, 4, 7, 10),
                        labels = c("January", "April", "July", "October")) +
  scale_fill_manual(
    values = met.brewer("Degas", n = 5),  
    labels = c("-50", "-10", "1", "10", "25"),  
    name = "Distance from\nOutlet (km)"
  ) +
  scale_colour_manual(
    values = met.brewer("Degas", n = 5),
    labels = c("-50", "-10", "1", "10", "25"),
    name = "Distance from\nOutlet (km)"
  ) +
  labs(x = "Month",
       y = "Total Length (cm)") +
  theme(legend.position = "bottom",
        legend.background = element_blank())

p2 <- plot_predictions(moda, by = c("Gear"))
p2 <- p2 +
  scale_y_continuous(limits = c(log(50), log(300)),
                     breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  scale_x_discrete(labels = c('Cast Net', 'Line', 'Gillnet', 'Trawl', 'Sighted')) +
  labs(x = "Gear",
       y = "Total Length (cm)") +
  theme(legend.position = "bottom") 

p3 <- plot_predictions(moda, condition = "logDB")
p3 <- p3 +
  geom_rug(data = sizgam%>% filter(Species_NB == "A. cuspidata"), 
           aes(x = logDB), alpha = 0.5, sides = "b") +
  scale_x_continuous(breaks = log10(c(500, 2000, 8000, 32000)),
                     labels = c("500", "2000", "8000", "32000")) +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = expression("Drainage Basin Area" ~ (km^2)),
       y = "") 

p4 <- plot_predictions(moda, condition = c("Cap_Year"))
p4 <- p4 +
  geom_rug(data = sizgam%>% filter(Species_NB == "A. cuspidata"), 
           aes(x = Cap_Year), alpha = 0.5, sides = "b") +
  scale_x_continuous(limits = c(2002, 2022)) +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = "Year",
       y = "") +
  theme(legend.position = "bottom")

(p1 + p3) / (p2 + p4) + plot_annotation(tag_levels = list(c('a)', 'b)', 'c)', 'd)'))) &  
  theme(plot.tag = element_text(size = 13, family = "Optima"),
        axis.title.x = element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
        plot.margin = margin(t = 15, r = 0, b = 10, l = 0, unit = "pt"),
        plot.tag.position = c(0.05, 1.05)) 

ggsave(
  "figs/supps/s9/s9_ppac.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#Pristis----

p1 <- plot_predictions(modp, condition = "Cap_Mo")
p1 <- p1 +
  geom_rug(data = sizgam %>% filter(Species_NB == "P. pristis"), 
           aes(x = Cap_Mo), alpha = 0.5, sides = "b") +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10),
                     labels = c("February", "April", "June", "August", "October")) +  
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = "Month",
       y = "Total Length (cm)") 

p2 <- plot_predictions(modp, condition = c("Cap_Year"))
p2 <- p2 +
  geom_rug(data = sizgam%>% filter(Species_NB == "P. pristis"), 
           aes(x = Cap_Year), alpha = 0.5, sides = "b") +
  scale_x_continuous(limits = c(2002, 2022)) +
  scale_y_continuous(breaks = log(c(75, 100, 150, 250, 400, 650)),
                     labels = c("75", "100", "150", "250", "400", "650")) +
  labs(x = "Year",
       y = "") 

p3 <- plot_predictions(modp, condition = "logDB")
p3 <- p3 +
  geom_rug(data = sizgam%>% filter(Species_NB == "P. pristis"), 
           aes(x = logDB), alpha = 0.5, sides = "b") +
  scale_x_continuous(breaks = log10(c(500, 2000, 8000, 32000)),
                     labels = c("500", "2000", "8000", "32000")) +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = expression("Drainage Basin Area" ~ (km^2)),
       y = "")

p4 <- plot_predictions(modp, condition = "transout")
p4 <- p4 +
  scale_x_continuous(breaks = sqrt(c(0, 30, 60, 120, 240)),
                     labels = c("0", "30", "60", "120", "240")) +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = "Distance from Outlet (km)",
       y = "Total Length (cm)") 

(p1 + p3) / (p4 + p2) + plot_annotation(tag_levels = list(c('a)', 'b)', 'c)', 'd)'))) &  
  theme(plot.tag = element_text(size = 13, family = "Optima"),
        axis.title.x = element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
        plot.margin = margin(t = 15, r = 0, b = 10, l = 0, unit = "pt"),
        plot.tag.position = c(0.05, 1.05)) 

ggsave(
  "figs/supps/s11/s11_pppp.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#Greenie----

p1 <- plot_predictions(modz, condition = c("Cap_Mo", "transout"))
p1 <- p1 +
  geom_rug(data = sizgam%>% filter(Species_NB == "P. zijsron"), 
           aes(x = Cap_Mo), alpha = 0.5, sides = "b") +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  scale_x_continuous(breaks = c(1, 4, 7, 10),
                     labels = c("January", "April", "July", "October")) +
  scale_fill_manual(
    values = met.brewer("Degas", n = 5),  
    labels = c("-10", "-2", "0", "2", "10"),
    name = "Distance from\nOutlet (km)"
  ) +
  scale_colour_manual(
    values = met.brewer("Degas", n = 5),
    labels = c("-10", "-2", "0", "2", "10"),
    name = "Distance from\nOutlet (km)"
  ) +
  labs(x = "Month",
       y = "Total Length (cm)") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0, b = 10, unit = "pt"))

p2 <- plot_predictions(modz, condition = c("Cap_Year"))
p2 <- p2 +
  geom_rug(data = sizgam%>% filter(Species_NB == "P. zijsron"), 
           aes(x = Cap_Year), alpha = 0.5, sides = "b") +
  scale_y_continuous(limits = c(log(100), log(600)),
                     breaks = log(c(100, 150, 250, 400, 650)),
                     labels = c("100", "150", "250", "400", "650")) +
  labs(x = "Year",
       y = "Total Length (cm)")

p3 <- plot_predictions(modz, condition = c("logDB"))
p3 <- p3 +
  geom_rug(data = sizgam %>% 
             filter(Species_NB == "P. zijsron"), 
           aes(x = logDB), alpha = 0.5, sides = "b") +
  scale_x_continuous(breaks = log10(c(500, 2000, 8000, 32000)),
                     labels = c("500", "2000", "8000", "32000")) +
  scale_y_continuous(breaks = log(c(50, 100, 150, 250, 400, 650)),
                     labels = c("50", "100", "150", "250", "400", "650")) +
  labs(x = expression("Drainage Basin Area" ~ (km^2)),
       y = "")

p4 <- plot_predictions(modz, by = "Gear")
p4 <- p4 +
  scale_y_continuous(limits = c(log(100), log(600)),
                     breaks = log(c(100, 150, 250, 400, 650)),
                     labels = c("100", "150", "250", "400", "650")) +
  scale_x_discrete(labels = c('Cast Net', 'Line', 'Gillnet', 'Sighted')) +
  labs(x = "Gear",
       y = "") 

(p1 + p3) / (p4 + p2) + plot_annotation(tag_levels = list(c('a)', 'b)', 'c)', 'd)'))) &  
  theme(plot.tag = element_text(size = 13, family = "Optima"),
        axis.title.x = element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
        plot.margin = margin(t = 15, r = 0, b = 10, l = 0, unit = "pt"),
        plot.tag.position = c(0.05, 1.08)) 

ggsave(
  "figs/supps/s10/s10_pppz.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
