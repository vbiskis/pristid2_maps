# Details ----
#' 7_fategam_pplots.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Mar 2025
#' - modified 2025-12-31
#' Content: 
#' + final plotting output for fate model
#' Output:
#' + gps map
#' -----------

source('helpers/help_stats.R')
fatedf <- readRDS('data/rds/sits_fatemod.RDS')
modfate <- readRDS('data/rds/fatmod.RDS')
gratia::draw(modfate)

fategam <- fatedf %>% 
  mutate(Species_NB = if_else(Species_NB == 'Pristis sp.', 'Pristidae', Species_NB),
         Species = if_else(Species_NB == 'Pristidae', NA, Species_NB), # removing unk species
         Gear3 = case_when(Gear2 == 'Sighting Only' | is.na(Gear2) ~ NA, # only using animals that are caught
                           Gear2 %in% c('Cast Net', 'Line') ~ 'Rec Gear',
                           .default = 'Comm Gear')) # just looking at actual catches

glimpse(fategam)
fategam$Species <- as.factor(fategam$Species)
fategam$Gear3 <- as.factor(fategam$Gear3)

source('helpers/help_plot.R') #lets go!
source('helpers/help_map.R')
theme_set(mytheme)

# partial plots----
## ggpredict----

gr <- ggeffects::ggemmeans(modfate, terms = "Gear3") 
gr$x <- factor(gr$x, 
               levels = c("Comm Gear", "Rec Gear"),
               labels = c("Commercial", "Recreational"))

grplot <- ggplot() +
  geom_point(data = gr, 
             aes(x = x, y = predicted), 
             size = 3) +
  geom_errorbar(data = gr, 
                aes(x= x, ymin = conf.low, ymax = conf.high), 
                width = 0.2, size = 0.8) +
  labs(
    x = "Gear",
    y = "Likelihood of Survival"
  ) 

cy <- ggeffects::ggemmeans(modfate, terms = c("Cap_Year", "NetFree")) 
yplot <- ggplot(cy, aes(x = x, y = predicted, group = group)) +
  geom_rug(data = fategam, aes(x = Cap_Year), alpha = 0.5, sides = "b", inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  geom_line(aes(color = group), linewidth = 0.8) +
  scale_fill_manual(values = met.brewer("Demuth", n = 2),
                    labels = c("Open", "Net Free"),
                    name = "Fishing Zoning") +
  scale_color_manual(values = met.brewer("Demuth", n = 2),
                     labels = c("Open", "Net Free"),
                     name = "Fishing Zoning") +
  labs(x = "Year", y = "Likelihood of Survival") +
  theme(legend.position = c(0.3, 0.8), legend.background = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank())

reg <- ggeffects::ggemmeans(modfate, terms = "Region") 
reg$x <- factor(reg$x, 
               levels = c('GoC', 'WCY', "CSEQ", "NEQ"),
               labels = c("GoC", 'WCY', "CEQ", "NEQ"))

regplot <- ggplot() +
  geom_point(data = reg, 
             aes(x = x, y = predicted), 
             size = 3) +
  geom_errorbar(data = reg, 
                aes(x= x, ymin = conf.low, ymax = conf.high), 
                width = 0.2, size = 0.8) +
  labs(
    x = "Region",
    y = "Likelihood of Survival"
  ) 

popdn <- ggeffects::ggemmeans(modfate, terms = c("logpop", "logaDNm")) 
intplot <- ggplot(popdn, aes(x = 10^x, y = predicted, 
                             color = group, fill = group)) +
  geom_rug(data = popdn, aes(x = 10^x, color = group), alpha = 0.5, sides = "b") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(x = "Human Population Density (50 km)") +
  scale_color_manual(labels = function(x) paste(round(10^as.numeric(x), 1)),
                     values = rev(met.brewer("Greek", n = 3)), 
                     name = "Annual\nDays Netted") +
  scale_fill_manual(labels = function(x) paste(round(10^as.numeric(x), 1)),
                    values = rev(met.brewer("Greek", n = 3)), 
                    name = "Annual\nDays Netted") +
  theme(legend.position = c(0.28, 0.3), legend.background = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank())

(grplot + yplot + regplot + intplot) +
  plot_layout(design = "AB 
              CD",
              widths = c(1, 1.25),
              heights = c(1, 1)) & plot_annotation(tag_levels = list(c('a)', 'b)', 'c)', 'd)'))) &  
  theme(plot.tag = element_text(size = 14, family = "Optima"),
        axis.text = element_text(size = 13), axis.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 5, unit = "pt")),
        plot.margin = margin(t = 15, r = 10, b = 10, l = 0, unit = "pt"),
        plot.tag.position = c(0.05, 1.05)) 

ggsave(
  "figs/fig10/fig10_fatepp.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 7,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
