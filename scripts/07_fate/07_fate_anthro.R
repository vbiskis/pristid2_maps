# Details ----
#' 07_fate_anthro.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 7 Apr 2025
#' - modified 2025-12-30 (consolidated)
#' Content: 
#' + base stats for fate section intro
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
theme_set(mytheme)

fatedf <- readRDS('data/rds/sits_fatemod.RDS')

# base----

fatedf %>% 
  group_by(
    Species_NB, Sub_Type
  ) %>% 
  summarise(
    alive = sum(Fate == 1),
    dead = sum(Fate == 0),
    prob = alive/n()
  )

# visualise rlsps----
ggplot(fatedf) +
  geom_point(aes(x = Cap_Year, y = Fate)) +
  geom_smooth(aes(x = Cap_Year, y = Fate), method = "gam")

ggplot(fatedf) + # where ppl catch them stays low; mean value
  geom_point(aes(x = Cap_Year, y = logpop)) +
  geom_smooth(aes(x = Cap_Year, y = logpop), method = "gam") 

ggplot(fatedf) +
  geom_point(aes(x = Cap_Year, y = logprotg)) + #increasing with time
  geom_smooth(aes(x = Cap_Year, y = logprotg), method = "gam") 

ggplot(fatedf) +
  geom_point(aes(x = logpop, y = logprotg)) + # hmmm interesting
  geom_smooth(aes(x = logpop, y = logprotg), method = "gam") 

ggplot(fatedf) +
  geom_point(aes(x = Cap_Year, y = logmavDNm)) + #decreasing with time
  geom_smooth(aes(x = Cap_Year, y = logmavDNm), method = "gam") 

ggplot(fatedf) +
  geom_point(aes(x = logpop, y = logRF)) +
  geom_smooth(aes(x = logpop, y = logRF), method = "gam") # damn!

# fate tests ----
## gear v time----
facet_lab <- c("historic" = "Pre-2009",
               "recent" = "Post-2009")
ggplot(data = fatedf) +
  geom_mosaic(aes(x=product(Gear), fill = Alive2),
              direction = "x") +
  facet_grid(~Sub_Type, scales = "free_x",
             labeller = labeller(Sub_Type = facet_lab)) +
  scale_y_continuous() +
  scale_x_productlist(labels = c('Cast Net', 'Line', 'Gillnet', 'Trawl', 'Sighted')) +
  xlab("Method of Capture") +
  ylab("Relative Frequency") +
  scale_fill_manual(values = c("grey", "black")) +  # Custom fill colors
  labs(fill = 'Released Alive') +
  theme(legend.position = "bottom")

ggsave(
  "figs/addl/fate/addl_geartime.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

# gear v size----
ggplot(fatedf, aes(Size_Final, Gear, color = Alive2)) +
  geom_violin() +
  #facet_wrap(~Sub_Type) +
  #coord_flip() +
  labs(x = bquote('Size'),
       y = bquote(""),
       color = "Fate")

t.test(logTL2 ~ Alive2, 
       data = fatedf)

fatedf %>%
  group_by(Alive2) %>%
  dplyr::summarize(MeanSize = mean(Size_Final, na.rm = TRUE),
            SE = sd(Size_Final, na.rm = TRUE) / sqrt(n()))

# trophies ----
## size v time----
ggplot(fatedf, aes(Cap_Year, logTL2, color = Sawless2)) +
  geom_point() +
  stat_ellipse(level = 0.95) +
  scale_color_manual(values = c("darkred", "darkgreen"),
                     labels = c("No", "Yes")) +    
  labs(title = "Trophy Taking", 
       color = 'Trophy Taken',
       x= bquote('Year'),
       y= bquote("Body Size"))

fatedf %>%
  group_by(Sub_Type, Sawless2) %>%
  dplyr::summarize(MeanSize = mean(Size_Final, na.rm = TRUE),
            SE = sd(Size_Final, na.rm = TRUE) / sqrt(n()))

fatedf %>%
  group_by(Gear, Sawless2) %>%
  dplyr::summarize(MeanSize = mean(Size_Final, na.rm = TRUE),
            SE = sd(Size_Final, na.rm = TRUE) / sqrt(n()))

t.test(logTL2 ~ Sawless2, 
       data = fatedf)

