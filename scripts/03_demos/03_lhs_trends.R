# Details ----
#' 03_lhs_trends.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - New new 2025-12-13
#' Content: 
#' + use lhs df 
#' + looking for clustering according to lhs
#' Output:
#' + detailed supps props graph
#' -----------

source('helpers/help_plot.R')
theme_set(mytheme)

library(emmeans) # for stats
library(ggh4x) # for plots!

sitslhs <- readxl::read_xlsx("data/processed/sitslhs.xlsx") 
alllhsbass <- readRDS('data/rds/alllhsbass.RDS')
basin_lhs <- readRDS('data/rds/basin_lhs.RDS')

basin_lhs <- basin_lhs %>% 
  mutate(Species_NB = factor(Species_NB, levels = c('A. cuspidata', 'P. clavata', # gonna filter him out anyway but
                                                    'P. zijsron', 'P. pristis')))

alllhsbass <- alllhsbass %>% 
  mutate(Species_NB = factor(Species_NB, levels = c('A. cuspidata',  'P. clavata', 
                                                    'P. zijsron',  'P. pristis')))
# stats testing----

# For each species
for(sp in unique(basin_lhs$Species_NB)) { # do any basins have sig higher juv prob?
  sp_data <- sitsdemo %>% filter(Sub_Type == 'recent',
                                Species_NB == sp)
  model <- glm(YOY ~ Dbasin, 
               data = sp_data, 
               family = binomial)
  
  basin_probs <- emmeans(model, "Dbasin", type = "response")
  print(sp)
  print(basin_probs)
}

pairs(basin_probs)

# Visualize hotspots----
# lets separate West/East Coast 
all_basins <- levels(sitslhs$Dbasin)
basin_set <- all_basins[all_basins %in% unique(c(basin_lhs$Dbasin, alllhsbass$Dbasin))]

x_scales <- list(
  scale_x_discrete(limits = west_basins),
  scale_x_discrete(limits = east_basins)
) #for both

WC <- ggplot(basin_lhs %>% 
                filter(Coast == "W", Species_NB != 'P. clavata')) +
  geom_point(data = alllhsbass %>% 
               filter(n_total == 1, Coast == 'W') %>% 
               mutate(LHS = case_when(n_yoy == 1 ~ 'yoy', n_ad == 1 ~ 'adult',
                                      .default = 'juvenile'),
                      LHSf = factor(LHS, levels = c('yoy', 'juvenile', 'adult'))), 
             aes(x = Dbasin, y = 0, fill = LHSf),
             color = 'black', shape = 21, size = 3, stroke = 0.5) +
  geom_col(aes(x = Dbasin, y = 1), fill = 'grey30', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_imm), fill = 'grey50', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_juv), fill = 'grey80', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_yoy), fill = "white", color = 'black') +
  facet_grid2( ~ Species_NB, scales = "free", remove_labels = "all", drop = TRUE) +  
  scale_x_discrete(limits = west_basins) +
  coord_flip() +
  scale_fill_manual(values = c('yoy' = 'white', 
                               'juvenile' = 'grey80', 
                               'adult' = 'grey30'),
                    labels = c('YOY', 'Juvenile', 'Adult'),
                    name = 'Life History Stage') +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
  theme(strip.text.x = element_text(face = "italic", margin = margin(b = 4)),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(l = 0, r = 5, unit = "pt")), 
        panel.spacing.x = unit(0.5, "lines"),
        legend.position = "none", plot.margin = margin(t = 0, b = 5, l = 0, r = 0, unit = "pt")) +
  labs(x = "Basin", y = "Proportion of Occurrences with Known TL")

EC <- ggplot(basin_lhs %>% 
               filter(Coast == "E")) +
  geom_point(data = alllhsbass %>% 
               filter(n_total == 1,
                      Coast == "E",
                      Species_NB != 'P. clavata') %>% 
               mutate(LHS = case_when(n_yoy == 1 ~ 'yoy',
                                      n_ad == 1 ~ 'adult',
                                      .default = 'juvenile'),
                      LHSf = factor(LHS, levels = c('yoy', 'juvenile', 'adult'))), 
             aes(x = Dbasin, y = 0, fill = LHSf),
             color = 'black', shape = 21, size = 3, stroke = 0.5) +
  geom_col(aes(x = Dbasin, y = 1), fill = 'grey30', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_imm), fill = 'grey50', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_juv), fill = 'grey80', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_yoy), fill = "white", color = 'black') +
  facet_grid2( ~ Species_NB, scales = "free", remove_labels = "all") +  
  scale_x_discrete(limits = east_basins) +
  coord_flip() +
  scale_fill_manual(values = c('yoy' = 'white', 
                               'juvenile' = 'grey80', 
                               'adult' = 'grey30'),
                    labels = c('YOY', 'Juvenile', 'Adult'),
                    name = 'Life History Stage') +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
  theme(strip.text = element_blank(), 
        axis.title.y = element_text(margin = margin(l = 0, r = 5, unit = "pt")), 
        panel.spacing.x = unit(0.5, "lines"),
        legend.position = c(0.85, 0.25), legend.background = element_rect(linewidth = 0.5),
        plot.margin = margin(t = 5, b = 0, l = 0, r = 0, unit = "pt")) +
  labs(x = "Basin", y = "Proportion of Occurrences with Known TL")

## last modification----
pc <- ggplot(basin_lhs %>% 
         filter(Species_NB == 'P. clavata')) +
  geom_col(aes(x = Dbasin, y = 1), fill = 'grey30', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_imm), fill = 'grey50', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_juv), fill = 'grey80', color = 'black') +
  geom_col(aes(x = Dbasin, y = prop_yoy), fill = "white", color = 'black') +
  facet_grid2( ~ Species_NB, scales = "free", remove_labels = "all") +  
  coord_flip() +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
  theme(strip.text.x = element_text(face = "italic", margin = margin(b = 4)),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none", plot.margin = margin(t = 5, b = 5,
                                                       r = 5, l = 5, unit = "pt"))

WC / EC + plot_layout(heights = c(1.5, 2.5)) +
  inset_element(pc, 
                left = 0.2255, right = 0.6604,
                bottom = 1.2955, top = 1.4017) & theme(plot.margin = margin(l = 0, unit = "pt"))

ggsave(
  "figs/thesis_vs/v2/fig3.s4/thes_dems_percent.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

  