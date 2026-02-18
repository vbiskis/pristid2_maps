# Details ----
#' 02_spc_teps.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Apr 2025
#' + upgraded 22 Dec 2025 -> split to multiple
#' Content: 
#' + TEP monthly vs subs monthly spec comp
#' -----------

source("helpers/help_plot.R")
library(ggpattern)
theme_set(mytheme)

tep <- readxl::read_xlsx("data/processed/tep_intxns.xlsx")
sitsenv <- readxl::read_xlsx("data/processed/sitsenv.xlsx")

# get him ready----
sitsbymo <- sitsenv %>%
  mutate(Species_NB = if_else(Species_NB == 'Pristis sp.', 
                              'Pristidae', Species_NB)) %>% # not bothering with the genus
  group_by(Region, Cap_Mo, Species_NB) %>%
  summarize(count = n(), .groups = "drop")

species_long <- sitsbymo %>%
  group_by(Region, Species_NB) %>%
  mutate(species_prop = count / sum(count)) %>%
  ungroup() 

# subs----

subs_coast <- species_long %>% 
  filter(!(Region %in% c('TS'))) %>% 
  mutate(Coast = case_when(Region %in% c('WCY', 'GoC') ~ 'West Coast',
                           .default = 'East Coast'),
         Quarter = case_when(Cap_Mo <= 3 ~ 'Q1',
                             Cap_Mo > 3 & Cap_Mo <= 6 ~ 'Q2',
                             Cap_Mo > 6 & Cap_Mo <= 9 ~ 'Q3',
                             .default = 'Q4'),
         Coast = factor(Coast, levels = c("West Coast", "East Coast")))

sc1 <- ggplot() +
  geom_col(data = subs_coast, 
           aes(x = Quarter, y = count, fill = Species_NB),
           position = "stack") +
  facet_wrap(~ Coast, nrow = 1,
             scales = "free") +
  scale_fill_manual(values = spec_col_fam,
                    labels = spec_labs_fam) +
  labs(
    x = "Quarter", y = "Freq. of Interaction", fill = "Species"
  ) +
  theme(
    legend.position = "bottom",
    plot.margin = margin(l = 5, t = 2, b = 2, unit = "pt"),
    strip.text = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt")),
    axis.title.x = element_text(margin = margin(t = 2, b = 2, unit = "pt")))

# vs. teps----
tep_mut <- tep %>% #only want recent for season
  filter(Year >= 2010) %>%
  group_by(Method, Region, Quarter, Species_NB) %>% 
  summarise(count = sum(`Total Interactions`), .groups = "drop") %>% 
  mutate(Method = factor(Method, levels = c("Gillnetting", "Trawling", "Other")),
         Region = factor(Region, levels = c("GOC", "EC")))

tp1 <- ggplot() +
  geom_col_pattern(data = tep_mut, 
                   aes(x = Quarter, y = count, 
                       fill = Species_NB,
                       pattern = Method),
                   position = "stack",
                   pattern_spacing = 0.025) +
  facet_wrap( ~ Region,
             scales = "free_y",
             labeller = labeller(Region = c("GOC" = "West Coast", 
                                            "EC" = "East Coast"))) +
  scale_fill_manual(values = c("blue4", "gold2", "red3", "darkgreen", "grey40")) +
  scale_pattern_manual(values = c("Gillnetting" = "none",
                                  "Trawling" = "stripe", 
                                  "Other" = "circle")) +
  labs(
    x = "", y = "Freq. of Interaction", fill = "Species",
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(l = 5, t = 2, b = 2, unit = "pt"),
    strip.text = element_text(face = "plain", hjust = 0,
                              margin = margin(t = 0, b = 5, unit = "pt")),
    panel.spacing.x = unit(0.5, "lines"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt")))

(tp1 & theme(plot.tag.position = c(-0.02, 0.87))) / 
  (sc1 & theme(plot.tag.position = c(-0.02, 1))) + 
  plot_layout(ncol = 1) + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 13, family = "Optima"))

ggsave(
  "figs/supps/s4/s4_tep_v_subs.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 6,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

# is it sig ----
tep_q <- tep %>% 
  select(-`Fishing Method`, -Species) %>% 
  filter(Species_NB != "Pristidae") %>% 
  group_by(Region, Year, Quarter, Species_NB) %>% 
  summarise(Intxns = sum(`Total Interactions`)) 

tep_ec <- tep_q %>% 
  filter(Region == 'EC')

ec_tabm <- tep_ec %>%
  group_by(Species_NB, Quarter) %>%
  summarise(totint = sum(Intxns))%>%
  pivot_wider(names_from = Quarter, 
              values_from = totint, 
              values_fill = 0) %>%
  column_to_rownames("Species_NB") %>%
  as.matrix()

chisq.test(ec_tabm) #sig diff quarterly dist by species

tep_goc <- tep_q %>% 
  filter(Region == 'GOC')

goc_tabm <- tep_goc %>%
  filter(Quarter != "Q4") %>% 
  group_by(Species_NB, Quarter) %>%
  summarise(totint = sum(Intxns))%>%
  pivot_wider(names_from = Quarter, 
              values_from = totint, 
              values_fill = 0) %>%
  column_to_rownames("Species_NB") %>%
  as.matrix()

chisq.test(goc_tabm) #sig diff quarterly dist by species

