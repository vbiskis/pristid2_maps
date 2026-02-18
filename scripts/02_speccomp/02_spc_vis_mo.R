# Details ----
#' 02_spc_vis_mo.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Sep 2024
#' Content: 
#' + pulled the month figure out for 3.7
#' -----------

source("helpers/help_plot.R")
library(readxl)

sitsawks <- readxl::read_xlsx("data/processed/sitsawks.xlsx") 
theme_set(mytheme)

#fishing effort----
subs <- readxl::read_xlsx("data/Sub_Fishing.xlsx")
fish_eff <- table(subs$Cap_Mo)
print(fish_eff)
fish_mo <- as.data.frame.table(fish_eff)
names(fish_mo)[1] <- "Month"
str(fish_mo)
fish_mo <- fish_mo %>%
  mutate(Month = as.numeric(Month),
         Freq = as.numeric(Freq))

ggplot(fish_mo, aes(x = Month,
                    y = Freq)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme_bw() +
  scale_x_continuous(breaks = c(3, 6, 9, 12), labels = c("Mar", "June", "Sep", "Dec")) +
  labs(y = "",
       x = "") +
  geom_line()

#Is it sig?----
sitsbymo <- sitsawks %>%
  group_by(Cap_Mo, Species_NB) %>%
  summarize(count = n(), .groups = "drop")

species_long <- sitsbymo %>%
  group_by(Species_NB) %>%
  mutate(species_prop = count / sum(count)) %>%
  ungroup() %>%
  dplyr::select(Species_NB, Cap_Mo, species_prop)

sub_long <- fish_mo %>%
  mutate(sub_prop = Freq / sum(Freq)) %>%
  dplyr::select(Month, sub_prop)

sm <- ggplot() +
  geom_col(data = species_long, 
           aes(x = Cap_Mo, y = species_prop, fill = Species_NB),
           position = "stack", width = 0.7) +
  geom_line(data = sub_long, 
            aes(x = Month, y = sub_prop, group = 1),
            color = "black", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(3, 6, 9, 12), labels = c("Mar", "Jun", "Sep", "Dec")) +
  scale_fill_manual(values = spec_col_ks, labels = spec_labs_ks) +
  labs(
    x = "Month",
    y = "Proportion of Annual Total",
    fill = "Species"
  ) +
  theme(
    legend.position = "none", 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 13), 
    plot.margin = margin(t = 0, l = 0, b = 0, r = 0, unit = "pt"))

sm + plot_annotation(tag_levels = list(c('b')), tag_suffix = ')') &
  theme(plot.tag = element_text(size = 15, family = "Optima"),
        plot.tag.position = c(0.015, 1))

ggsave(
  "figs/fig6/fig6b_spcp_mo.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
