# Details ----
#' 02_spc_vis_wgear.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Dec 2025
#' Content: 
#' + same graphic but
#' + decreasing bin # (as in analysis)
#' + and adding alpha for gear
#' -----------

source("helpers/help_plot.R")
sits_cln <- readxl::read_xlsx("data/processed/sits_cln.xlsx") 
theme_set(mytheme)

# plot----
sc <- ggplot(sits_cln %>% 
               mutate(Region = factor(Region, c('WCY', 'NEQ', 'GoC', 'CQ'))), 
             aes(x = ColY2, fill = Species_NB, alpha = Gear2)) +
  geom_bar(position = "fill") +
  facet_wrap(~Region, nrow = 2) +
  scale_fill_manual(values = spec_col_ks, labels = spec_labs_ks) +
  scale_alpha_manual(values = c(1, 0.5)) +
  labs(y = "Proportion", x = "", fill = "Species") +
  theme(legend.position = "bottom", legend.spacing.x = unit(0.5, "cm"),
        legend.justification = c(1, 0), legend.box = "horizontal",
        legend.title = element_text(size = 14), legend.text = element_text(size = 13),
        axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 0.75, 
                                   size = 13, margin = margin(b = 5)),
        panel.spacing = unit(0.5, "lines"),
        strip.text = element_text(face = "plain", hjust = 0, size = 14,
                                  margin = margin(t = 0, b = 5, unit = "pt")),
        plot.margin = margin(t = 0, l = 0, b = 0, r = 0, unit = "pt")) +
  labs(x = "Time Bin",
       y = "Relative Abundance",
       fill = "Species",
       alpha = "Gear Type")

sc + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 15, family = "Optima"),
        plot.tag.position = c(0.015, 1))

ggsave(
  "figs/fig6/fig6a_spcp.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8.5,
  height = 5.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

# I like him!
# So this trend isn't a bad thing... it makes sense and is actually relatively uniform
# The issue in analysis is due to missing cats in bins!
# We WILL have to collapse these then to do it properly