# Details ----
#' 06_TL_reg_plots.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2024 Oct 2024
#' Content: 
#' TL/age plots for linear section
#' -----------

source('helpers/help_plot.R')
source('helpers/help_stats.R')
source('helpers/functions/src_summfxn.R')
theme_set(mytheme)

sitsiz <- read_xlsx('data/processed/sitsiz.xlsx')
sizpredks <- read_xlsx('data/processed/sizpredks.xlsx')

#Check Size DFs----
ggqqplot(sitsiz$logTL2)
ggdensity(sitsiz$logTL2) #nice!

# Pre-Post----

summary(logTL2 ~ Species_NB, data = sitsiz) #1Way
summary(logTL2 ~ Species_NB + Sub_Type, data = sitsiz) #2Way

## BoxPlot----

ggplot(sitsiz, aes(Species_NB, Size_Final, color=Species_NB)) +
  facet_wrap(~Sub_Type) +
  geom_boxplot() +
  scale_color_manual(values = spec_col_all, 
                     labels = spec_labs_all) +
  theme(legend.position = "none") +
  labs(x= bquote('Species'),
       y= bquote("Size (cm)"))

## Age Dist----

num_levels <- length(levels(sizpredks$Species_NB))
anox <- num_levels / 4
pristids <- 3*anox

ageg <- ggplot(sizpredks, aes(Species_NB, Est_Age, color=Species_NB)) +
  geom_boxplot() +
  facet_wrap(~Sub_Type,
             labeller = labeller(Sub_Type = c("historic" = "1988 - 2009", 
                                              "recent" = "2010 - 2023"))) +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen")) +
  theme(axis.text.x = element_text(face = "italic"),
        strip.text = element_text(margin = margin(4, 0, 4, 0)),
        strip.background = element_rect(color = "lightgray"),
        legend.position = "none",
        #panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(5, 5, 10, 10), "pt")) +
  scale_y_continuous(trans = "log10") +
  geom_segment(aes(x = 1.5, xend = 4.5, y = 8, yend = 8),
             color = 'black',
             linetype = 'dashed') +
  geom_segment(aes(x = 0.5, xend = 1.5, y = 2, yend = 2),
             color = 'darkgrey',
             linetype = 'dashed') +
  labs(x= bquote('Species'),
       y= bquote("Age (y)"),
       color= 'Species')
ageg

##Line Graph----
sizg <- ggplot(sizpredks, aes(x=Cap_Year, y=Size_Final, color=Species_NB)) +
  geom_point() +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "gray37")) +
  theme(legend.text = element_text(face = "italic"),
        legend.position = "none",
        axis.text.x = element_text(margin = margin(b = -7)),
        plot.margin = unit(c(5, 5, 10, 10), "pt")) +
  scale_y_continuous(trans = "log10") +
  labs(x= bquote('Year'),
       y= bquote("Total Length (cm)"),
       color= 'Species')

sizg #so sad

#lets combine:
plot_grid(sizg + theme(legend.position = "none",
                       plot.title = element_blank()),
          ageg + theme(plot.title = element_blank()),
          label_fontface = "plain", 
          label_fontfamily = "optima",
          label_size = 13,  
          label_x = 0.01,
          nrow = 2, rel_heights = c(1.75, 1),
          labels = c("a)", "b)"))

ggsave(
  "figs/thesis_vs/v2/fig3.10/thes_TLdecline.png",
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
