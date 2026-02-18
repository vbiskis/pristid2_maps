# Details ----
#' 02_spc_vis_spat.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 17 Sep 2024
#' Content: 
#' + Figure for spc comp. (fig3.7a = spatial)
#' + idea of 6 year bins for analysis!
#' -----------

source("helpers/help_plot.R")
source("helpers/functions/src_summfxn.R")

sitsawc <- readxl::read_xlsx("data/processed/sitsawc.xlsx") 
theme_set(mytheme)

# Box----
sf_sum_box(sitsawc,
           x = "Species_NB", #x and z just need to be factors
           y = "Cap_Year",
           z = 'Region',
           met = mean, 
           y_lab = 'Capture Year')

# Bar----
ggplot(sitsawc, aes(Cap_Year, color=Species_NB)) +
  geom_bar(alpha = 0.0) +
  facet_wrap(~Region, ncol = 3) +
  #coord_flip() +
  theme(legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "gray37", "purple")) +
  geom_vline(xintercept = c(2006, 2017),
             color = 'darkgrey',
             linetype = 'dashed') +
  labs(title = "Mean Capture Year", 
       x= bquote("Year"),
       y= bquote("Density"),
       color = 'Species')

## Relative Bar----
year_range <- range(sitsawc$Cap_Year)

species_counts <- sitsawc %>%
  mutate(
    Year_Bin = cut(Cap_Year,
                   breaks = c(1983, 1990, 1997, 2004, 2011, 2018, 2024),
                   right = FALSE,
                   labels = c("1983-1989", "1990-1996", "1997-2003", 
                              "2004-2010", "2011-2017", "2018-2023"))
  ) %>%
  group_by(Region, Year_Bin, Species_NB) %>%
  summarize(count = n())

### build him----
species_counts <- species_counts %>%
  filter(Region != 'TS', Region != 'SEQ') %>% 
  mutate(Species_NB = factor(Species_NB, 
                             levels = names(spec_col_all)),
         Region = factor(Region, 
                         levels = c("WCY", "NEQ", "GoC", "CQ")))

RBar <- ggplot() +
  geom_bar(data = species_counts, aes(x = Year_Bin, y = count, fill = Species_NB),
           position = "fill", 
           stat = "identity") +
  facet_wrap(~Region, ncol = 2) +
  scale_fill_manual(values = spec_col_all,
                    labels = spec_labs_all) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 0.75, margin = margin(b = 5))) + 
  scale_x_discrete(labels = c("1983-1989", "1990-1996", "1997-2003", 
                              "2004-2010", "2011-2017", "2018-2023")) + 
  labs(x = "Year",
       y = "Relative Abundance",
       fill = "Species")
RBar

ggsave(
  "figs/thesis_vs/v2/fig3.7/alt_spcp_6ybins.png",
  plot = RBar,
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
