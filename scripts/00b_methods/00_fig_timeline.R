# Details ----
#' 00_fig_timeline.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - Feb 2024
#' Content: 
#' +  start of results figure - timeline
#' -----------

source("helpers/help_plot.R")
theme_set(mytheme)

sitsub <- readxl::read_xlsx("data/SubmDeets.xlsx") #submitter breakdown
sitsawc <- readxl::read_xlsx("data/processed/sitsawc.xlsx") #submitter breakdown

## Totals----
Sub_counts <- table(sitsub$Sub_Year)
sub_counts <- as.data.frame.table(Sub_counts)
names(sub_counts)[1] <- "Year"
sub_counts$Year <- as.character(sub_counts$Year) #stupid I have to do this
sub_counts <- sub_counts %>%
  mutate(Year = as.numeric(Year),
         Freq = as.numeric(Freq))

subct <- ggplot() +
  geom_line(data = sub_counts,
            aes(x = Year,
                y = Freq)) +
  labs(y = bquote("Annual \nSubmissions")) +
  geom_vline(xintercept = 2015,
             color = 'darkgrey',
             linetype = 'dashed') +
  geom_vline(xintercept = 2017.5,
             color = 'grey25',
             linetype = 'dashed') +
  scale_x_continuous(limits = c(1990, max(sub_counts$Year))) + # Set x-axis limits
  theme(plot.margin = unit(c(5, 9, 2, -2), "pt"),
        axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 13)) 

## Species Counts----
species_countsALL <- sitsawc %>%
  group_by(Cap_Year, Species_NB) %>%
  summarize(count = n())

plotord <- c("A. cuspidata", "P. clavata", "P. pristis", "P. zijsron", "Pristis sp.", "Pristidae")

sitsbub <- ggplot(species_countsALL, 
                  aes(x = Cap_Year, y = factor(Species_NB, levels=rev(plotord)), 
                                         color = Species_NB, size = count)) +
  geom_vline(xintercept = 2015,
             color = 'darkgrey',
             linetype = 'dashed') +
  geom_vline(xintercept = 2017.5,
             color = 'grey25',
             linetype = 'dashed') +
  geom_point() +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "gray37", "purple"),
                     labels = spec_labs_all) +
  scale_size_continuous(range = c(1, 10)) + 
  scale_y_discrete(labels = rev(spec_labs_all)) +
  theme(legend.position = "right",
        legend.text = element_text(size = 13),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 13),
        plot.margin = unit(c(2, 5, 2, 10), "pt")) +  
  guides(color = "none") +
  labs(title = NULL,
       x = "Year",
       color = "Species",
       size = "Frequency") 

## And legends----
lsiz <- get_legend(
  sitsbub + guides(size = guide_legend(nrow = 4)) +
    theme(legend.position = "right")
)

base <- cowplot::plot_grid(lsiz, subct, labels = c("", "b)"),
                           label_size = 12, label_fontface = "plain", label_fontfamily = "Optima",
                           label_x = c(1.0, -0.025), ncol = 2, rel_widths = c(0.2675,1))

# Final Fig----
cowplot::plot_grid(sitsbub + theme(axis.title.x = element_blank(),
                                   axis.text.x = element_blank(),
                                   legend.position = "none"), 
                   base, labels = c("a)", ""),
                   label_fontface = "plain", label_fontfamily = "Optima", label_size = 14,  
                   nrow = 2, rel_heights = c(3,1))

ggsave(
  "figs/fig4/fig4_sitsdist.png",
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
