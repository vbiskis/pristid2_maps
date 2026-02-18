# Details ----
#' 02_fig_wetdry.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 28 Mar 2025
#' + upgraded 22 Dec 2025 -> just for style :)
#' Content: 
#' + methods figure, seasonality
#' -----------

source("helpers/help_plot.R")
theme_set(mytheme)

month_abbr <- c(month.abb[10:12], month.abb[1:9]) #Gotta say start with Oct and go around

month_labels <- c(month_abbr, "Oct") #want an extra one for complete circle
x <- 1:13  
y <- sin(2*pi*(x-1)/12)  #Start with wet season

seasonal_data <- data.frame(
  month_num = x,
  month_label = month_labels,
  season_value = y
)

ggplot(seasonal_data, aes(x = month_num, y = season_value, group = 1)) +
  geom_line(size = 1.2, color = "darkgrey") +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 4, linetype = "solid", color = "lightgrey") +
  geom_vline(xintercept = 10, linetype = "solid", color = "lightgrey") +
  annotate("text", x = 1.95, y = 0.28, label = "Start of Wet", 
           color = "darkblue", fontface = "bold", family = "optima", size = 5) +
  annotate("text", x = 7.92, y = 0.28, label = "Start of Dry", 
           color = "brown", fontface = "bold", family = "optima", size = 5) +
  annotate("segment", x = 1, y = 0.19, xend = 1, yend = 0.07, 
           arrow = arrow(length = unit(0.2, "cm")), color = "darkblue") +
  annotate("segment", x = 7, y = 0.19, xend = 7, yend = 0.07, 
           arrow = arrow(length = unit(0.2, "cm")), color = "brown") +
  scale_x_continuous(breaks = 1:13, labels = month_labels) +
  scale_y_continuous(limits = c(-1.1, 1.1),
                     breaks = c(-1, 0, 1),
                     labels = c("Dry (-1)", "Neutral (0)", "Wet (1)")) +
  labs(x = "Month",
       y = "Season") +
  theme(
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 13), axis.title = element_text(size = 14), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 2, b = 2, l = 2, r = 2, unit = "pt")
  )

ggsave("figs/fig3/fig3_methods_season.tiff", width = 8, height = 4)
