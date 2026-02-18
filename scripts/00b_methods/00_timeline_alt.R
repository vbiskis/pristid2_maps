# Details ----
#' 00_timeline_dens.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date:Feb 26 2024
#' - Split to sep script 2025-12-09 
#' Content: General settings for stats
#' -----------

#Starting out with a first look at the data
#number of catches/sightings submitted through time

#Load em up----
sitsawc <- read_xlsx('data/processed/sitsawc.xlsx')

#having a look
str(sitsawc)
statvar <- sitsawc[,c(6, 8:10, 13:14, 19, 21)] #cont/ordinal only!
str(statvar)
ggpairs(statvar)
#looks good, expected collinearities, ie. lat/long, age/size

#Start with large scale species analysis
#Over years

#Subs Summary----
#Using confirmed only cat for comparison (sitsawc)

#By Time
ggqqplot(sitsawc$Cap_Year)
ggdensity(sitsawc$Cap_Year)
shapiro.test(sitsawc$Cap_Year) #nonparametric

QLDA1 <- aov(Cap_Year ~ Species_NB, data = sitsawc) #1Way
summary(QLDA1) #but year is non para
kruskal.test(Cap_Year ~ Species_NB, data = sitsawc)
#nonsig, similar mean cap years

#Pristidae screwing with things - use sitsawks (species level known only)
kruskal.test(Cap_Year ~ Species_NB, data = sitsawks) #still not sig

# stacked bars----
#how do I want to show this

ggplot(sitsawc, aes(Cap_Year, color=Species_NB)) + #Opt 3, via bars
  geom_bar(alpha = 0.0) +
  #coord_flip() +
  theme(legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "gray37", "purple")) +
  geom_vline(xintercept = 2017,
             color = 'darkgrey',
             linetype = 'dashed') +
  labs(title = "Mean Capture Year", 
       x= bquote("Year"),
       y= bquote("Density"),
       color = 'Species')

# densities----

ggplot(sitsawc, aes(Cap_Year, color=Species_NB)) +
  geom_density() +
  theme(legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "grey37", "purple")) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1000, name = "Submissions")) +
  geom_vline(xintercept = 2015,
             color = 'darkgrey',
             linetype = 'dashed') +
  annotate(geom = "text", x = 2014.5, y = 0.07, 
                label = "Submissions Opened", 
                angle = 90, size = 3.5, 
            color = "darkgrey", fontface = "plain",
            hjust = 0, vjust = 0) +
  geom_vline(xintercept = 2017.5,
             color = 'grey25',
             linetype = 'dashed') +
  annotate(geom = "text", x = 2017, y = .07, 
                  label = "Sightings Campaign", 
                  angle = 90, size = 3.5, 
              color = "grey25", fontface = "plain",
              hjust = 0, vjust = 0) +
  labs(title = "Mean Capture Year", 
       x= bquote("Year"),
       y= bquote("Density"),
       color = 'Species')

##Print----
ggsave(
  "figs/fig3.4/fig4_alt_subsdist.tiff",
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
