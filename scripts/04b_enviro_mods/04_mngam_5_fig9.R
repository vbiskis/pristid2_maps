# Details ----
#' 04_mngam_5_fig9.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 11 Mar 2025
#' - separated and reordered all components of figure, 2025-12-23
#' >> now easier for journal modifications
#' Content: 
#' + wrap in figure 9 components 
#' ++ pca (a)
#' ++ drainage (b) **THIS script (no clavata)
#' ++ other top 4 spec predictors (c-f)
#' -----------

source('helpers/functions/src_predplot.R')
source('helpers/help_plot.R')
theme_set(mytheme)

# prev plots----
## all spec----
### pca----
pcaplot <- readRDS('data/rds/pcaplot.RDS')

### model----
plot_list <- readRDS('data/rds/spec_gam_plots.RDS')

# build last one----
## data----
## no clavata----
gammy <- readxl::read_xlsx("data/processed/gammy.xlsx") 
gammync <- gammy %>% 
  filter(Species_NB != "P. clavata") # for sep modeling
gammync$Species_NB <- as.numeric(factor(gammync$Species_NB)) - 1 #need it as 0 to 2

## model----
library(mgcv)
gam_mn_nc6 <- gam(list(Species_NB ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(logDB),
                       ~ s(transout, k = 5) + s(Tmax) + s(logPrec) + s(logDB)),
                  family = multinom(K=2),
                  data = gammync)

## predictions----
seqdb <- seq(min(gammync$logDB), max(gammync$logDB), length=100)
newdata1 <- data.frame(logDB = seqdb)

newdata1$Tmax <- mean(gammync$Tmax)
newdata1$transout <- mean(gammync$transout)
newdata1$logPrec <- mean(gammync$logPrec)

predsdb <- predict(gam_mn_nc6, newdata=newdata1, type="response")

pred_df <- data.frame(
  logDB = seqdb,
  predsdb
) 

names(pred_df) <- c("logDB", "A. cuspidata", "P. pristis", "P. zijsron")
pred_dfl <- pivot_longer(
  pred_df, 
  cols = -logDB,
  names_to = "Species", 
  values_to = "Probability"
)

## and plot----
db <- ggplot(pred_dfl, 
             aes(x = logDB, y = Probability, color = Species)) +
  geom_line(size = 0.8, alpha = 0.8) +
  ylim(0,1) +
  scale_color_manual(values = c("blue4", "red3", "darkgreen"),
                     labels = c("A. cuspidata", "P. pristis", "P. zijsron")) +
  labs(x = expression("Drainage Basin Area (log"[10]*"km"^2*")"),
       y = "Probability of Encounter") +
  theme(
    #axis.text.y = element_blank(), # he needs it actually
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none",
  ) 

# stitch our fig!!----
legend <- get_legend(plot_list[[5]] + theme(legend.text = element_text(size = 12), 
                                            legend.title = element_text(size = 13)))  

top_row <- pcaplot + db + plot_list[[2]] +
  plot_layout(widths = c(2, 2, 2)) 
middle_row <- plot_list[[1]] + plot_list[[3]] + plot_list[[4]] 

# Combine with custom layout
combined_plot <- top_row  / middle_row / legend +
  plot_layout(heights = c(2, 2, 0.1)) +
  plot_annotation(tag_levels = list(c('a)', 'b)', 'c)', 'd)', 'e)', 'f)', '')), 
                  tag_prefix = '', 
                  tag_suffix = ''
  ) & theme(plot.tag = element_text(size = 13, family = "Optima"),
            plot.margin = margin(l = 2, t = 2, b = 2, r = 2, unit = "pt"),
            axis.title = element_text(size = 13), axis.text = element_text(size = 12))

# Display the combined plot
print(combined_plot)

ggsave(
  "figs/thesis_vs/v2/fig3.9/thes_specgam.png",
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
