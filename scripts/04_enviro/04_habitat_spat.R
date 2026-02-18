# Details ----
#' 04_habitat_spat.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 19 Sept 2024
#' + upgraded 22 Dec 2025 -> split to multiple
#' Content: 
#' + key factors affecting species capture
#' ++ dist to mouth, stream order ~ species
#' ++ overlaid with rec fishing pressure & TL if individual
#' -----------

source("helpers/help_plot.R")
library(ggdensity)

theme_set(mytheme)

envmod <- readxl::read_xlsx("data/processed/sits_envmod.xlsx") # our full env guy
sitsmods <- readxl::read_xlsx("data/processed/sitsmods.xlsx") # and the anth guy

str(sitsmods)
#To River Mouth----
#but is it upstream or offshore?
spec_pred <- envmod %>% # ll do something based on this for the full models
  dplyr::select(SUB, Species_NB, Gear,
                Cap_Year, Cap_Mo, Cap_Day,
                GPS_type, Bord_In, 
                STRM_ORDER, DB_area,
                DTO, DTM, DTO_IO, transout) %>% 
  left_join(sitsmods %>% 
              dplyr::select(SUB, logRF, dist_Cl_RF, NetFree, 
                            Size_Final, logTL2, Age_Class)) %>% 
  mutate(DTM_IO = case_when(Bord_In == 0 ~ DTM * -1,
                                   .default = DTM),
         logdtio = case_when(Bord_In == 0 ~ log(DTO) * -1,
                             .default = log(DTO)),
         Maturity = if_else(Age_Class == "adult", "Mature", "Immature")) 

spec_predj <- spec_pred[!(spec_pred$GPS_type %in% 
                           c("3", '4')),] #only accurate GPS points considered
spec_predj <- spec_predj[!(spec_predj$Species_NB %in% c("Pristidae", 'Pristis sp.')),] # and known spec

QLDA6 <- aov(DTM_IO ~ Species_NB*Maturity, data = spec_predj) #1Way
summary(QLDA6) # ya for sure

ggplot(spec_predj, aes(Species_NB, DTM_IO, color = Species_NB)) +
  geom_boxplot() +
  coord_flip() +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "gray37", "purple")) +
  theme(axis.text.y = element_text(face = "italic"),
        legend.position = "none") +
  labs(x= bquote('Species'),
       y= bquote("Distance (km)"),
       color= 'Species')

##Distance to Cl Outlet----
ggplot(spec_predj, aes(Species_NB, DTO_IO, color=Species_NB)) +
  geom_boxplot() +
  coord_flip() +
  scale_color_manual(values = c("blue4", "gold2", "red3", "darkgreen", "gray37", "purple")) +
  theme(axis.text.y = element_text(face = "italic"),
        legend.position = "none") +
  labs(x= bquote('Species'),
       y= bquote("Distance (km)"),
       color= 'Species')

#is this affected by flow?
sitsclose <- spec_predj[(spec_predj$DTO_IO >= -25),] #max 25 km from mouth river

QLDA4 <- manova(cbind(STRM_ORDER, DTO_IO) ~ Species_NB*Maturity, data = spec_predj)
summary(QLDA4)

rfe <- lm(logRF ~ logdtio*log10(DB_area), data = spec_pred)
plot(rfe) #gross!
summary(rfe)

spec_predj %>% 
  group_by(Species_NB) %>% 
  summarise(
    minrf = min(10^logRF, na.rm = TRUE),
    maxrf = max(10^logRF, na.rm = TRUE),
    rf = mean(10^logRF, na.rm = TRUE),
    rfse = sd(10^logRF, na.rm = TRUE)/sqrt(sum(!is.na(logRF))),
    distcl = mean(dist_Cl_RF),
    bse = sd(dist_Cl_RF, na.rm = TRUE)/sqrt(sum(!is.na(dist_Cl_RF)))
  )

rfp <- lm(dist_Cl_RF ~ Species_NB, data = spec_predj)
summary(rfp)
TukeyHSD(aov(rfp)) # ya so it's just pristis

ggplot() +
  stat_summary_2d(data = spec_predj,
                  aes(x = STRM_ORDER, y = logdtio, z = logRF), 
                  fun = mean, bins = 8, alpha = 0.75) +
  scale_fill_gradient(low = "white", high = "red", 
                      name = "Mean Annual Trips",
                      labels = function(x) signif(exp(x), -1)) +
  geom_point(data = spec_predj,
             aes(x = STRM_ORDER, y = logdtio,
                 color = Species_NB, size = exp(logTL2))) +
  geom_hdr_lines(data = spec_predj,
                 aes(x = STRM_ORDER, y = logdtio, color = Species_NB),
                 probs = c(0.25, 0.5, 0.75), show.legend = FALSE) +
  facet_grid( ~ Species_NB,
              labeller = labeller(Species_NB = c('A. cuspidata' = 'a)', 
                                                 'P. clavata' = 'b)',
                                                 'P. pristis' = 'c)', 
                                                 'P. zijsron' = 'd)'))) +
  scale_size(name = "Total Length (cm)") +
  scale_color_manual(values = spec_col_ks,
                     name = "Species", guide = guide_legend(
                       label.theme = element_text(face = "italic", 
                                                  size = 12))) +
  scale_x_continuous(breaks = c(2, 4, 6, 8),
                   labels = c("2", "4", "6", "8")) +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 0, round(exp(x), -1), -round(exp(-x), -1))
  }) + 
  xlab("Strahler Stream Order") +
  ylab("Distance from Outlet (km)") +
  theme(plot.margin = margin(l = 5, t = 2, b = 2, unit = "pt"),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "plain", hjust = 0, size = 13,
                                  margin = margin(t = 0, b = 5, unit = "pt")),
        panel.spacing.x = unit(0.5, "lines"),  panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-log(100), log(400)))

ggsave(
  "figs/thesis_vs/v2/fig3.s5/thes_rfe.png",
  plot = last_plot(),
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

QLDA5 <- aov(STRM_ORDER ~ Species_NB*Maturity, data = spec_predj)
summary(QLDA5) #ha still sig even discounting the far ones!

dist_num <- spec_predj %>%
  select_if(is.numeric)

ggcorr(dist_num, label = FALSE)
