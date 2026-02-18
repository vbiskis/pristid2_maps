# Details ----
#' 02_spc_vis_mo_region.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 14 Apr 2025
#' + 22 Dec 2025 -> split to multiple
#' Content: 
#' + same as 02_spc_vis_mo but split by region to see spec effect
#' -----------

source("helpers/help_plot.R")
subs <- readxl::read_xlsx("data/Sub_Fishing.xlsx")
sitsenv <- readxl::read_xlsx("data/processed/sitsenv.xlsx")

# Summarise----
# cytags----
subs <- readxl::read_xlsx("data/Sub_Fishing.xlsx")
subs <- subs %>% 
  mutate(Region = case_when(QLDRegion == "TS" | QLDRegion == "Torres" ~ "TS",
                            QLDRegion == "NEC" | QLDRegion == "NEQ" ~ "NEQ",
                            QLDRegion == "GoC" | QLDRegion == "GOC" ~ "GoC",
                            QLDRegion == "CEQ" | QLDRegion == "CQ" ~ "CQ",
                            QLDRegion == "WCY" | QLDRegion == "NPA" ~ "WCY",
                            .default = "SEQ")) # fixing fixing 

fish_effm <- table(subs$Cap_Mo, subs$Region)
print(fish_effm) # check check
fish_mor <- as.data.frame.table(fish_effm)
names(fish_mor)[1] <- "Month"
str(fish_mor) # more check

fish_mor <- fish_mor %>%
  mutate(Region = as.factor(Var2),
         Month = as.numeric(Month),
         Freq = as.numeric(Freq))

sub_longr <- fish_mor %>%
  group_by(Region) %>% 
  mutate(sub_prop = Freq / sum(Freq))

# subs----
sitsbymo <- sitsenv %>%
  mutate(Species_NB = if_else(Species_NB == 'Pristis sp.', 'Pristidae', Species_NB)) %>% 
  group_by(Region, Cap_Mo, Species_NB) %>%
  summarize(count = n(), .groups = "drop")

species_long <- sitsbymo %>%
  group_by(Region, Species_NB) %>%
  mutate(species_prop = count / sum(count)) %>%
  ungroup() 

subs_region <- species_long %>% 
  filter(!(Region %in% c('SEQ', 'TS'))) %>% 
  mutate(Region = factor(Region,
                         levels = c("WCY", "NEQ", "GoC", "CQ")))

rf_region <- sub_longr %>% 
  filter(!(Region %in% c('SEQ', 'TS'))) 

rf_region %>%
  group_by(Region) %>%
  summarise(max_prop = max(sub_prop)) #to check for scale

#everything month plot!----

ggplot() +
  geom_col(data = subs_region, 
           aes(x = Cap_Mo, y = count, fill = Species_NB),
           position = "stack") +
  facet_wrap(~ Region, nrow = 2,
             scales = "free") +
  geom_line(data = rf_region,
            aes(x = Month, y = sub_prop*max(Freq)),
            color = "black", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(3, 6, 9, 12), 
                     labels = c("Mar", "Jun", "Sep", "Dec")) +
  scale_y_continuous(labels = function(x) as.integer(x),
    sec.axis = sec_axis(~ ./max(rf_region$Freq), 
                        name = "Proportion of Annual Fishing Effort", 
                        labels = scales::percent)) +
  scale_fill_manual(values = spec_col_fam, labels = spec_labs_fam) +
  labs( x = "Month", y = "Frequency of Interactions", fill = "Species") +
  theme(
    legend.position = "bottom", plot.margin = margin(t = 2, b = 2, unit = "pt"),
    strip.text = element_text(face = "plain", hjust = 0,
                              margin = margin(t = 0, b = 5, unit = "pt")),
    panel.spacing.y = unit(0.5, "lines"),
    axis.text.x = element_text(margin = margin(t = 0, unit = "pt")),
    axis.title.x = element_text(margin = margin(t = 2, b = 2, unit = "pt"))
    )

# after all why not----
## why shouldn't I keep it----

ggsave(
  "figs/thesis_vs/v2/fig3.7/alt_spc_reg_mo.png",
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
