# Details ----
#' 04_pca_0_pre.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2 Oct 2024
#' - modified 2025-12-22
#' > there were multiple pca runs to look at vars
#' >> clarified what each was used for ! (v necessary)
#' Content: 
#' + initial pca , pre-model build 
#' ++ checking for both co-linearity and redundancy
#' -----------

source('helpers/help_stats.R')
source('helpers/help_plot.R')
theme_set(mytheme)

maxenv <- readxl::read_xlsx("data/processed/sits_envmod.xlsx") # anthro factors

last_env <- maxenv %>%
  dplyr::select(where(is.numeric)) 

str(last_env) # haha

#corr----
#which of these corr vars are best?
ggcorr(last_env, label = TRUE) #yikes yup that's everything

library(Hmisc)
var_clust <- varclus(~ DB_area + SB_area + vol_yearavg + 
                       RivLg + STRM_ORDER + 
                       LocType + DTO_IO +
                       DTO + DTM +
                       Salt + MG + Flat + 
                       Cap_Year + oni + #colin
                       Precipitation + wetdry + Cap_Mo +
                       relannflow + relmoflow +
                       Tmax + Temp_wat, data = maxenv)
plot(var_clust) #goh that is interesting af

#test----
test_env <- maxenv %>% 
  dplyr::select(Species_NB, 
                DTO_IO, #LocType, 
                STRM_ORDER, 
                #RivLg,
                DB_area, #SB_area, 
                #vol_yearavg, 
                #Salt, 
                Flat, MG, 
                #Cap_Year, 
                oni, 
                #wetdry, 
                Cap_Mo, 
                Precipitation, 
                Tmax #Temp_wat,
                #relannflow, relmoflow, #cuts down number in PCA
                #GPS_long, GPS_lat
  ) 

logicols = c('Flat', "MG")
test_env[logicols] <- lapply(test_env[logicols], as.numeric)
na_specs <- colSums(is.na(test_env)) #okay better

#dont need normal data nec

#Combo PCA----
PCA <- test_env %>%
  dplyr::select(where(is.numeric))%>%
  drop_na() %>%
  as.matrix() 

spec <- test_env %>%
  drop_na() %>% 
  as.matrix() 

pca <- prcomp(PCA, scale. = TRUE)
summary(pca)

##Graph----
biplot(pca)
correlation <- cor(PCA, pca$x)
arrow_data <- data.frame(
  x = 0,
  y = 0,
  xend = correlation[1:3, 1] * max(pca$x[,1]),
  yend = correlation[1:3, 2] * max(pca$x[,2]),
  color = c("black", "orange", "grey")
)

#lets color it by diff vars
BiggerPCA <- ggplot(data = data.frame(pca$x, spec),
                 aes(x = PC1, y = PC2, color = Species_NB)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point() +
  stat_ellipse(data = subset(data.frame(pca$x, spec), 
                             !Species_NB %in% c("Pristidae", "Pristis sp.")),
               aes(x = PC1, y = PC2, color = Species_NB), 
               type = "norm", 
               level = 0.95) +
  scale_color_manual(values = spec_col_ks,
                     labels = spec_labs_ks) +
  labs(x = "PC1", y = "PC2", color = "Species")

pcaplot <- BiggerPCA +
  geom_segment(data = arrow_data,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "inches")),
               color = arrow_data$color,
               inherit.aes = FALSE) +
  theme(legend.position = "none")

saveRDS(pcaplot, 'data/rds/pcaplot.RDS')

#Vars----
plot(pca, type = "l", main = "Scree Plot")
library(factoextra)
fviz_eig(pca)
eigenvals(pca) 

var_explained <- pca$sdev^2 / sum(pca$sdev^2)
cumulative_var <- cumsum(var_explained)
n_components <- which(cumulative_var >= 0.8)[1]
#who are they!
fviz_contrib(pca, choice = "var", axes = 1) # sso, distance, prec, dbsize
fviz_contrib(pca, choice = "var", axes = 2) # month, prec, tmax (mmmm co-lin)
fviz_contrib(pca, choice = "var", axes = 3) # year? nah, tmax, oni
fviz_contrib(pca, choice = "var", axes = 4) 
fviz_contrib(pca, choice = "var", axes = 5) 

ggcorr(test_env, label = TRUE) # MUCH betta

# distance to outlet
# precipitation
# stream order
# db size (flow)
# season

# DTO > loctype > DTM 
# DB_area > RivLg 
# may be sep from SSO
# volyravg ~ DB_area (more data)
# temp water > tmax (but more data)
# wetdry > Cap_Mo (but prec can be sep)
# Flats really just measure of prox to mouth

