# Details ----
#' 03_lhs_dendro.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - New new 2025-12-13
#' Content: 
#' + looking for clustering according to lhs
#' -----------

source('helpers/help_plot.R')
source('helpers/help_stats.R')
theme_set(mytheme)

library(cluster) # test
library(dendextend) # visualise
library(indicspecies)

sitsdemo <- readxl::read_xlsx("data/processed/sitsdemo.xlsx") 
# all spec (c), less vars

# base dendro----
lhs_tbl <- table(sitsdemo$Dbasin, sitsdemo$LHS)
lhs_tbl <- lhs_tbl[rowSums(lhs_tbl) > 0, ] 

lhs_sim <- vegdist(lhs_tbl, method = "bray")
lhs_clust <- hclust(lhs_sim, method = "average")

# Plot
plot(lhs_clust, cex = 0.5)

# could be as low as 4 and high as 7
# but lets check

dendtbl <- sitsdend %>% # bc PERMANOVA needs this instead...
  group_by(Dbasin, LHS) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = LHS, 
              values_from = count, 
              values_fill = 0)

lhs_mat <- dendtbl %>%
  dplyr::select(-Dbasin) %>%
  as.matrix()

lhs_meta <- dendtbl %>% # fine we'll take the names out
  dplyr::select(Dbasin)

rownames(lhs_mat) <- paste(lhs_meta$Dbasin) 

# Test significance----

k_values <- 2:10
sil_widths <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  k <- k_values[i]
  clusters <- cutree(lhs_clust, k = k)
  sil <- silhouette(clusters, lhs_sim)
  sil_widths[i] <- mean(sil[, 3])
}

# Plot the results
plot(k_values, sil_widths, type = "b", 
     xlab = "Number of clusters (k)", 
     ylab = "Average Silhouette Width",
     main = "Optimal number of clusters") #ah suggests 3 is better than 4!

## 3----
k3clust <- cutree(lhs_clust, k=3) # first peak
anosim_k3 <- anosim(lhs_sim, k3clust)
permanova_k3 <- adonis2(lhs_mat ~ k3clust, method = "bray")  

# 8----
k8clust <- cutree(lhs_clust, k=8) # and best
anosim_k8 <- anosim(lhs_sim, k8clust) # mmmmm
permanova_k8 <- adonis2(lhs_mat ~ k8clust, method = "bray") 

# lets add some flair! for meaning----
## get species as colour----
dend <- as.dendrogram(lhs_clust)
dend_labels <- labels(dend)

# for each basin, pull the dominant species
basin_species <- sitsdend %>%
  group_by(Dbasin) %>%
  summarise(
    dom_species = names(which.max(table(Species_NB))),
    .groups = "drop"
  )

# now map them
basin_color_map <- basin_species %>%
  mutate(Color = spec_col_fam[dom_species]) # from plot helper - predefined colours :)

basin_colors <- basin_color_map$Color[ # and how good is that, matching with a match function
  match(dend_labels, basin_color_map$Dbasin)
]

dend <- set(dend, "labels_col", basin_colors) # now set
plot(dend, xlab = "Drainage Basin", cex = 0.5) # aha!

rect.hclust(lhs_clust, k = 3, border = "grey")
# just picking out those ones with v little known 
rect.hclust(lhs_clust, k = 4, border = "red") # YOY:adult group sep from full adult
rect.hclust(lhs_clust, k = 8, border = "blue") # not til here do we sep the juv to the YOY cluster

# which LHS cause this separation?----

# Cut dendrogram into clusters
k <- 4
cut_lhs <- cutree(lhs_clust, k = k)

# Indicator analysis - which life stages define each cluster?
indval <- multipatt(lhs_tbl, cut_lhs, control = how(nperm = 999))
summary(indval) # after inspecting rivers - it's 3!

## add characteristics to plot----

# get proportion of each life stage per cluster
cluster_lhs_summary <- sitsdend %>%
  filter(Dbasin %in% names(cut_lhs)) %>%  # Only basins in the dendrogram
  mutate(Cluster = cut_lhs[match(Dbasin, names(cut_lhs))]) %>%
  group_by(Cluster, LHS) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Cluster) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(Cluster, desc(prop))

# and dominant life stage per cluster
cluster_labels <- cluster_lhs_summary %>%
  filter(!is.na(LHS)) %>%  # Remove NA from labels 
  group_by(Cluster) %>%
  slice_max(prop, n = 2) %>%  # Get top 2 life stages
  summarise(
    Label = sprintf("Cluster %s:\n%s (%.0f%%),\n%s (%.0f%%)", 
                    LETTERS[Cluster[1]], 
                    LHS[1], prop[1] * 100,
                    LHS[2], prop[2] * 100),
    .groups = "drop"
  )

### lhs structure----
cluster_labels <- cluster_labels %>% 
  mutate(
    Label = if_else(Cluster == 3, "Cluster C:\adult (100%)", Label) 
  ) #manually remove that silly 0% NA

cluster_centers <- tapply(1:length(labels(dend)), cut_lhs, mean)

cluster_x_positions <- list() # label placement

for(i in 1:k) {
  cluster_basins <- names(cut_lhs[cut_lhs == i]) # get basin
  basin_positions <- which(labels(dend) %in% cluster_basins) # and position
  cluster_x_positions[[i]] <- mean(basin_positions) # and start
}

### and size for lhs----

basin_props <- sitslhs %>%
  filter(Dbasin %in% names(cut_lhs), 
         !is.na(LHS)) %>%
  group_by(Dbasin) %>%
  summarise(prop_juv = mean(LHS %in% c("YOY", "juvenile")), 
            has_yoy = any(LHS == "YOY"),
            .groups = "drop")

basinprops_ord <- basin_props[match(labels(dend), basin_props$Dbasin), ]

# no juv = 2, smaller the more juvs there are
line_widths <- 1.5 - (basinprops_ord$prop_juv)

# dashed (2) if has YOY, solid (1) if not
line_types <- ifelse(basinprops_ord$has_yoy, 2, 1)

names(line_widths) <- basinprops_ord$Dbasin
names(line_types) <- basinprops_ord$Dbasin

dend <- dendrapply(dend, function(n) { #bc the leafs are being annoying!
  if(is.leaf(n)) {
    leaf_label <- attr(n, "label")
    idx <- which(basin_props$Dbasin == leaf_label)
    
    if(length(idx) > 0) {
      attr(n, "edgePar") <- list(
        lwd = 1.5 - basin_props$prop_juv[idx],
        lty = ifelse(basin_props$has_yoy[idx], 2, 1)
      )
    }
  }
  return(n)
})

# new print----
png(
  filename = "figs/thesis_vs/v2/fig3.8/alt_dendro_lhs.png",
  width = 8.5,
  height = 6,
  units = "in",
  res = 300  # Resolution (dpi)
)

# and plot----
par(family = "optima",
    mar = c(6, 4, 2, 0),
    mgp = c(4, 1, 0))
plot(dend, yaxt = "n", xlab = "Drainage Basin", cex = 0.5) # aha!
axis(2, pos = -1.5)

# add legend
legend("topright", 
       legend = spec_labs_fam,
       fill = spec_col_fam,
       title = " Dominant Species ", 
       cex = 0.9,
       pt.cex = 2, 
       box.lwd = 1,        
       inset = 0.03,
       x.intersp = 1.25,
       y.intersp = 1.25)

manual_heights <- c(0.75, 0.535, 0.67, 0.535)

# All labels at manually set height (I cant deal anymore ugh)
for(i in 1:k) {
  cluster_desc <- cluster_labels$Label[cluster_labels$Cluster == i]
  
  text_width <- strwidth(cluster_desc, cex = 0.7)
  text_height <- strheight(cluster_desc, cex = 0.7)
  
  # Draw white rectangle
  rect(xleft = cluster_x_positions[[i]] - text_width/2 - 0.45,
       xright = cluster_x_positions[[i]] + text_width/2 + 0.45,
       ybottom = manual_heights[i] - text_height/2 - 0.025,
       ytop = manual_heights[i] + text_height/2 + 0.025,
       col = "white", 
       border = NA)
  
  # top line (bc im nuts)
  segments(x0 = cluster_x_positions[[i]] - text_width/2 - 0.45,
           x1 = cluster_x_positions[[i]] + text_width/2 + 0.45,
           y0 = manual_heights[i] + text_height/2 + 0.025,
           y1 = manual_heights[i] + text_height/2 + 0.025,
           lwd = 0.75)
  
  # bottom line
  segments(x0 = cluster_x_positions[[i]] - text_width/2 - 0.45,
           x1 = cluster_x_positions[[i]] + text_width/2 + 0.45,
           y0 = manual_heights[i] - text_height/2 - 0.025,
           y1 = manual_heights[i] - text_height/2 - 0.025,
           lwd = 0.75)
  
  text(cluster_x_positions[[i]], 
       manual_heights[i],  # Your manual heights
       cluster_desc, 
       cex = 0.8, 
       font = 1)
}

dev.off()

#for reporting

# are clusters closer to boat ramps?

sitsspl <- readxl::read_xlsx("data/processed/sitsspl.xlsx") 
cluster_stat <- sitsspl[sitsspl$Dbasin %in% names(cut_lhs), ]
cluster_stat$Cluster <- cut_lhs[match(cluster_stat$Dbasin, names(cut_lhs))]

# ANOVA: Does stream order differ among clusters?
summary(aov(STRM_ORDER ~ factor(Cluster), data = cluster_stat))
TukeyHSD(aov(STRM_ORDER ~ factor(Cluster), data = cluster_stat))
#for sure, theres an enviro thing to it

sitsmods <- readxl::read_xlsx("data/processed/sitsmods.xlsx") 

cluster_stat <- cluster_stat %>% 
  left_join(sitsmods %>% 
              select(SUB, logRF, dist_Cl_RF, logpop))

ggqqplot(cluster_stat$logbramps) #ehhhhhh
cluster_stat$logbramps <- log(cluster_stat$dist_Cl_RF)

# real question -> dist to boat ramps
summary(aov(logRF ~ factor(Cluster), data = cluster_stat)) #nope!
kruskal.test(log(dist_Cl_RF) ~ factor(Cluster), data = cluster_stat)
#TukeyHSD(kruskal.test(log(dist_Cl_RF) ~ factor(Cluster), data = cluster_stat))
