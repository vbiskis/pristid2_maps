# Details ----
#' 03_lhs_dendro.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 
#' - New new 2025-12-13
#' Content: 
#' + v2: sep by spec AND demos!
#' -----------

source('helpers/help_plot.R')
source('helpers/help_stats.R')
theme_set(mytheme)

library(cluster) # test
library(dendextend) # visualise
library(indicspecies)

sitsdemo <- readxl::read_xlsx("data/processed/sitsdemo.xlsx") 

sitsdend <- sitsdemo %>%
  filter(!(Species_NB %in% c('Pristidae', 'Pristis sp.')))

#let's make it----
spcdem <- sitsdend %>%
  group_by(Dbasin, Species_NB, LHS) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = LHS, 
              values_from = count, 
              values_fill = 0)

met2 <- spcdem %>%
  dplyr::select(Dbasin, Species_NB)

spcdem_matrix <- spcdem %>%
  dplyr::select(-Dbasin, -Species_NB) %>%
  as.matrix()

rownames(spcdem_matrix) <- paste(met2$Dbasin, met2$Species_NB,  sep = " : ")

lhs_sim2 <- vegdist(spcdem_matrix, method = "bray")
lhs_clust2 <- hclust(lhs_sim2, method = "average")

# Plot
plot(lhs_clust2, cex = 0.5)

k_values <- 2:12
sil_widths <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  k <- k_values[i]
  clusters <- cutree(lhs_clust2, k = k)
  sil <- silhouette(clusters, lhs_sim2)
  sil_widths[i] <- mean(sil[, 3])
}

# Plot the results
plot(k_values, sil_widths, type = "b", 
     xlab = "Number of clusters (k)", 
     ylab = "Average Silhouette Width",
     main = "Optimal number of clusters")

plot(lhs_clust2, cex = 0.5)

rect.hclust(lhs_clust2, k = 4, border = "red") 
rect.hclust(lhs_clust2, k = 11, border = "blue") 

k <- 4
cut_lhs <- cutree(lhs_clust2, k = k)

anosim <- anosim(spcdem_matrix, cut_lhs) # mmmmm
permanova <- adonis2(spcdem_matrix ~ cut_lhs, method = "bray") 

# Indicator analysis - which life stages define each cluster?
indval <- multipatt(spcdem_matrix, cut_lhs, control = how(nperm = 999))
summary(indval)

cluster_data <- data.frame(
  basin_species = names(cut_lhs),
  Cluster = cut_lhs
) %>%
  separate(basin_species, 
           into = c("Dbasin", "Species_NB"), sep = " : ", remove = FALSE)

# now map them
cluster_lhs_summary <- sitsdend %>%
  mutate(basin_species = paste(Dbasin, Species_NB, sep = " : ")) %>%
  filter(basin_species %in% names(cut_lhs)) %>%
  left_join(cluster_data, by = "basin_species") %>%
  group_by(Cluster, LHS) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Cluster) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(Cluster, desc(prop))

# Dominant life stage per cluster
cluster_labels <- cluster_lhs_summary %>%
  filter(!is.na(LHS)) %>%
  group_by(Cluster) %>%
  slice_max(prop, n = 2) %>%
  summarise(
    Label = sprintf("Cluster %s:\n%s (%.0f%%),\n%s (%.0f%%)",
                    LETTERS[Cluster[1]],
                    LHS[1], prop[1] * 100,
                    LHS[2], prop[2] * 100),
    .groups = "drop"
  )

# Color by species
dend <- as.dendrogram(lhs_clust2)
dend_labels <- labels(dend)

label_info <- data.frame(
  full_label = dend_labels,
  Dbasin = sapply(strsplit(dend_labels, " : "), function(x) x[1]),
  Species_NB = sapply(strsplit(dend_labels, " : "), function(x) x[2])
)

# Color by species
species_colors <- spec_col_fam[label_info$Species_NB]
dend <- set(dend, "labels_col", species_colors)

# Change labels to just show basin names
dend <- set(dend, "labels", label_info$Dbasin)

# Plot
plot(dend, xlab = "Drainage Basin", cex = 0.5)
rect.hclust(lhs_clust2, k = 4, border = "grey")

# Cluster positions for labels
cluster_x_positions <- list()
for(i in 1:k) {
  cluster_members <- names(cut_lhs[cut_lhs == i]) # get basin:species combos
  member_positions <- which(dend_labels %in% cluster_members) # positions in dendrogram
  cluster_x_positions[[i]] <- mean(member_positions)
}

### Line widths and types based on basin-species LHS proportions ----

# for better vis!
dend <- as.dendrogram(lhs_clust2)
dend_labels_og <- labels(dend) # need the original thing

basin_species_props <- sitsdend %>%
  mutate(basin_species = paste(Dbasin, Species_NB, sep = " : ")) %>%
  filter(basin_species %in% dend_labels_og,  
         !is.na(LHS)) %>%
  group_by(basin_species) %>%
  summarise(prop_juv = mean(LHS %in% c("YOY", "juvenile")), 
            has_yoy = any(LHS == "YOY"),
            .groups = "drop")

# Need to match basin_species in original data to basin labels in dend
basin_species_props %>% filter(has_yoy) %>% pull(basin_species)

# Order to match dendrogram (still using original labels)
basinspecies_props_ord <- basin_species_props[match(dend_labels_og, 
                                                    basin_species_props$basin_species), ]

# Line widths: thicker = fewer juveniles
line_widths <- 1.5 - (basinspecies_props_ord$prop_juv)
# Line types: dashed if has YOY, solid if not
line_types <- ifelse(basinspecies_props_ord$has_yoy, 2, 1)

# Apply to dendrogram leaves
dend_with_edges <- dendrapply(dend, function(n) {
  if(is.leaf(n)) {
    leaf_label <- attr(n, "label")  # Still "Basin : Species"
    match_row <- basin_species_props[basin_species_props$basin_species == leaf_label, ]
    
    if(nrow(match_row) > 0) {
      attr(n, "edgePar") <- list(
        lwd = 1.5 - match_row$prop_juv[1],
        lty = ifelse(match_row$has_yoy[1], 2, 1)
      )
    }
  }
  return(n)
})

label_info <- data.frame(
  full_label = dend_labels_og,
  Dbasin = sapply(strsplit(dend_labels_og, " : "), function(x) x[1]),
  Species_NB = sapply(strsplit(dend_labels_og, " : "), function(x) x[2])
)

# Color by species
species_colors <- spec_col_fam[label_info$Species_NB]
dend_final <- set(dend_with_edges, "labels_col", species_colors)

# Change labels to just basin names (LAST step)
dend_final <- set(dend_final, "labels", label_info$Dbasin)

# new print----
png(
  filename = "figs/fig7/fig7_dendro_speclhs.png",
  width = 8.5,
  height = 6.75,
  units = "in",
  res = 300  # Resolution (dpi)
)

# and plot----
par(family = "optima",
    mar = c(5, 2.5, 0, 0),
    mgp = c(4, 1, 0))
plot(dend_final, yaxt = "n", xlab = "Drainage Basin : Species", cex = 0.5)
axis(2, pos = -1.5)

# add legend
legend("topright",
       legend = spec_labs_ks,
       fill = spec_col_ks,
       title = " Species ",
       cex = 0.9,
       pt.cex = 2,
       box.lwd = 1,
       x.intersp = 1,
       y.intersp = 1)

manual_heights <- c(0.535, 0.56, 0.6, 0.63)

# All labels at manually set height (I cant deal anymore ugh)
for(i in 1:k) {
  cluster_desc <- cluster_labels$Label[cluster_labels$Cluster == i]
  
  text_width <- strwidth(cluster_desc, cex = 0.7)
  text_height <- strheight(cluster_desc, cex = 0.7)
  
  # White rectangle background
  rect(xleft = cluster_x_positions[[i]] - text_width/2 - 0.45,
       xright = cluster_x_positions[[i]] + text_width/2 + 0.45,
       ybottom = manual_heights[i] - text_height/2 - 0.025,
       ytop = manual_heights[i] + text_height/2 + 0.025,
       col = "white", 
       border = NA)
  
  # Top line
  segments(x0 = cluster_x_positions[[i]] - text_width/2 - 0.45,
           x1 = cluster_x_positions[[i]] + text_width/2 + 0.45,
           y0 = manual_heights[i] + text_height/2 + 0.025,
           y1 = manual_heights[i] + text_height/2 + 0.025,
           lwd = 0.75)
  
  # Bottom line
  segments(x0 = cluster_x_positions[[i]] - text_width/2 - 0.45,
           x1 = cluster_x_positions[[i]] + text_width/2 + 0.45,
           y0 = manual_heights[i] - text_height/2 - 0.025,
           y1 = manual_heights[i] - text_height/2 - 0.025,
           lwd = 0.75)
  
  # Cluster label text
  text(cluster_x_positions[[i]], 
       manual_heights[i],
       cluster_desc, 
       cex = 0.8, 
       font = 1)
}

dev.off()

#for reporting

# are clusters closer to boat ramps?

sitsspl <- readxl::read_xlsx("data/processed/sitsspl.xlsx") 
sitsmods <- readxl::read_xlsx("data/processed/sitsmods.xlsx") 

cluster_assignments <- data.frame(
  basin_species = names(cut_lhs),
  Cluster = cut_lhs
) %>%
  mutate(
    Dbasin = sapply(strsplit(basin_species, " : "), function(x) x[1]),
    Species_NB = sapply(strsplit(basin_species, " : "), function(x) x[2])
  )

# Get unique basins with their clusters
cluster_stat <- sitsspl %>%
  filter(Dbasin %in% cluster_assignments$Dbasin) %>%
  left_join(cluster_assignments %>% select(Dbasin, Species_NB, Cluster), 
            by = c("Dbasin", "Species_NB")) %>%
  left_join(sitsmods %>% select(SUB, dist_Cl_RF, logRF, logpop, logaDNm), by = "SUB") %>% 
  filter(!is.na(Cluster))

ggqqplot(cluster_stat$dist_Cl_RF)
cluster_stat$logbramps <- log(cluster_stat$dist_Cl_RF)
ggqqplot(cluster_stat$logbramps) #ehhhhhh
ggqqplot(cluster_stat$logRF) 

kruskal.test(logbramps ~ factor(Cluster), data = cluster_stat%>% 
               filter(Species_NB != "P. pristis")) #not without P. pristis!

kruskal.test(logRF ~ factor(Cluster), data = cluster_stat) # ahhh yes tho

kruskal.test(logaDNm ~ factor(Cluster), data = cluster_stat) #and yup

kruskal.test(logpop ~ factor(Cluster), data = cluster_stat) #ahhh yes tho

cluster_stat %>% 
  #filter(Species_NB != "P. pristis") %>% 
  group_by(Cluster) %>% 
  summarise(
    distBR = mean(dist_Cl_RF, na.rm = TRUE),
    logRF = mean(logRF, na.rm = TRUE),
    logaDNm = mean(logaDNm, na.rm = TRUE),
    logpop = mean(logpop, na.rm = TRUE)
  )
