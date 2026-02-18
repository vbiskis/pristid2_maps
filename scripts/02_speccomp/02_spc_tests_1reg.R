# Details ----
#' 02_spc_tests_1reg.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 12 Dec 2025
#' Content: 
#' + nmds
#' Output:
#' supps fig
#' -----------

source("helpers/help_stats.R")
sits_cln <- readxl::read_xlsx("data/processed/sits_cln.xlsx") 

source("helpers/help_plot.R")
set_theme(mytheme)

# Bray-Curtis with time bins----
community_data <- sits_cln %>%
  group_by(Region, ColY2, Species_NB) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Species_NB, 
              values_from = count, 
              values_fill = 0)

metadata <- community_data %>%
  dplyr::select(Region, ColY2)

community_matrix <- community_data %>%
  dplyr::select(-Region, -ColY2) %>%
  as.matrix()

rownames(community_matrix) <- paste(metadata$Region, 
                                    metadata$ColY2, 
                                    sep = "_")

bc_dist <- vegdist(community_matrix, method = "bray")
bc_clust <- hclust(bc_dist, method = "average")
plot(bc_clust) #its the GULF

# PERMANOVA----
permanova_result <- adonis2(community_matrix ~ Region + ColY2,
                            data = community_data,
                            method = "bray",
                            permutations = 999)
print(permanova_result)

## Sequential----
permanova_seq <- adonis2(community_matrix ~ Region + ColY2,
                         data = community_data,
                         method = "bray",
                         permutations = 999,
                         by = "terms")  # Test each term
print(permanova_seq)

# nmds----
nmds <- metaMDS(community_matrix, distance = "bray", k = 2)

nmds_scores <- as.data.frame(nmds$points)  
nmds_scores$Region <- metadata$Region
nmds_scores$ColY2 <- metadata$ColY2
names(nmds_scores)  # quick check 

# Plot
ggplot(nmds_scores, aes(x = MDS1, y = MDS2,   #ah! there we go
                        color = Region, shape = ColY2)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Region), level = 0.95) +
  labs( #subtitle = paste("Stress =", round(nmds$stress, 3)),
       color = "Region", 
       shape = "Year Bin")

ggsave(
  "figs/supps/s3/s3_nmds.png",
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

### side plot with gear!----
meta_gear <- sits_cln %>%
  group_by(Region, ColY2) %>%
  summarise( # get gear type in from metadata
    dominant_gear = names(sort(table(Gear2), decreasing = TRUE))[1],
    .groups = "drop"
  )

# Same as above...
site_scores <- scores(nmds, display = "sites")
nmds_scores <- as.data.frame(site_scores)
nmds_scores$Region <- metadata$Region
nmds_scores$ColY2 <- metadata$ColY2 

# Add gear
nmds_scores <- nmds_scores %>%
  left_join(meta_gear, by = c("Region", "ColY2"))

nmd <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        color = Region,
                        shape = dominant_gear,
                        size = ColY2)) +  # Use factor directly
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(group = Region), level = 0.95, linewidth = 0.5) +
  labs(subtitle = paste("Stress =", round(nmds$stress, 3)),
       size = "Time Period",
       shape = "Gear")  #eh... it makes it more visible but really doesn't help



