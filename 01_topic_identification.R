## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## create topics ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# 1. Dimensionality reduction
# 2. hdbscan


setwd(dir = "path/directory")
# Load the required packages
{
  library(proxy) # dist. matrix
  library(tidyverse) 
  library(uwot) # modern umap implementation
  library(dbscan) # clustering
}

# load data
load(file = "embeddings_sent_w5_o2_v3l.RData")


## == == == == == == == == == == == == == == == == == == == == == == == == == ==
## ## ## create clustering with umap & hdbscan ## ## ## ##

# Step 1: create UMAP embedding
umiD <- umap2(embeddings[,1:1024]
              , n_components = 32
              , metric = "cosine"
              , min_dist = 0.0001
              , seed = 137
              , n_neighbors = 15
              , spread = .6)

#plot(umiD[,16:17], pch = "·")

# hdbscan clustering
clustis <- hdbscan(umiD
                   , minPts = 3
                   , gen_hdbscan_tree = T
                   )
clustis$cluster %>% max()
sum(clustis$cluster==0)
plot(clustis)


# include clusters to embeddings
embeddings <- embeddings %>% 
  mutate(cluster = clustis$cluster, 
         probs = clustis$membership_prob)

# create cluster-centers via weighted mean of cluster-embedding-points
embeddings_cluster <- embeddings %>% 
  group_by(cluster) %>% 
  summarise(across(1:1024, ~ sum(. * probs) / sum(probs))) %>% 
  filter(cluster != 0) # remove noise points

# combine
combined_embeddings <- left_join(embeddings, embeddings_cluster, by = "cluster")
combined_embeddings <- combined_embeddings %>% na.omit()
#combined_embeddings$chapter
#combined_embeddings$cluster
#combined_embeddings[,c("chapter","cluster")] %>% unique() %>% plot(pch="*")

# save data
final_topics <- combined_embeddings %>% select(chapter, cluster, X1.y:X1024.y)
save(final_topics, file = "final_topics_w5_o2_v3l_umap32.RData")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Plots

# Heatmap plot (Figure in submission)
# 1. Prepare the Data: Count Cluster Occurrences per Chapter
heatmap_data <- final_topics %>%
  group_by(chapter, cluster) %>%
  summarize(count = log2(n())) %>%
  ungroup()

# 2. Create the Heatmap with ggplot2
ggplot(heatmap_data, aes(x = chapter, y = cluster, fill = count)) +
  geom_tile() +  # Use geom_tile for heatmap
  scale_fill_gradient(low = "darkgreen", high = "red") + # Color gradient
  theme_minimal() + # A cleaner theme
  labs(
    x = "Chapter",
    y = "Cluster",
    fill = "Log(Count)",
    title = "Cluster Distribution Across Chapters"
  ) +
  scale_x_continuous(breaks = 1:27) + 
  theme(
    axis.text.x = element_text(angle = 0), # Rotate x-axis labels if needed
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


## UMAP plot of final topics
umitops <- umap::umap(final_topics[,3:1026] %>% unique()
                      , metric = "cosine"
                      , min_dist = 0.01
                      , n_neighbors = 50
                      #, random_state = 5
)
plot(umitops$layout, pch = "·")
