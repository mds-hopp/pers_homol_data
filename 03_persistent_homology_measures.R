## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## persistent homology measures ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## betti numbers
## Wasserstein and bottleneck distances
## topics &  novel topics

library(TDAstats)
library(TDA)
library(tidyverse)
library(igraph)

setwd("your/directory")
load(file = "topicnetwork_multiple_w5_o2_v3l.RData")


# create pers. homol. for each chapter
chap <- 27
p.hom_cluster <- list()
for (i in 1:chap){
  print(i)
  current_graph <- subgraph_from_edges(HG_network, E(HG_network)[chapter <= i], delete.vertices = T)

  current_dist <- distances(current_graph) %>% as.matrix()
  p.hom_cluster[[i]] <- calculate_homology(current_dist, dim = 2, threshold = max(current_dist), format = "distmat")
}
# takes approx. 42 seconds on an AMD Ryzen 7 7840U (16)

# save dataset
save(p.hom_cluster, file="p.hom_cluster_w5_o2_v3l.RData")
#load(file="p.hom_cluster_w5_o2_v3l.RData")

## create betti numbers
# betti numbers
chap <- 27
betti_numbers_cluster <- data.frame("b0" = 0,"b1" = 0,"b2" = 0)
for (i in 1:chap){
  betti <- table(p.hom_cluster[[i]][,"dimension"]) %>% as.numeric()
  betti_numbers_cluster[i,1:length(betti)] <- betti
}
betti_numbers_cluster[is.na(betti_numbers_cluster)] <- 0

# Detrending
# Add row numbers as a predictor
row_numbers <- 1:nrow(betti_numbers_cluster)
# Detrend each column
detrended_betti <- data.frame(row_numbers = row_numbers)
for (col_name in names(betti_numbers_cluster)) {
  model <- lm(betti_numbers_cluster[[col_name]] ~ row_numbers)
  fitted_values <- fitted(model)
  detrended_values <- betti_numbers_cluster[[col_name]] - fitted_values
  detrended_betti[[col_name]] <- detrended_values
}

detrended_betti <- detrended_betti[,-1]
names(detrended_betti) <- c("b0_detr","b1_detr","b2_detr")

# save betti numbers
save(betti_numbers_cluster, detrended_betti, file = "betti_nmbrs_v3l.RData")
#load(file = "betti_nmbrs_v3l.RData")


# == == == == == == == == == == == == == == == == == == == == == == == == == ==
## persistence distance measures
homol_dist_clust <- data.frame("bottle_dim0"=0, "bottle_dim1"=0, "bottle_dim2"=0,
                               "wasser_dim0"=0, "wasser_dim1"=0, "wasser_dim2"=0)

for (i in 1:26){ # distances are comutative
  homol_dist_clust[i+1,] <- c(bottleneck(p.hom_cluster[[i]], p.hom_cluster[[i+1]], dimension = 0),
                              bottleneck(p.hom_cluster[[i]], p.hom_cluster[[i+1]], dimension = 1),
                              bottleneck(p.hom_cluster[[i]], p.hom_cluster[[i+1]], dimension = 2),
                              wasserstein(p.hom_cluster[[i]], p.hom_cluster[[i+1]], dimension = 0),
                              wasserstein(p.hom_cluster[[i]], p.hom_cluster[[i+1]], dimension = 1),
                              wasserstein(p.hom_cluster[[i]], p.hom_cluster[[i+1]], dimension = 2)
  )
  print(i)
}

# i = 1
homol_dist_clust[1,] <- c(bottleneck(c(), p.hom_cluster[[1]], dimension = 0),
                          bottleneck(c(), p.hom_cluster[[1]], dimension = 1),
                          bottleneck(c(), p.hom_cluster[[1]], dimension = 2),
                          wasserstein(c(), p.hom_cluster[[1]], p = 1, dimension = 0),
                          wasserstein(c(), p.hom_cluster[[1]], p = 1, dimension = 1),
                          wasserstein(c(), p.hom_cluster[[1]], p = 1, dimension = 2)
)


# create a detrended version
# Add row numbers as a predictor
row_numbers <- 1:27
# Detrend each column
detrended_dists <- data.frame(row_numbers = row_numbers)
for (col_name in names(homol_dist_clust)) {
  model <- lm(homol_dist_clust[[col_name]] ~ row_numbers)
  fitted_values <- fitted(model)
  detrended_values <- homol_dist_clust[[col_name]] - fitted_values
  detrended_dists[[col_name]] <- detrended_values
}
detrended_dists <- detrended_dists[,-1] # remove linear
#matplot(detrended_dists, type = "l")

# save data
save(homol_dist_clust, detrended_dists, file = "homol_dist_clust_w5_o2_v3l.RData")
#load(file = "homol_dist_clust_w5_o2_v3l.RData")


## == == == == == == == == == == == == == == == == == == == == == == == == == ==
# number of topics and number of new topics
load("final_topics_w5_o2_v3l_umap32.RData")
load("topicnetwork_multiple_w5_o2_v3l.RData")

novel_topics <- V(HG_network)$chapter %>% 
  sort() %>% 
  table()

topics_number <- final_topics %>%
  dplyr::select(chapter, cluster) %>%
  arrange(chapter) %>%
  group_by(chapter) %>%
  summarise(unique_topics = n_distinct(cluster)) %>%
  mutate(cumulative_topics = cumsum(unique_topics))

# add to DF
topics_number[, "novel_topics"] <- novel_topics %>% as.numeric()

# remove first column
topics_number <- topics_number[,-1]
matplot(topics_number[,-2], type = "l")

# save data
save(topics_number, file="topics_v3l.RData")





## ## ## ## ## ## #
## PLOTS

# Pers. Homol. Diagr. Plot. (plot 27 used in publication)
for (i in 1:27){
  png(filename = paste0("plots/pers.homol.",i,".png"), width = 1000, height = 1000, res = 200)
  plot_persist(p.hom_cluster[[i]], flat = F) %>% print()
  dev.off()
}

# Plot of detrended betti nmbrs
matplot(detrended_betti, type = "l")

## Not used in publication: Betti numbers plot
# Convert to long format for ggplot
betti_long <- betti_numbers_cluster %>% 
  #mutate(across(c("b0", "b1", "b2"), ~ log(abs(.) + 1))) %>% 
  mutate(X = 1:nrow(betti_numbers_cluster)) %>% 
  pivot_longer(cols = c("b0", "b1", "b2"), 
               names_to = "Betti_Number", 
               values_to = "Value")
# betti numbers
ggplot(betti_long, aes(x = X, y = Value, color = Betti_Number)) +
  geom_line() +
  labs(x = "Chapter", y = "Value", title = "Betti Numbers") +
  theme_classic() + 
  scale_color_manual(values = c("b0" = "tomato", "b1" = "green3", "b2" = "steelblue")) + 
  scale_x_continuous(breaks = betti_long$X)


# not used in publication: plot of wasserstein & bottleneck distances
# Convert to long format for ggplot
homol_bottle_clust_long <- homol_dist_clust %>% 
  mutate(X = 1:nrow(homol_dist_clust)) %>% 
  pivot_longer(cols = c("bottle_dim0", "bottle_dim1", "bottle_dim2"), 
               names_to = "Bottle_Number", 
               values_to = "Value")
homol_wasser_clust_long <- homol_dist_clust %>% 
  mutate(X = 1:nrow(homol_dist_clust)) %>% 
  pivot_longer(cols = c("wasser_dim0", "wasser_dim1", "wasser_dim2"), 
               names_to = "Wasser_Number", 
               values_to = "Value")

# Create the ggplot
ggplot(homol_bottle_clust_long, aes(x = X, y = Value, color = Bottle_Number)) +
  geom_line() +
  labs(x = "Chapter", y = "Value", title = "Bottleneck Distances", color = "Bottleneck Distance") +
  theme_classic() + 
  scale_color_manual(values = c("bottle_dim0" = "tomato", "bottle_dim1" = "green3", "bottle_dim2" = "steelblue"), 
                     labels = c("Dimension 0", "Dimension 1", "Dimension 2")) +
  scale_x_continuous(breaks = 1:27) +
  coord_trans(y = "log10")

# Create the ggplot
ggplot(homol_wasser_clust_long, aes(x = X, y = Value, color = Wasser_Number)) +
  geom_line() +
  labs(x = "Chapter", y = "Value", title = "Wasserstein Distances", color = "Wasserstein Distance") + # Changed this line
  theme_classic() +
  scale_color_manual(values = c("wasser_dim0" = "tomato", "wasser_dim1" = "green3", "wasser_dim2" = "steelblue"),
                     labels = c("Dimension 0", "Dimension 1", "Dimension 2")) +
  scale_x_continuous(breaks = 1:27) +
  coord_trans(y = "log10")