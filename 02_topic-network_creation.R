## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## Topic Network Creation ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ## # ## ## # ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

library(igraph)
library(tidyverse)

setwd("your/directory")
load("final_topics_w5_o2_v3l_umap32.RData")

# create edgeweights
edgeweights <- vector()
for (i in 0:(nrow(final_topics)-1)){
  edgeweights[i] <- proxy::simil(final_topics[c(i,i+1),3:1026]
                                , method = "cosine") %>% as.numeric()
}

# Create an edgelist
edgelist <- data.frame(
  from = final_topics$cluster[-length(final_topics$cluster)],
  to = final_topics$cluster[-1]
)

# create and populate the graph
HG_network <- edgelist %>% as.matrix() %>% graph_from_edgelist(directed = F)
E(HG_network)$chapter <- final_topics$chapter[-1]
E(HG_network)$weight <- edgeweights
V(HG_network)$chapter <- aggregate(chapter ~ cluster, data = final_topics, FUN = min) %>% 
  select(chapter) %>% unlist() %>% as.numeric()


# simplify the graph
HG_network <- igraph::simplify(HG_network
                               , remove.multiple = F
                               , remove.loops = T)

# for plotting
HG_network2 <- igraph::simplify(HG_network
                                , remove.multiple = T
                                , remove.loops = T
                                , edge.attr.comb = "sum")

# saving the graph
save(HG_network, file="topicnetwork_multiple_w5_o2_v3l.RData")
#load(file="topicnetwork_multiple_w5_o2_v3l.RData")



# == == == == == == == == == == == == == == == == == == == == == == == == == == 
## descriptives

# identify stable (makro) cluster solution
grps <- vector()
for (i in 1:100){
  grps[i] <- cluster_walktrap(HG_network2, steps = i*10) %>% length
}
matplot(grps, type = "l", ylim = c(1,6))
# 2 big clusters

# diameter of the network
diameter(HG_network2)


# == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Small worldness & hierarchical measures

# check for small-worldness
set.seed(137066)
qgraph::smallworldness(HG_network2, B = 100)
# approx 3.4

# == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Hierarchical measure
# Mones E, Vicsek L, Vicsek T (2012) Hierarchy Measure for Complex Networks. PLoS ONE 7(3): e33799. 
# doi:10.1371/journal.pone.0033799

# 1. functions
# a) Function to calculate the Local Reaching Centrality for unweighted undirected graphs
local_reaching_centrality_unweighted_undirected <- function(graph) {
  N <- vcount(graph)
  C_R <- numeric(N)
  
  # Calculate shortest paths (ignoring edge weights)
  distances <- distances(graph, algorithm = "unweighted")
  
  for (i in 1:N) {
    reachable_nodes <- which(distances[i, ] > 0 & distances[i, ] < Inf)
    if (length(reachable_nodes) > 0) {
      C_R[i] <- sum(1 / distances[i, reachable_nodes]) / (N - 1)
    }
  }
  
  return(C_R)
}

# b)Function to calculate the Global Reaching Centrality for unweighted undirected graphs
global_reaching_centrality_unweighted_undirected <- function(graph) {
  C_R <- local_reaching_centrality_unweighted_undirected(graph)
  C_max_R <- max(C_R)
  GRC <- sum(C_max_R - C_R) / (vcount(graph) - 1)
  return(GRC)
}

global_reaching_centrality_unweighted_undirected(HG_network2)
local_reaching_centrality_unweighted_undirected(HG_network2)


# 2. Significance test for global measure
# a) Generate random networks with the same number of nodes and edges
num_randomizations <- 1000 
random_GRCs <- numeric(num_randomizations)

for (i in 1:num_randomizations) {
  # Generate a random graph with the same number of nodes and edges
  random_graph <- sample_gnm(302, 778, directed = F)
  
  # Calculate GRC for the random graph
  random_GRCs[i] <- global_reaching_centrality_unweighted_undirected(random_graph)
}

# b) Calculate p-value
p_value <- sum(random_GRCs >= 0.162414) / num_randomizations

# significant (p < .001)




# == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Plots for Submission

# number of new topics per chapter
V(HG_network)$chapter %>% 
  sort() %>% 
  table() %>% 
  barplot(xlab = "Chapter"
          , ylab = "Number of novel topics"
          , ylim = c(0,30)
          , main = "Number of novel topics per chapter"
          , cex.names = 0.8  # Controls the size of the bar labels
          , cex.lab = 0.8    # Controls the size of the axis labels
          , cex.main = 0.9   # Controls the size of the main title
          , cex.axis = 0.8   # Controls the size of the axis annotations
          , mgp = c(3, 0.5, 0))   


# plot of the whole topic network
layout <- layout_nicely(HG_network2)
degrees <- degree(HG_network2)
plot(HG_network2
     , layout = layout
     , vertex.size = log2(degree(HG_network2)+1)
     , vertex.label = NA
     , vertex.color = heat.colors(6)[log2(degrees + 1)]
     , edge.color = rgb(0,0,0,0.5)
     , edge.arrow.size = .1
     , edge.width = (E(HG_network2)$weight)
)

# not used in publication: narrative flow
plot(HG_network2
     , layout = layout
     , vertex.size = log2(degree(HG_network2)+1)
     , vertex.label = NA
     , vertex.color = colorRampPalette(c("red","green"))(length(degrees))
     , edge.color = rgb(0,0,0,0.5)
     , edge.arrow.size = .1
     , edge.width = (E(HG_network2)$weight)
)

# not used in publication: cluster solution
plot(cluster_walktrap(HG_network2, steps = 500), HG_network2
     , layout = layout
     , vertex.size = 2
     , vertex.label = NA
     , edge.color = rgb(0,0,0,0.2)
     , edge.arrow.size = .1
)

# not used in publication: chapter-by-chapter plots
for (i in 1:27){
  chip <- i
  png(filename = paste0("plots/chapter",chip,".png"), res = 100, type = "cairo", width = 1024, height = 1024)
  plot(HG_network
       , layout = layout
       , vertex.size=3
       , vertex.color=ifelse(V(HG_network)$chapter > chip, NA, heat.colors(6)[log2(degrees+1)])
       , vertex.frame.color=NA
       , vertex.label=NA
       , edge.arrow.size = .5
       , edge.color=ifelse(E(HG_network)$chapter > chip, NA, rgb(0,0,0.5,0.3)))
  dev.off()
}
