library(tidyverse)

# combine datasets
setwd("set/your/wd")

# betti
load("betti_nmbrs_v3l.RData")

# wasserstein $ bottle
load("homol_dist_clust_w5_o2_v3l.RData")

# load pers. homology
load("p.hom_cluster_w5_o2_v3l.RData")

# topics
load("topics_v3l.RData")

# the user dataset
load("user_engagement_data.RData")


# average ratings per chapter
chapter_descriptives <- data_wide %>%
  group_by(chapter) %>%
  dplyr::summarise(across(boring:exciting, 
                          list(
                            mean = ~mean(., na.rm = T),
                            sd = ~sd(., na.rm = T)
                          ),
                          .names = "{.col}_{.fn}")) %>%
  arrange(chapter)


# rename for consistency and combine all data
names(detrended_dists) <- c("bottle_dim0_detr", "bottle_dim1_detr", "bottle_dim2_detr", 
                            "wasser_dim0_detr", "wasser_dim1_detr", "wasser_dim2_detr")
predictors <- cbind(betti_numbers_cluster, detrended_betti, 
                    homol_dist_clust, detrended_dists,
                    topics_number)


# save final dataset
save(predictors, chapter_descriptives, file = "predictors_and_DV_v3l.RData")
