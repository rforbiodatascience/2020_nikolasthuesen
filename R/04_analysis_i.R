# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

# Model data
# ------------------------------------------------------------------------------
#numeric_columns <- my_data_clean_aug %>% lapply(is.numeric) %>% unlist()
 
my_data_numeric <- my_data_clean_aug %>% select(starts_with('g')) 

my_pca <- my_data_numeric  %>%
  prcomp(center = TRUE, scale. = TRUE)

# Visualise data
# ------------------------------------------------------------------------------

#PCA plot
my_pca %>% tidy('pcs') %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

#Augmentet PCA table
my_pca_aug <- my_pca %>% augment(my_data_clean_aug)

#Plot first two PCs and color by event label
my_pca_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = event_label)) +
  geom_point()

#Do K-means clustering
my_k_org <- my_pca_aug %>%
  select(starts_with('g')) %>% 
  kmeans(centers = 2)
my_k_org

#Add clustering label to pca table:
my_pca_aug_k_org <- my_k_org %>% 
  broom::augment(my_pca_aug) %>%
  rename(cluster_pca = .cluster)
my_pca_aug_k_org

#plot K-means clustering
my_pca_aug_k_org %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point()

# Write data


# ------------------------------------------------------------------------------
#write_tsv(...)
#ggsave(...)