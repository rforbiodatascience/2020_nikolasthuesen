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

#A lot of cheating here. Both loaded, cleaned and wrangled.
data('gravier', package = 'datamicroarray')
set.seed(676571)
cancer_data=mutate(as_tibble(pluck(gravier,"x")),y=pluck(gravier,"y"),pt_id=1:length(pluck(gravier, "y")),age=round(rnorm(length(pluck(gravier,"y")),mean=55,sd=10),1))
cancer_data=rename(cancer_data,event_label=y)
cancer_data$age_group=cut(cancer_data$age,breaks=seq(10,100,by=10))

my_data_raw = cancer_data

# Wrangle data
# ------------------------------------------------------------------------------
my_data <- my_data_raw # %>% ...

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data,
          path = "data/01_my_data.tsv")