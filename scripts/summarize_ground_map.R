#### Summarize the ground survey data
# Author: Derek Young

library(tidyverse)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))

#### Load data ####

ground_map = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")) %>% st_transform(3310)
roi = st_read(data("study_area_perimeter/ground_map_mask_precise.geojson")) %>% st_transform(3310)

# remove some duplicate trees
ground_map = ground_map %>%
  filter(!(tree_id %in% c(237, 1268, 2491, 2490, 1901)))

# correct a height typo
ground_map[ground_map$tree_id == 668,"Height"] = 36.4


trees = st_intersection(ground_map,roi)

trees = trees %>%
  filter(
         Height > 5)

nrow(trees)
