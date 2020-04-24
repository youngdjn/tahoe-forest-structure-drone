data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(sf)
library(here)

source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))



#### Load and clean ground map data ####
ground_map = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")) %>% filter(Height > 5) %>% st_transform(3310)
ground_map$ground_tree_id = 1:nrow(ground_map)
ground_map$final_drone_map_match_id = NA

#### Constants ####
search_distance = 5
search_height = 10

#### Load drone data ####
drone_map = st_read(data("reference_drone_stem_map/treetops_vwf001.geojson")) %>% st_transform(3310)


#### Run compariston/eval ####
match_compare_single(ground_map, drone_map, drone_map_name = "test1")


