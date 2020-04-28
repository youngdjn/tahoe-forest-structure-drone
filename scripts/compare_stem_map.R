## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map


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
search_distance = 4
search_height_proportion = .2

#### Load drone data ####

### Get all file names
drone_map_files = list.files(data("post_metashape_products/detected_trees"), pattern="paramset.*\\.geojson", full.names = TRUE)



drone_map_file = drone_map_files[1]
drone_map = st_read(drone_map_file) %>% st_transform(3310)

## get drone map name from filename
parts = str_split(drone_map_file,"/")[[1]]
filename = parts[length(parts)]
drone_map_name = str_split(filename,"\\.")[[1]][1]



## Run compariston/eval ##
profvis({
match_compare_single(ground_map, drone_map, drone_map_name = drone_map_name)
})






















