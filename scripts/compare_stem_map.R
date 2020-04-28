## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map


data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(sf)
library(here)
library(furrr)

source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))



#### Load and clean ground map data ####
ground_map = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")) %>% st_transform(3310)
ground_map$ground_tree_id = 1:nrow(ground_map)
ground_map$final_drone_map_match_id = NA

#### Constants ####
search_distance = 4
search_height_proportion = .25
smallest_size = 10 # smallest tree size to include in a size class for comparison

#### Filter ground map data to only trees within the height search distance of the smallest size category (here hard-coded as 10 m)
ground_map = ground_map %>%
  filter(Height >= (smallest_size-smallest_size*search_height_proportion))


#### Load drone data ####

### Get all file names
drone_map_files = list.files(data("post_metashape_products/detected_trees"), pattern="paramset.*\\.geojson", full.names = TRUE)


plan(multiprocess)

# Run it. Don't need the returned value. It is a record of whether the drone set was skipped because it had an implausible number of trees.
comparison_plausible = map(drone_map_files[173], match_compare_single_wrapper, ground_map = ground_map)

















