## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map

library(tidyverse)
library(sf)
library(here)
library(furrr)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))



#### Load and clean ground map data ####
ground_map = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")) %>% st_transform(3310)
ground_map$ground_tree_id = 1:nrow(ground_map)
ground_map$final_drone_map_match_id = NA

# remove some duplicate trees
ground_map = ground_map %>%
  filter(!(tree_id %in% c(237, 1268, 2491, 2490, 1901)))

# correct a height typo
ground_map[ground_map$tree_id == 668,"Height"] = 36.4


#### Constants ####
search_distance_fun = function(x) { 1 + 0.1 * x}
# previously: search_distance_fun = function(x) { 4 }
search_distance = search_distance_fun(40) # this is used for buffering in and out of the stand to look for perimeter trees
search_height_proportion = .50
# previously: search_height_proportion = .3
smallest_size = 10 # smallest tree size to include in a size class for comparison

#### Filter ground map data to only trees within the height search distance of the smallest size category (here hard-coded as 10 m)
ground_map = ground_map %>%
  filter(Height >= (smallest_size-smallest_size*search_height_proportion))


#### Load drone data ####

### Get all file names
drone_map_files = list.files(data("detected_trees"), pattern="paramset.*\\.geojson", full.names = TRUE)


# make output directory if it doesn't exist
dir.create(data("drone_map_evals/individual"))
dir.create(data("drone_map_evals/stem_map_pairing_lines"))

plan(multiprocess)

# Run it. Don't need the returned value. It is a record of whether the drone set was skipped because it had an implausible number of trees (or the output already existsed) .
comparison_plausible = future_map(drone_map_files %>% sample, match_compare_single_wrapper, ground_map = ground_map, .options=future_options(scheduling=5))