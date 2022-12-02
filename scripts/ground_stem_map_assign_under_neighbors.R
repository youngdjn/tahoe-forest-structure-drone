## Determine if a ground map tree is considered under another, and store this evaluation in the ground map in the attribute `under_neighbor`

library(tidyverse)
library(sf)
library(here)
library(furrr)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

ground_map = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")) %>% st_transform(3310)



## distances between all trees
ground_trees_buffer = st_buffer(ground_map, 8)
tree_neighbors = st_intersects(ground_trees_buffer,ground_map)

## are any of the tree indexes in the list taller than the focal?
any_taller = function(i, tree_neighbors, ground_map) {
  
  focal_height = ground_map[i,]$Height
  other_trees = tree_neighbors[[i]] %>% setdiff(i)
  
  other_trees_sp = ground_map[other_trees,]
  other_trees_sp$dist = st_distance(ground_map[i,], other_trees_sp) %>% as.vector
  
  other_trees_sp$heightdiff = other_trees_sp$Height - focal_height
  other_trees_sp$dist_thresh = other_trees_sp$heightdiff * 0.1 + 1
  other_trees_sp$focal_is_under = ((focal_height < other_trees_sp$Height) & (other_trees_sp$dist < other_trees_sp$dist_thresh))
  
  focal_is_under = any(other_trees_sp$focal_is_under)
  
  return(focal_is_under)
  
}

ground_map$under_neighbor = map_lgl(1:nrow(ground_map), any_taller, tree_neighbors = tree_neighbors, ground_map = ground_map)

st_write(ground_map,data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson"), delete_dsn = TRUE)