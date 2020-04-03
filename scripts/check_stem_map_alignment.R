#### For each ground plot and subplot, determine number of trees that have been aligned to drone map.
# Author: Derek Young

data_dir = "/storage/tahoe-stem-map-alignment/"

library(tidyverse)
library(readxl)
library(sf)
library(here)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))

#### Load and clean data ####

ground_trees_orig = st_read(data("ground_truth_stem_map/uncorrected_copy/ept_trees_01_uncorrected_copy.geojson")) %>% filter(Height > 5) %>% st_transform(3310)
ground_trees_corr = st_read(data("ground_truth_stem_map/corrected/ept_trees_01_corrected.geojson")) %>% filter(Height > 5) %>% st_transform(3310)

drone_trees = st_read(data("drone_stem_map/treetops_vwf001.geojson")) %>% st_transform(3310)

## clip drone trees to a buffer around ground trees

ground_trees_buffer = st_buffer(ground_trees_orig, 20) %>% st_union()
drone_trees = st_intersection(drone_trees,ground_trees_buffer)


#### Check alignment tree by tree ####

## For each tree in corrected ground map:
##    - for the nearest drone tree, get: distance, percent height change
##    - get x and y shift, relative to uncorrected map
##    - compute: was it moved, and is it on top of a drone tree




compare_ground_drone = function(i) {

  tree = ground_trees_corr[i,]

  # filter drone trees to those within +- 25% height
  drone_trees_heightmatch = drone_trees %>%
    filter(height %>% between(tree$Height-10, tree$Height+10))
  
  # get the closest of the height match trees
  drone_tree_match_index = st_nearest_feature(tree,drone_trees_heightmatch)
  drone_tree_match = drone_trees_heightmatch[drone_tree_match_index,]
  # get their distance
  dist = st_distance(tree,drone_tree_match)
  # get coord offests
  offset = st_coordinates(drone_tree_match) - st_coordinates(tree)
  
  ## get the distance from the original (uncorrected) tree
  # look up uncorrected tree
  tree_orig = ground_trees_orig %>%
    filter(tree_id == tree$tree_id)
  
  tree_shift = st_coordinates(tree) - st_coordinates(tree_orig)
  
  tree_match = data.frame(ground_tree_id = tree$tree_id,
                          ground_tree_plot = tree$Plot,
                          ground_tree_loc = tree$data_col_location,
                          ground_tree_height = tree$Height,
                          ground_tree_shift_x = tree_shift[1],
                          ground_tree_shift_y = tree_shift[2],
                          drone_tree_id = drone_tree_match$treeID,
                          drone_tree_height = drone_tree_match$height,
                          ground_drone_distance = dist,
                          ground_drone_offset_x = offset[1],
                          ground_drone_offset_y = offset[2])
  
  return(tree_match)
}



tree_matches = map_dfr(1:nrow(ground_trees_corr), compare_ground_drone)



