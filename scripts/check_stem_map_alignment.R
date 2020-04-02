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

ground_trees_orig = st_read(data("ground_truth_stem_map/uncorrected_copy/ept_trees_01_uncorrected_copy.geojson")) %>% st_transform(3310)
ground_trees_corr = st_read(data("ground_truth_stem_map/corrected/ept_trees_01_corrected.geojson")) %>% st_transform(3310)

drone_trees = st_read(data("drone_stem_map/treetops_vwf001.geojson")) %>% st_transform(3310)

## clip drone trees to a buffer around ground trees

ground_trees_buffer = st_buffer(ground_trees_orig, 20) %>% st_union()
drone_trees = st_intersection(drone_trees,ground_trees_buffer)


#### Check alignment tree by tree ####

## For each tree in corrected ground map:
##    - for the nearest drone tree, get: distance, percent height change
##    - get x and y shift, relative to uncorrected map
##    - compute: was it moved, and is it on top of a drone tree

tree = ground_trees_corr[1,]

# filter drone trees to those within +- 25% height
drone_trees_heightmatch = drone_trees %>%
  filter(height %>% between(tree$Height*0.75, tree$Height*1.25))

# get the closest of the height match trees
drone_tree_match_index = st_nearest_feature(tree,drone_trees_heightmatch)
drone_tree_match = drone_trees_heightmatch[drone_tree_match_index,]
# get their distance
dist = st_distance(tree,drone_tree_match)
# get coord offests
offset = st_coordinates(drone_tree_match) - st_coordinates(tree)

tree_match = data.frame(ground_tree_id = tree$ground_tree_id,
                           ground_tree_plot = tree$Plot,
                           ground_tree_loc = tree$data_col_location,
                           ground_tree_height = tree$Height,
                          drone_tree_id = drone_tree_match$treeID,
                          drone_tree_height = drone_tree_match$height,
                        ground_drone_distance = dist,
                          ground_drone_offset_x = offset[1],
                          ground_drone_offset_y = offset[2])




