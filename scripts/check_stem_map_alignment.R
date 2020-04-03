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
    filter(height %>% between(tree$Height-8, tree$Height+8))
  
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


#### Compute derived match variables ####

tree_matches = tree_matches %>%
  mutate(shifted = ground_tree_shift_x > 0.01 | ground_tree_shift_y > 0.01) %>%
  mutate(aligned = ground_drone_offset_x < 0.1 & ground_drone_offset_y < 0.1)


#### Summarize shifted trees ####

trees_shifted = tree_matches %>%
  filter(shifted) %>%
  group_by(ground_tree_plot,ground_tree_loc) %>%
  summarize(n_trees_shifted = n(),
            mean_x_shift = mean(ground_tree_shift_x),
            mean_y_shift = mean(ground_tree_shift_y),
            cv_x_shift = abs(sd(ground_tree_shift_x) / mean(abs(ground_tree_shift_x))),
            cv_y_shift = abs(sd(ground_tree_shift_y) / mean(abs(ground_tree_shift_y))))

## Pull in number of trees (aligned or not) per plot. This also has the effect of making sure all plot locs are included, even if no aligned trees

all_subplots = tree_matches %>%
  select(ground_tree_plot,ground_tree_loc) %>%
  group_by(ground_tree_plot,ground_tree_loc) %>%
  summarize(n_trees_total = n())

tree_shift_summary = full_join(trees_shifted, all_subplots) %>%
  mutate(n_trees_shifted = ifelse(is.na(n_trees_shifted),0,n_trees_shifted)) %>%
  mutate(percent_trees_shifted = n_trees_shifted/n_trees_total * 100)

## Make it look a little less overwhelming by rounding

tree_shift_summary = tree_shift_summary %>%
  mutate_at(vars(contains("shift")), function(x) round(x,3)) %>%
  select(ground_tree_plot,ground_tree_loc,n_trees_shifted,n_trees_total,percent_trees_shifted,everything())


#### Summarize by plot (not also subplot) ####

tree_shift_summary_plot = tree_shift_summary %>%
  group_by(ground_tree_plot) %>%
  summarize(n_trees_shifted = sum(n_trees_shifted),
            n_trees_total = sum(n_trees_total)) %>%
  mutate(percent_trees_shifted = n_trees_shifted/n_trees_total)


#### Write it ####

write.csv(tree_shift_summary,data("alignment_eval/tree_shift_summary.csv"),row.names=FALSE)
write.csv(tree_shift_summary_plot,data("alignment_eval/tree_shift_summary_plot.csv"),row.names=FALSE)



