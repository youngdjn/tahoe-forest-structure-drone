library(sf)
library(tidyverse)
library(terra)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

## Conveinence functions ##
# Most importantly, this defines a function 'datadir' that prepends any relative file path with the absolute path to the data directory (specified in data-dir.txt)
source(here("scripts/convenience-functions.R"))

## Function definitions
source("scripts/tree-map-comparison/lib/prep-tree-maps.R")
source("scripts/tree-map-comparison/lib/match-trees.R")
source("scripts/tree-map-comparison/lib/tree-det-stats.R")
source("scripts/tree-map-comparison/lib/connect-matches.R")
source("scripts/tree-map-comparison/lib/area-based-stats.R")
source("scripts/tree-map-comparison/lib/combine-stats-make-corr-plots.R")


# Assumes observed trees (field survey) includes all trees > 5 m tall and that the dataset has already been filtered to those trees

tmp_dir = datadir("temp")
output_dir = datadir("comparison-output")
observed_trees_filepath = datadir("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")
predicted_trees_filepath = datadir("detected_trees_example/paramset14_1016_20201021T0648_dsm_chm-vwf_196.geojson")
plot_bound_filepath =  datadir("study_area_perimeter/ground_map_mask_precise.geojson")


# Prepare the predicted and observed tree datasets for comparison by adding the right attributes (e.g. whether in internally buffered area, which predicted tree the observed tree is matched to) and projecting the predicted tree dataset to the observed tree dataset
prep_tree_maps_for_comparison(observed_trees_filepath = observed_trees_filepath,
                                 predicted_trees_filepath = predicted_trees_filepath,
                                 plot_bound_filepath =  plot_bound_filepath,
                                 internal_plot_buffer_dist = 5)

match_trees(observed_trees_filepath = observed_trees_filepath,
                    predicted_trees_filepath = predicted_trees_filepath,
                    plot_bound_filepath =  plot_bound_filepath,
                    search_height_proportion = 0.5,
                    canopy_position_info = TRUE,
                    tmp_dir = datadir("temp"),
                    search_distance_fun_slope = 0.1,
                    search_distance_fun_intercept = 1)

# ^ This function saves (in tmp_dir) a gpkg of the observed trees, with a column indicating which predicted tree (if any) it was matched to

connect_matches(tmp_dir = tmp_dir,
                predicted_trees_filepath = predicted_trees_filepath,
                output_dir = output_dir)

tree_det_stats(tmp_dir = tmp_dir,
                       predicted_trees_filepath = predicted_trees_filepath,
                       output_dir = output_dir)

area_based_stats(observed_trees_filepath = observed_trees_filepath,
                 predicted_trees_filepath = predicted_trees_filepath,
                 focal_region_polygon_filepath = plot_bound_filepath,
                 virtual_plot_size = 30,
                 canopy_position_info = TRUE,
                 tmp_dir = tmp_dir)

combine_stats(tmp_dir = tmp_dir,
              output_dir = output_dir,
              predicted_trees_filepath = predicted_trees_filepath)

make_height_corr_plots(output_dir = output_dir,
                 predicted_trees_filepath = predicted_trees_filepath)  
