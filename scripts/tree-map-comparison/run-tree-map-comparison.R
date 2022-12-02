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

# Path to the observed (reference) stem map. It is assumed that this stem map includes exhaustive survey of trees with heights down to 50% of the minimum height class evaluated (currently hard-coded at 10 m, so heights down to at least 5 m). If the dataset has smaller trees, removing them first will make this run faster.
#    If you wish to include matching for "overstory trees only", this stem map should be pre-attributed with a column called "under-neighbor" that indicates whether a tree is understory or overstory.
#    This attribution is performed by the script scripts/ground_stem_map_assign_under_neighbor.R
observed_trees_filepath = datadir("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")
# Path to the predicted (drone) stem map. It is assumed that this stem map includes trees with heights down to 50% of the minimum height class evaluated (currently hard-coded at 10 m, so heights down to at last 5 m). If the dataset has smaller trees, removing them first will make this run faster.
predicted_trees_filepath = datadir("detected_trees_example/paramset14_1016_20201021T0648_dsm_chm-vwf_196.geojson")
# Path to the field plot boundary. This defines the outer edge of the field (observed trees) plot. It is assumed that the predicted tree stem map extends at least to this boundary if not beyond.
plot_bound_filepath =  datadir("study_area_perimeter/ground_map_mask_precise.geojson")

# Location of temp directory (holds intermediate files between the comparison steps) and the directory for comparison outputs
tmp_dir = datadir("temp")
output_dir = datadir("comparison-output")


#### BEGIN STEM MAP COMPARISON WORKFLOW ####

# Prepare the predicted and observed tree datasets for comparison by adding the necessary attributes (including: whether in internally buffered area, which predicted tree the observed tree is matched to) and projecting the predicted tree dataset to the observed tree dataset
# This function saves the prepared tree datasets back to their original filenames
prep_tree_maps_for_comparison(observed_trees_filepath = observed_trees_filepath,
                              predicted_trees_filepath = predicted_trees_filepath,
                              plot_bound_filepath =  plot_bound_filepath,
                              internal_plot_buffer_dist = 5) # By how many meters to buffer in from the plot edge when computing precision and recall to ensure all predicted trees have a fair chance to match to an observed tree

# This function saves (in tmp_dir) a gpkg of the observed trees, with a column indicating which predicted tree (if any) it was matched to
match_trees(observed_trees_filepath = observed_trees_filepath,
            predicted_trees_filepath = predicted_trees_filepath,
            tmp_dir = tmp_dir,
            search_height_proportion = 0.5, # Within what fraction (+ or -) of the observed tree height is a predicted tree allowed to match
            additional_overstory_comparison = TRUE, # In addition to comparing against *all* observed trees, should we compare against *overstory trees only*. If so, the observed trees stem map file needs to have an attribute "under_neighbor" previously assigned by the script scripts/ground_stem_map_assign_under_neighbor.R
            search_distance_fun_slope = 0.1, # The slope of the linear function for relating observed tree height to the potential matching distance
            search_distance_fun_intercept = 1)  # The intercept of the linear function for relating observed tree height to the potential matching distance

# This function saves (in output_dir/predicted_observed_pairing_lines) a gpkg of lines connecting each observed tree to the predicted tree it was paired to
connect_matches(predicted_trees_filepath = predicted_trees_filepath,
                tmp_dir = tmp_dir,
                output_dir = output_dir)

# This function saves individual tree detection accuracy statistics (sensitivity, precision, F-score, etc) in tmp_dir
tree_det_stats(predicted_trees_filepath = predicted_trees_filepath,
               tmp_dir = tmp_dir,
               output_dir = output_dir)

# This function saves area-based accuracy statistics in tmp_dir
area_based_stats(observed_trees_filepath = observed_trees_filepath,
                 predicted_trees_filepath = predicted_trees_filepath,
                 focal_region_polygon_filepath = plot_bound_filepath,
                 tmp_dir = tmp_dir,
                 virtual_plot_size = 30, # The height and width of contiguous square grid cells laid over the field plot for computing area-based statistics
                 additional_overstory_comparison = TRUE) # In addition to comparing against *all* observed trees, should we compare against *overstory trees only*. If so, the observed trees stem map file needs to have an attribute "under_neighbor" previously assigned by the script scripts/ground_stem_map_assign_under_neighbor.R
                 
# This function combines the individual tree detection and area-based accuracy statistics and saves in output_dir/tree_detection_evals
combine_stats(tmp_dir = tmp_dir,
              output_dir = output_dir,
              predicted_trees_filepath = predicted_trees_filepath)

# This function compares the heights of paired predicted and observed trees and makes a height correspondence scatterplot and saves it in output_dir/correlation_figures
make_height_corr_plots(output_dir = output_dir,
                 predicted_trees_filepath = predicted_trees_filepath)
