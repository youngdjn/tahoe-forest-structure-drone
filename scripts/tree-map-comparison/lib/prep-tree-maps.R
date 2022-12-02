# Function to prepare predicted and observed tree maps for comparison by:
# - projecting the predicted trees to the same projection as the observed trees
# - assigning unique IDs to each tree
# - assigning the trees an attribute that designates whether they are within the internally-buffered region
# - adding an (empty for now) attribute that will store which predicted tree the observed tree is matched to
# It modifies the original files provided to it.

prep_tree_maps_for_comparison = function(observed_trees_filepath,
                                             predicted_trees_filepath,
                                             plot_bound_filepath,
                                             internal_plot_buffer_dist) {
  
  # Load field plot boundary and buffer it in so we have a way of spatially subsetting our predicted tree dataset to a region
  #    where all predicted trees have a fair chance of matching to an observed tree, allowing for some spatial error.
  #    Because observed trees are from a limited area, we need to be able to subset the predicted trees to a *more* limited area
  plot_bound = st_read(plot_bound_filepath)
  plot_bound_internal = plot_bound %>% st_transform(3310) %>% st_buffer(-internal_plot_buffer_dist) %>%
    mutate(internal_area = TRUE)
  
  ### Read in observed (field plot) trees and prepare them for comparison to predicted trees ###
  # Assumes field survey includes all trees > 5 m tall and that the dataset has already been filtered to those trees
  observed_trees = st_read(observed_trees_filepath) %>% st_transform(3310)
  
  # Assign incremental unique ID to each observed tree
  observed_trees = observed_trees  %>%
    mutate(observed_tree_id = 1:nrow(observed_trees))
  
  # Assign the trees an attribute that designates whether they are within the internally-buffered region
  observed_trees$internal_area = st_intersects(observed_trees,plot_bound_internal, sparse=FALSE) %>% as.vector
  
  # Add an (empty for now) attribute that will store which predicted tree the observed tree is matched to
  observed_trees$final_predicted_tree_match_id = NA
  
  ### Read in predicted (drone-detected) trees and prepare them for comparison to observed trees ###
  #      Project them to the observed trees dataset projection, and assign them unique IDs
  predicted_trees = st_read(predicted_trees_filepath) %>%
    st_transform(st_crs(observed_trees))
  
  predicted_trees = predicted_trees %>%
    mutate(predicted_tree_id = 1:nrow(predicted_trees))
  
  # Assign the trees an attribute that designates whether they are within an internally-buffered region
  predicted_trees$internal_area = st_intersects(predicted_trees,plot_bound_internal, sparse=FALSE) %>% as.vector
  
  ## Write them both back to their originals
  st_write(predicted_trees, predicted_trees_filepath, delete_dsn = TRUE)
  st_write(observed_trees, observed_trees_filepath, delete_dsn = TRUE)
  
}




