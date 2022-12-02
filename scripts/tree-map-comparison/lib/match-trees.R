# R functions used when matching predicted (drone) trees with observed (stem map) trees

# Used within `get_closest_tree` function
# increases search distance for higher trees
search_distance_fun <- function(search_distance_fun_slope,
                                search_distance_fun_intercept,
                                x) {
  search_distance_fun_slope * x + search_distance_fun_intercept
}

# For each observed tree, find the closest predicted tree
# within a similar height range (defined by `search_height_proportion` argument)
# and within an appropriate distance and x height


get_closest_tree <- function(observed_tree_index,      # For which of the observed trees should we find the closest matching predicted tree?
                             observed_trees,           # sf object of all observed trees
                             predicted_trees,          # sf object of all predicted trees
                             dist_mat,                 # Distance between every predicted tree and every observed tree
                             search_height_proportion, # Matches between predicted trees and observed trees are possible when the predicted tree height is within search_height_proportion * observed_tree_height
                             search_distance_fun_slope,
                             search_distance_fun_intercept) {
  observed_tree <- observed_trees[observed_tree_index, ]
  observed_tree$predicted_tree_match_id <- NA
  observed_tree$predicted_tree_match_distance <- NA
  
  predicted_trees$distance_to_observed_tree <- dist_mat[observed_tree_index, ] %>%
    as.numeric()
  
  # if we already matched that ground tree
  if (!is.na(observed_tree$final_predicted_tree_match_id)) {
    return(observed_tree)
  }
  
  # thin predicted trees map to trees within size bounds
  lwr <- observed_tree$Height - observed_tree$Height * search_height_proportion
  upr <- observed_tree$Height + observed_tree$Height * search_height_proportion
  predicted_trees_candidates <- predicted_trees[between(predicted_trees$height, lwr, upr), ]
  

  # if there are no predicted trees within the height bounds, return the observed tree unmatched
  if (nrow(predicted_trees_candidates) == 0) {
    return(observed_tree)
  }
  
  # thin to those within distance
  
  predicted_trees_candidates <- predicted_trees_candidates %>%
    dplyr::filter(distance_to_observed_tree < search_distance_fun(
      search_distance_fun_slope,
      search_distance_fun_intercept,
      observed_tree$Height)
    )
  
  # if there are no predicted trees within the height and distance bounds, return the observed tree unmatched
  if (nrow(predicted_trees_candidates) == 0) {
    return(observed_tree)
  }
  
  # take the closest
  
  nearest_tree_idx <- which.min(predicted_trees_candidates$distance_to_observed_tree)
  predicted_trees_match <- predicted_trees_candidates[nearest_tree_idx, ]
  
  observed_tree$predicted_tree_match_id <- predicted_trees_match$predicted_tree_id
  observed_tree$predicted_tree_match_distance <-
    predicted_trees_match$distance_to_observed_tree
  
  return(observed_tree)
}

# Prepares stem map ground truth and Tree Approximate Object data for matching
# sends each row through `get_closest_tree` function
# joins match info back to ground truth data

get_closest_matches <- function(observed_trees,                   # sf object of all observed trees
                                predicted_trees,                  # sf object of all predicted trees
                                predicted_trees_exclude,          # Which predicted trees are not candidates for matching (because they're already matched)? (uses predicted_tree_id)
                                search_height_proportion,         # Matches between predicted trees and observed trees are possible when the predicted tree height is within search_height_proportion * observed_tree_height
                                search_distance_fun_slope,
                                search_distance_fun_intercept) {
  
  # Out of the predicted trees, remove those that are not candidates for matching because they are already matched
  predicted_trees_dropexclude <- predicted_trees %>%
    filter(!(predicted_tree_id %in% predicted_trees_exclude))
  
  dist_mat <- st_distance(observed_trees, predicted_trees_dropexclude)
  
  # Remove spatial attributes (convert to normal data frame) to prevent conflicts when joining observed and predicted tree records (which have diff coords) (spatial position is no longer necessary at this matching stage)
  observed_trees_nonsp <- observed_trees
  st_geometry(observed_trees_nonsp) <- NULL
  
  # Remove spatial attributes (convert to normal data frame) to prevent conflicts when joining observed and predicted tree records (which have diff coords) (spatial position is no longer necessary at this matching stage)
  predicted_trees_nonsp <- predicted_trees_dropexclude
  st_geometry(predicted_trees_nonsp) <- NULL
  
  # For each observed tree, get its closest match among the eligible predicted trees
  observed_predicted_matches <- map_dfr(
    1:nrow(observed_trees),
    get_closest_tree,
    observed_trees = observed_trees_nonsp,
    predicted_trees = predicted_trees_nonsp,
    dist_mat = dist_mat,
    search_height_proportion = search_height_proportion,
    search_distance_fun_slope = search_distance_fun_slope,
    search_distance_fun_intercept = search_distance_fun_intercept
  )
  
  observed_predicted_matches <- observed_predicted_matches %>%
    dplyr::select(observed_tree_id, predicted_tree_match_id, predicted_tree_match_distance)
  
  observed_trees <- left_join(observed_trees, observed_predicted_matches, by = "observed_tree_id")
}

# Look for predicted trees that were matched to multiple observed trees
# and assign them to the observed tree they're closest to

remove_overmatched_predicted_trees <- function(observed_trees) {
  
  # Create a data frame indicating the number of observed trees that each predicted tree is matched to,
  # along with the distance to the closest of observed trees matched to each predicted tree (so that one can be kept and any others dropped)
  observed_summ <- observed_trees %>%
    filter(!is.na(predicted_tree_match_id)) %>% # only keep records for observed trees that were matched to a predicted tree
    group_by(predicted_tree_match_id) %>%       # summarize by counting the number of observed trees (rows in the data frame) matched to each predicted tree
    summarize(
      n = n(),
      closest_dist = min(predicted_tree_match_distance)
    )
  st_geometry(observed_summ) <- NULL
  
  ## bind the min distances to the ground map
  observed_trees <- left_join(observed_trees, observed_summ, by = "predicted_tree_match_id")
  
  ## keep the match if "drone map distance" == "closest distance"
  # figure out which they are to keep
  keep_match_indexes <- which(
    observed_trees$predicted_tree_match_distance == observed_trees$closest_dist
  )
  
  # store keeper matches in a new column
  observed_trees[keep_match_indexes, "final_predicted_tree_match_id"] <-
    observed_trees[keep_match_indexes, ]$predicted_tree_match_id
  
  ## clear the temporary columns
  observed_trees <- observed_trees %>%
    dplyr::select(
      -predicted_tree_match_id,
      -predicted_tree_match_distance,
      -n,
      -closest_dist
    )
  
  return(observed_trees)
}

# Send files through `get_closest_matches` and
# `remove_overmatched_drone_trees` functions multiple times,
# excluding trees that have already been matched.
match_trees_helper <- function(observed_trees,
                              predicted_trees,
                              search_height_proportion,
                              search_distance_fun_slope,
                              search_distance_fun_intercept) {

  # Iterate through tree matching four times (which from experience is more than sufficient to get all possible matches)
  # This loop finds the closest eligible predicted tree to each observed tree that meets the conditions (height and distance) for matching it,
  # then when multiple observed trees are matched to the same ground tree, only the closest one (by distance) is retained; the others are removed and matching is re-attempted
  predicted_trees_matched = NULL # placeholder to be populated (within the loop iterations) with IDs of predicted trees that have already been matched (so we don't try to match to them in later iterations)
  
  for(i in 1:4) {
    
    observed_trees <- get_closest_matches(
      observed_trees,
      predicted_trees = predicted_trees,
      predicted_trees_exclude = predicted_trees_matched,
      search_height_proportion,
      search_distance_fun_slope,
      search_distance_fun_intercept
    )
    
    # When multiple observed trees are matched to the same ground tree, only retain the closest one (by distance)
    observed_trees <- remove_overmatched_predicted_trees(observed_trees)
    
    # get the matches that are off-limits (already matched)
    predicted_trees_matched <- unique(observed_trees$final_predicted_tree_match_id)
    cat("Predicted trees matched (pass",i,"):", length(predicted_trees_matched), "\n")
    
  }
  
  
  return(observed_trees)
}


# Function matches each ground truth tree (observed) to the closest Tree Approximate Object (predicted)
# It accepts user input (canopy_position_info: boolean) on whether or not the ground truth
# data has information identifying the trees observed as understory or dominant
match_trees <- function(observed_trees_filepath,
                                predicted_trees_filepath,
                                plot_bound_filepath,
                                search_height_proportion,
                                canopy_position_info,
                                tmp_dir,
                                search_distance_fun_slope,
                                search_distance_fun_intercept) {
  

  observed_trees = st_read(observed_trees_filepath)
  predicted_trees = st_read(predicted_trees_filepath)
  
  
  # Create temp dir if doesn't exist
  dir.create(tmp_dir)
  
  # Get the name for this comparison run from the filename of the predicted trees dataset
  predicted_tree_dataset_name <-
    tools::file_path_sans_ext(basename(predicted_trees_filepath))
  
  # Prep the filenames for storing the matched predicted tree datasets (for matching to all observed trees and only overstory observed trees)
  tmp_gpkg_filename_all <- file.path(
      tmp_dir,
      paste0(predicted_tree_dataset_name, "_matched_all.gpkg")
    )
  tmp_gpkg_filename_overstory <- file.path(
      tmp_dir,
      paste0(predicted_tree_dataset_name, "_matched_overstory.gpkg")
    )

  # Run compare_tree_maps functions based on the tree category
  # information for validation dataset

  # For example, if the 'canopy_position_info' is 'no' i.e., if the information
  # identifying the trees as understory or dominant is unavailable in the ground
  # truth dataset then use 'all' the trees in the dataset for finding matches.
  # This is usually applicable if pdal.tao are used as ground truth to compare
  # the results from synthetic TAOs
  matched_observed_trees_all <- match_trees_helper(
    observed_trees,
    predicted_trees,
    search_height_proportion,
    search_distance_fun_slope,
    search_distance_fun_intercept
  )
  
  st_write(matched_observed_trees_all, tmp_gpkg_filename_all, delete_dsn = TRUE)
  

  if (canopy_position_info) {

    matched_observed_trees_overstory <- match_trees_helper(
      observed_trees %>% filter(under_neighbor == FALSE),
      predicted_trees,
      search_height_proportion,
      search_distance_fun_slope,
      search_distance_fun_intercept
    )
    
    st_write(matched_observed_trees_overstory, tmp_gpkg_filename_overstory, delete_dsn = TRUE)

  }
}
  
