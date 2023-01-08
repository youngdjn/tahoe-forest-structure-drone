# R functions used when matching predicted (drone) trees with observed (stem map) trees

# Function to match predicted trees against a single set (e.g. overstory trees only, or all trees) of observed trees. This is wrapped inside the match_trees function which runs it once for all trees and (if specified) once for overstory only.
match_trees_singlestratum = function(observed_trees,
                                     predicted_trees,
                                     search_height_proportion,
                                     search_distance_fun_slope,
                                     search_distance_fun_intercept) {
  # Faster tree matching logic
  # 
  # for each observed tree, compute distance to every predicted tree that is a candidate:
  #   within distance window function scaled by height of the observed tree
  # within height window range based on height of the observed tree
  # 
  # Logic:
  #   
  #   compute distance matrix for all observed to all predicted
  # Thin to only those distances within the radius for the tallest trees
  # 
  # for each observed point:
  #   compute its acceptable distance and height interval
  # thin to only points within the height interval
  # find all points within its acceptable horizontal distance
  # 
  # This produces a df of all the potential matches between predicteds and observeds, with their distances
  # 
  # Find the closest matches:
  #   Sort the matches list in ascending order
  # For each potential match:
  #   see if the predicted or observed is already matched (keep a running list)
  # if not, record the match, and add the predicted and observed IDs to the already-matched pool
  
  
  dist_mat <- st_distance(observed_trees, predicted_trees)
  
  colnames(dist_mat) = predicted_trees$predicted_tree_id
  rownames(dist_mat) = observed_trees$observed_tree_id
  
  dist_graph = as.data.frame(as.table(dist_mat)) %>%
    mutate(Var1 = as.numeric(as.character(Var1)),         # Need the as.character step because otherwise it's a factor and converting to numeric just assigns each ID a sequential index rather than presrving the number
           Var2 = as.numeric(as.character(Var2))) %>%
    rename("observed_tree_id" = "Var1",
           "predicted_tree_id" = "Var2",
           "dist" = "Freq") %>%
    mutate(dist = as.numeric(dist))
  
  # filter to only trees within the maximum matching distance
  tallest_obs_tree = max(observed_trees$Height)
  max_dist_window = search_distance_fun_intercept + search_distance_fun_slope*tallest_obs_tree
  dist_graph = dist_graph %>%
    filter(dist <= max_dist_window)
  
  # pull in each tree's height
  predicted_height = predicted_trees %>%
    select(predicted_tree_id, predicted_height = height)
  st_geometry(predicted_height) = NULL
  
  observed_height = observed_trees %>%
    select(observed_tree_id, observed_height = Height)
  st_geometry(observed_height) = NULL
  
  dist_graph = left_join(dist_graph,observed_height)
  dist_graph = left_join(dist_graph,predicted_height)
  
  dist_graph = dist_graph %>%
    mutate(max_dist = search_distance_fun_intercept + search_distance_fun_slope*observed_height) %>%
    filter(dist <= max_dist) %>%
    mutate(min_height = search_height_proportion * observed_height,
           max_height = (1+search_height_proportion) * observed_height) %>%
    filter(predicted_height >= min_height & predicted_height <= max_height) %>%
    arrange(dist)
  
  predicted_ids_matched = NULL
  observed_ids_matched = NULL
  
  for(i in 1:nrow(dist_graph)) {
    
    row = dist_graph[i,]
    if(row$predicted_tree_id %in% predicted_ids_matched | row$observed_tree_id %in% observed_ids_matched) {
      #already matched, no longer eligible
      next()
    } else {
      #this is a new match; record it
      predicted_ids_matched = c(predicted_ids_matched,row$predicted_tree_id)
      observed_ids_matched = c(observed_ids_matched,row$observed_tree_id)
      observed_trees[observed_trees$observed_tree_id == row$observed_tree_id, "final_predicted_tree_match_id"] = row$predicted_tree_id
    }
    
  }
  
  return(observed_trees)
  
}


# Function matches each ground truth tree (observed) to the closest Tree Approximate Object (predicted)
# It accepts user input (additional_overstory_comparison: boolean) on whether or not the ground truth
# data has information identifying the trees observed as understory or dominant
match_trees <- function(observed_trees_filepath,
                        predicted_trees_filepath,
                        search_height_proportion,
                        additional_overstory_comparison,
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
  
  ## Match against *all* (not overstory only) observed trees
  matched_observed_trees_all <- match_trees_singlestratum(
    observed_trees,
    predicted_trees,
    search_height_proportion,
    search_distance_fun_slope,
    search_distance_fun_intercept
  )
  
  st_write(matched_observed_trees_all, tmp_gpkg_filename_all, delete_dsn = TRUE)
  
  # If specified, match against *overstory only* observed trees
  if (additional_overstory_comparison) {
    
    matched_observed_trees_overstory <- match_trees_singlestratum(
      observed_trees %>% filter(under_neighbor == FALSE),
      predicted_trees,
      search_height_proportion,
      search_distance_fun_slope,
      search_distance_fun_intercept
    )
    
    st_write(matched_observed_trees_overstory, tmp_gpkg_filename_overstory, delete_dsn = TRUE)
    
  }
  
}