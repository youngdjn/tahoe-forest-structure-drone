# R functions used when calculating individual tree detection
# statistics after comparing Tree Approximate Objects with stem
# map ground truth data.

# Function to take a directory, filename, and dataframe and write it (creating the dir if necessary)
write_csv_to_dir = function(dataframe, output_dir, output_filename) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write_csv(dataframe, paste0(output_dir,"/",output_filename))
  
}


# Function computes the slope b by fitting the equation Y = a + bX
# using OLS.
get_slope <- function(y, x) {
  if (length(x) == 0 | length(y) == 0) {
    return(NA)
  }
  m <- lm(y ~ x)
  slope <- coef(m)[2]
  return(slope)
}

# Function to get height prediction accuracy metrics for all matched trees over a specified (observed) height
height_accuracy_metrics = function(trees_matched,min_height) {
  
  height_metrics <- trees_matched %>%
    filter(observed_tree_height >= min_height) %>%
    summarize(
      height_mae = mean(abs(height_err)),
      height_bias = mean(height_err),
      height_mean_observed = mean(observed_tree_height),
      height_mean_predicted = mean(predicted_tree_height),
      height_cor = cor(observed_tree_height, predicted_tree_height),
      height_slope = get_slope(observed_tree_height, predicted_tree_height)
    ) %>%
    mutate(height_cat = paste0(min_height,"+"))
  
}

# Function that takes a table of observed trees with an indication of whether they were matched to a predicted tree, a table of predicted trees with an indication of whether they were matched to an observed tree,
# and a minimum tree height to consider for mapping, and returns the number of predicted trees, number of observed trees, the number of predicted matched to observed, and number of observed matched to predicted.
# Purpose is that this can be re-run for different minimum heights.
# It is necessary to pass these two separate tables because when assessing
# sensitivity (recall), we need to check how many of the observed trees *within the internal negative buffered area* match to predicted trees *regardless of whether they're in the internal negative buffered area*,
# and similarly for precision, we need to check how many of the predicted trees *within the internal negative buffered area* match to observed trees *regardless of whether they're in the internal negative buffered area*,
# so the sets of trees used for each is different.
count_total_and_matched_trees = function(observed_predicted_match, predicted_observed_match, min_height) {
  
  observed_predicted_match_counts <- observed_predicted_match %>%
    filter(observed_tree_height >= min_height,
           observed_tree_internal_area == TRUE) %>%
    summarize(
      n_observed_matched_predicted =
        sum(!is.na(predicted_tree_id) & !is.na(observed_tree_id)),
      n_observed = n()
    )
  
  predicted_observed_match_counts <- predicted_observed_match %>%
    filter(predicted_tree_height >= min_height,
           predicted_tree_internal_area == TRUE) %>%
    summarize(
      n_predicted_matched_observed =
        sum(!is.na(predicted_tree_id) & !is.na(observed_tree_id)),
      n_predicted = n()
    )
  
  matched_counts = cbind(observed_predicted_match_counts,predicted_observed_match_counts)
  matched_counts$height_cat = paste0(min_height,"+")
  
  return(matched_counts)
  
}

calc_match_stats <- function(observed_trees,
                             predicted_trees,
                             predicted_tree_dataset_name,
                             canopy_position,
                             output_dir) {
  
  observed_trees_simple <- observed_trees %>%
    dplyr::select(observed_tree_id,
                  final_predicted_tree_match_id,
                  observed_tree_height = Height,
                  observed_tree_internal_area = internal_area
    ) %>%
    mutate(observed_tree_internal_area = as.vector(observed_tree_internal_area)) %>%
    mutate(final_predicted_tree_match_id = as.numeric(final_predicted_tree_match_id)) #TODO: fix this type inconsistency further upstream. Where is ID getting stored as a character?
  
  predicted_trees_simple <- predicted_trees %>%
    dplyr::select(predicted_tree_id,
                  predicted_tree_height = height,
                  predicted_tree_internal_area = internal_area
    ) %>%
    mutate(predicted_tree_internal_area = as.vector(predicted_tree_internal_area))
  
  # For each predicted tree, get the attributes of the observed tree it was matched to.
  # Same for observed trees matched to predicted trees (two separate tables).
  # It is necessary to have these two separate tables because when assessing
  # sensitivity (recall), we need to check how many of the observed trees *within the internal negative buffered area* match to predicted trees *regardless of whether they're in the internal negative buffered area*,
  # and similarly for precision, we need to check how many of the predicted trees *within the internal negative buffered area* match to observed trees *regardless of whether they're in the internal negative buffered area*,
  # so the sets of trees used for each is different.
  # Before joining observed and predicted trees, remove geometry (convert to regular data frame) because coordinates are not necessary,
  # and two spatial data frames with different geometry cannot be left_joined as it is ambiguous which one to take the coordinates from
  st_geometry(observed_trees_simple) <- NULL
  st_geometry(predicted_trees_simple) <- NULL
  
  predicted_observed_match <- left_join(predicted_trees_simple,
                                        observed_trees_simple,
                                        by = c("predicted_tree_id" = "final_predicted_tree_match_id")
  )
  observed_predicted_match <- right_join(predicted_trees_simple,
                                         observed_trees_simple,
                                         by = c("predicted_tree_id" = "final_predicted_tree_match_id")
  )
  
  
  # Sum the tree counts (number of predicted trees, number of predicted trees matched, number of observed trees, and number of observed trees matched)
  # across height classes
  
  over10_match = count_total_and_matched_trees(observed_predicted_match, predicted_observed_match, min_height = 10)
  over20_match = count_total_and_matched_trees(observed_predicted_match, predicted_observed_match, min_height = 20)
  
  match_stats = bind_rows(over10_match, over20_match)
  
  # get the height difference of the matched trees
  trees_matched <- observed_predicted_match %>%
    filter(!is.na(predicted_tree_id)) %>%
    mutate(height_err = predicted_tree_height - observed_tree_height)
  
  output_dir = paste0(output_dir, "/matched_tree_lists/")
  output_file = paste0("trees_matched_",predicted_tree_dataset_name,"_",canopy_position,".csv")
  
  write_csv_to_dir(dataframe = trees_matched,
                   output_dir = output_dir,
                   output_file = output_file)
  
  
  # Get height accuracy metrics based on a table of heights of the observed trees and the heights of the matched predicted trees
  height_metrics_over10 <- height_accuracy_metrics(trees_matched = trees_matched %>% filter(observed_tree_internal_area == TRUE), min_height = 10)
  height_metrics_over20 <- height_accuracy_metrics(trees_matched = trees_matched %>% filter(observed_tree_internal_area == TRUE), min_height = 20)
  
  # Bind the two height categories and compute height MAE for both
  height_stats <- bind_rows(height_metrics_over10, height_metrics_over20) %>%
    mutate(height_mae_percent = height_mae / height_mean_observed)
  
  # Compute sensitivity, precision, f_score for individual tree detection
  match_stats <- match_stats %>%
    mutate(sensitivity = n_observed_matched_predicted / n_observed,
           precision = n_predicted_matched_observed / n_predicted) %>%
    mutate(f_score = 2 * sensitivity * precision / (sensitivity + precision))
  
  # Combine individual tree detection and height accuracy
  match_stats <- full_join(match_stats, height_stats, by = "height_cat")
  match_stats$canopy_position <- canopy_position
  
  return(match_stats)
}



# Function reads the output location from stem map comparison
# Reads the output files and calculates individual tree statistics
# Saves the output as CSV in 'interim_files' folder

tree_det_stats <- function(tmp_dir,
                           predicted_trees_filepath,
                           output_dir) {
  
  # Create output and temp dirs if they don't exist
  dir.create(tmp_dir)
  dir.create(output_dir)
  
  predicted_trees <- st_read(predicted_trees_filepath)
  
  predicted_tree_dataset_name <-
    tools::file_path_sans_ext(basename(predicted_trees_filepath))
  
  # Collect the predicted tree GPKGs (located in the interim files location) from
  # stem map comparison for all and overstory tree category (should be two files)
  map_files <- list.files(tmp_dir,
                          full.names = TRUE, pattern = paste0(".*", predicted_tree_dataset_name,".*.gpkg$")
  )
  
  match_stats_i <- list()
  
  # This function reads each GPKG and calculates height error
  # for each matched tree and saves the CSV
  
  ind_tree_match_stats_func <- function(i) {
    matched_observed_trees <- st_read(i)
    
    map_file_canopy_position <- sub(
      ".*_(.*)", "\\1",
      tools::file_path_sans_ext(basename(i))
    )
    
    calc_match_stats(matched_observed_trees,
                     predicted_trees,
                     predicted_tree_dataset_name = predicted_tree_dataset_name,
                     canopy_position = map_file_canopy_position,
                     output_dir
    )
  }
  
  # 'ind_tree_match_stats_func' function returns a summary of the
  # individual tree stats i.e., F-score, sensitivity, precision etc
  # for both all and overstory tree position
  match_stats_i <- lapply(map_files, ind_tree_match_stats_func)
  
  # Combine both all and overstory tree stats into one table
  # and writes out CSV for further processing
  match_stats <- bind_rows(match_stats_i)
  write.csv(
    match_stats,
    file.path(
      tmp_dir,
      paste0(predicted_tree_dataset_name, "_match_stats.csv")
    )
  )
  
}