# Function that draws lines connecting the matched trees in ground
# truth and TAO dataset
connect_matched_trees <- function(observed_trees,
                                  predicted_trees,
                                  predicted_tree_dataset_name,
                                  output_dir) {
  matched_observed_trees <-
    observed_trees[!is.na(observed_trees$final_predicted_tree_match_id), ]
  
  predicted_trees_indexes <- match(
    matched_observed_trees$final_predicted_tree_match_id,
    predicted_trees$predicted_tree_id
  )
  

  matched_predicted_trees <- predicted_trees[predicted_trees_indexes, ]
  
  lines <- list()
  
  for (i in 1:nrow(matched_observed_trees)) {
    observed <- matched_observed_trees[i, ]
    predicted <- matched_predicted_trees[i, ]
    both <- st_union(observed, predicted)
    line <- st_cast(both, "LINESTRING")
    
    lines[[i]] <- line
  }
  
  lines_df <- do.call("rbind", lines)
  

  lines_df <- lines_df %>%
    dplyr::select(observed_tree_id, predicted_tree_id)
  
  output_dir <- paste0(
    output_dir, "/predicted_observed_pairing_lines/"
  )
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = T)
  
  lines_with_predicted_crs <- st_transform(lines_df, st_crs(predicted_trees))
  output_geojson_path <- sprintf(
    "%s/pairings_%s.gpkg", output_dir, predicted_tree_dataset_name
  )
  st_write(lines_with_predicted_crs, output_geojson_path, delete_dsn = T, quiet = T)
}




# Function reads the output location from stem map comparison
# Reads the output files and creates vector files of lines connecting the predicted-observed tree pairs
# Saves the output as gpkg in 'predicted_observed_pairing_lines' in output folder

connect_matches <- function(tmp_dir,
                            predicted_trees_filepath,
                            output_dir) {
  
    predicted_trees <- st_read(predicted_trees_filepath)

    predicted_tree_dataset_name <-
        tools::file_path_sans_ext(basename(predicted_trees_filepath))

    # Collect the GPKGs (located in the interim files location) from
    # stem map comparison for all and overstory tree category

    map_files <- list.files(tmp_dir,
        full.names = TRUE, pattern = "*.gpkg$"
    )


    ## creates vector files of lines connecting the predicted-observed tree pairs

    connect_matched_trees_call <- function(i) {
        matched_observed_trees <- st_read(i)

        map_file_canopy_position <- sub(
            ".*_(.*)", "\\1",
            tools::file_path_sans_ext(basename(i))
        )

        connect_matched_trees(
            matched_observed_trees,
            predicted_trees,
            paste0(predicted_tree_dataset_name, "_", map_file_canopy_position),
            output_dir
        )
    }

    lapply(map_files, connect_matched_trees_call)
    
    return(TRUE)
}
