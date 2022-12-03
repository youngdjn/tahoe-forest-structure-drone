# R functions used when calculating individual tree detection
# statistics after comparing Tree Approximate Objects with stem
# map ground truth data.

# Constant:
sq_m_per_ha = 10000

area_based_based_comparison <- function(observed_trees,
                                        predicted_trees,
                                        focal_region_polygon,
                                        virtual_plot_size) {
  
  ## Make a spatial grid of contiguous square "virtual plots" of the specified size within the focal region (where we have both observed and predicted trees) and assign the trees to the plot they fall within 
  
  # Make the grid (it will include grid cells on the edge that extend beyond the focal region polygon)
  grid <- st_make_grid(focal_region_polygon, cellsize = virtual_plot_size)
  # Find which grid cells are fully within the focal region polygon
  grid_inside <- st_contains(focal_region_polygon, grid, sparse = FALSE)[1,]
  # Keep only the grid cells that are within the focal region polygon
  grid_full <- grid[grid_inside]
  # Make it able to hold attributes (data columns)
  grid_full <- st_sf(grid_full)
  # Assign grid cell IDs
  grid_full$grid_id <- 1:nrow(grid_full)
  # Assign each tree the ID of the grid cell ("virtual plot") it falls within
  observed_trees <- st_intersection(observed_trees, grid_full)
  predicted_trees <- st_intersection(predicted_trees, grid_full)
  
  observed_trees_simple <- observed_trees %>%
    dplyr::select(observed_tree_id,
                  final_predicted_tree_match_id,
                  height = Height,
                  grid_id
    )
  predicted_trees_simple <- predicted_trees %>%
    dplyr::select(predicted_tree_id,
                  height = height,
                  predicted_tree_internal_area = internal_area,
                  grid_id
    )
  st_geometry(observed_trees_simple) <- NULL
  st_geometry(predicted_trees_simple) <- NULL
  predicted_observed_match <- bind_rows(predicted_trees_simple, observed_trees_simple)
  
  ### within each grid cell, compute number of trees by each size class
  
  predicted_observed_match_over10 <- predicted_observed_match %>%
    filter(height > 10) %>%
    mutate(height_cat = "10+")
  
  predicted_observed_match_over20 <- predicted_observed_match %>%
    filter(height > 20) %>%
    mutate(height_cat = "20+")
  
  predicted_observed_match <- bind_rows(
    predicted_observed_match_over10,
    predicted_observed_match_over20
  )
  
  density_by_cell <- predicted_observed_match %>%
    group_by(grid_id, height_cat) %>%
    # convert trees per virtual plot to trees per hectare (virtual plot size is in meters and plots are square)
    summarize(
      density_observed =
        sum(!is.na(observed_tree_id)) / (virtual_plot_size^2 / sq_m_per_ha),
      density_predicted =
        sum(!is.na(predicted_tree_id)) / (virtual_plot_size^2 / sq_m_per_ha)
    ) %>%
    
    mutate(
      error_abs = density_predicted - density_observed,  # This is not absolute value, but simply the raw different (positive or negative) between predicted and observed density
      error_pct = (density_predicted - density_observed) / density_observed
    )
  
  density_summary <- density_by_cell %>%
    group_by(height_cat) %>%
    summarize(
      mean_abs_err = mean(abs(error_abs)),
      mean_bias = mean(error_abs),
      mean_observed_treedensity = mean(density_observed),
      mean_predicted_treedensity = mean(density_predicted),
      correlation = cor(density_observed, density_predicted),
      n_virtual_plots = n()
    ) %>%
    mutate(mean_abs_err_pct = mean_abs_err / mean_observed_treedensity)
  
  
  return(density_summary)
}







# Function reads the location of the ground truth (
# ground_map_file) and input TAO dataset (drone_map_file)
# Takes in input from user whether the ground truth dataset has
# identification of trees as dominant or understory (additional_overstory_comparison)
# Reads user input on the size of the virtual plot in metres.
# Calculates the area based statistics
# Saves the output as CSV in 'interim_files' folder

area_based_stats <- function(observed_trees_filepath,  # The ground-reference stem map (gpkg) that has been prepped by the `prep_tree_maps` function
                             predicted_trees_filepath, # The predicted trees map (gpkg) that has been prepped by the `prep_tree_maps` function
                             focal_region_polygon_filepath,    # The focal region within which to compare observed and predicted tree densities
                             virtual_plot_size,                # The size (sides of a square, in m) of plots that we impose on the ground to compute area-based stats in comparing the ground map and the tree map
                             additional_overstory_comparison,               # Boolean for whether or not the prepped observed tree map gpkg contains an attribute "under_neighbor" to indicate whether a tree is overstory or understory (added by the prep_tree_maps function based on tree proximity and relative height)
                             tmp_dir) {
    # read in data files
    observed_trees <- st_read(observed_trees_filepath)
    predicted_trees <- st_read(predicted_trees_filepath)
    predicted_tree_dataset_name <- tools::file_path_sans_ext(
        basename(predicted_trees_filepath)
    )

    focal_region_polygon <- st_read(focal_region_polygon_filepath) %>%
        st_transform(st_crs(observed_trees))

    # Run compare_tree_maps functions based on the tree category
    # information for validation dataset

    area_based_stats_all <- area_based_based_comparison(
      observed_trees,
      predicted_trees,
      focal_region_polygon,
      virtual_plot_size
    )
    
    area_based_stats_all$canopy_position <- "all"
    
    
    if (additional_overstory_comparison) {
      
      area_based_stats_overstory <- area_based_based_comparison(
            observed_trees %>% filter(under_neighbor == FALSE),
            predicted_trees,
            focal_region_polygon,
            virtual_plot_size
        )
        area_based_stats_overstory$canopy_position <- "overstory"
        
        area_based_stats <- bind_rows(area_based_stats_all, area_based_stats_overstory)
    } else {
      area_based_stats = area_based_stats_all
    }
        
    # save outputs
    write.csv(
        area_based_stats,
        file.path(
            tmp_dir,
            paste0(predicted_tree_dataset_name, "_areabased_stats.csv")
        )
    )
 
}
