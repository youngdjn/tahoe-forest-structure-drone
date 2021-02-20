## Use the shift amounts and directions learned from the manual ground stem map alignment (check_reference_stem_map_alignment.R) to shift all trees in ground map

library(tidyverse)
library(sf)
library(here)


#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))



#### Comparison ####


## For each ground-truth tree, get the closest drone tree within 5 m distance and 2 m height

get_closest_tree = function(ground_tree_index, ground_map, drone_map, dist_mat) {

  ground_tree = ground_map[ground_tree_index,]
  ground_tree$drone_map_match_id = NA
  ground_tree$drone_map_match_distance = NA
  
  drone_map$distance_to_ground_tree = dist_mat[ground_tree_index,] %>% as.numeric
  
  # if we already matched that ground tree
  if(!is.na(ground_tree$final_drone_map_match_id)) return(ground_tree)
  
  # thin drone map to trees within size bounds
  lwr = ground_tree$Height-ground_tree$Height*search_height_proportion
  upr = ground_tree$Height+ground_tree$Height*search_height_proportion
  drone_map_candidates = drone_map[between(drone_map$height,lwr,upr),]

  if(nrow(drone_map_candidates) == 0) return(ground_tree)
  
  # thin to those within distance
  drone_map_candidates = drone_map_candidates %>%
    filter(distance_to_ground_tree < search_distance_fun(ground_tree$Height))
  
  if(nrow(drone_map_candidates) == 0) return(ground_tree)
  
  # take the closest
  drone_map_match = drone_map_candidates[which(drone_map_candidates$distance_to_ground_tree == min(drone_map_candidates$distance_to_ground_tree)),]

  ground_tree$drone_map_match_id = drone_map_match$drone_tree_id
  ground_tree$drone_map_match_distance = drone_map_match$distance_to_ground_tree
  
  return(ground_tree)
}


## apply to all ground trees

get_closest_matches = function(ground_map, drone_map, drone_trees_exclude) {

  # ground_map = ground_map %>%
  #   mutate(drone_map_match_id = NA,
  #          drone_map_match_distance = NA) %>%
  #   select(-geometry,geometry) # get the geometry column to the end
  
  drone_map_dropexclude = drone_map %>%
    filter(!(drone_tree_id %in% drone_trees_exclude))
  
  dist_mat = st_distance(ground_map, drone_map_dropexclude)
  
  ground_map_nonsp = ground_map
  st_geometry(ground_map_nonsp) = NULL
  
  drone_map_nonsp = drone_map_dropexclude
  st_geometry(drone_map_nonsp) = NULL
  
  a = map_dfr(1:nrow(ground_map) , get_closest_tree, ground_map = ground_map_nonsp, drone_map = drone_map_nonsp, dist_mat = dist_mat)

  a = a %>%
    select(ground_tree_id, drone_map_match_id, drone_map_match_distance)
  
  ground_map = left_join(ground_map,a, by="ground_tree_id")

}


### Look for drone trees claimed by multiple ground trees
## and assign it to the ground tree it's closest to

remove_overmatched_drone_trees = function(ground_map) {
  
  ground_summ = ground_map %>%
    filter(!is.na(drone_map_match_id)) %>%
    group_by(drone_map_match_id) %>%
    summarize(n = n(),
              closest_dist = min(drone_map_match_distance))
  st_geometry(ground_summ) = NULL
  
  
  ## bind the min distances to the ground map
  ground_map = left_join(ground_map,ground_summ, by="drone_map_match_id")
  
  ## keep the match if "drone map distance" == "closest distance"
  # figure out which they are to keep
  keep_match_indexes = which(ground_map$drone_map_match_distance == ground_map$closest_dist)
  
  # store keeper matches in a new column
  ground_map[keep_match_indexes,"final_drone_map_match_id"] = ground_map[keep_match_indexes,]$drone_map_match_id
  
  ## clear the temporary columns
  ground_map = ground_map %>%
    select(-drone_map_match_id,
           -drone_map_match_distance,
           -n,
           -closest_dist)
  
  return(ground_map)
}


## Run the matching

compare_tree_maps = function(ground_map, drone_map) {
  
  ground_map = get_closest_matches(ground_map, drone_map = drone_map, drone_trees_exclude = NULL)
  ground_map = remove_overmatched_drone_trees(ground_map)
  
  # get the matches that are off-limits (already matched)
  drone_trees_matched = unique(ground_map$final_drone_map_match_id)
  cat("Drone trees matched (first pass):", length(drone_trees_matched), "\n")
  ground_map = get_closest_matches(ground_map, drone_map = drone_map, drone_trees_exclude = drone_trees_matched)
  ground_map = remove_overmatched_drone_trees(ground_map)
  
  ###!!! might not need this additional loop
  
  # get the matches that are off-limits (already matched)
  drone_trees_matched = unique(ground_map$final_drone_map_match_id)
  cat("Drone trees matched (second pass):", length(drone_trees_matched), "\n")
  
  ground_map = get_closest_matches(ground_map, drone_map = drone_map, drone_trees_exclude = drone_trees_matched)
  ground_map = remove_overmatched_drone_trees(ground_map)
  
  drone_trees_matched = unique(ground_map$final_drone_map_match_id)
  cat("Drone trees matched (third pass):", length(drone_trees_matched), "\n")
  
  ground_map = get_closest_matches(ground_map, drone_map = drone_map, drone_trees_exclude = drone_trees_matched)
  ground_map = remove_overmatched_drone_trees(ground_map)
  
  drone_trees_matched = unique(ground_map$final_drone_map_match_id)
  cat("Drone trees matched (fourth pass):", length(drone_trees_matched), "\n")
  
  return(ground_map)

}



#### Prep data ####

prep_data = function(ground_map, drone_map, reduced_area) {
  
  # assign tree IDs
  drone_map$drone_tree_id = 1:nrow(drone_map)
  
  # Clip to extent of ground map (buffered by search radius)
  # formerly: ground_map_footprint = ground_map %>% st_union %>% st_convex_hull
  if(reduced_area) {
    ground_map_footprint = st_read(data("study_area_perimeter/smaller_project_mask.geojson")) %>% st_transform(st_crs(drone_map))
  } else {
      ground_map_footprint = st_read(data("study_area_perimeter/ground_map_mask_precise.geojson")) %>% st_transform(st_crs(drone_map))
  }

  drone_map = st_intersection(drone_map,ground_map_footprint %>% st_buffer(search_distance) )
  
  # Need a label to know if drone map trees were part of a buffered-in polygon
  ground_map_footprint_bufferin = ground_map_footprint %>% st_buffer(-search_distance*2)
  drone_map$internal_area = st_intersects(drone_map,ground_map_footprint_bufferin, sparse=FALSE)
  
  # Need a label to know if ground map trees were within the focal polygon
  ground_map$internal_area = st_intersects(ground_map,ground_map_footprint_bufferin, sparse=FALSE)
  
  return(list(ground_map = ground_map, drone_map = drone_map))
}



get_slope = function(y,x) {
  m = lm(y~x)
  slope = coef(m)[2]
  return(slope)
}


#### Compute correspondence statistics ####

## which ground trees are matched to a drone tree?
# ground_map$final_drone_map_match_id

## which drone trees are matched to a ground tree?
# get which ground tree they're matched to
calc_match_stats = function(ground_map, drone_map) {
  
  ground_map_simple = ground_map %>%
    select(ground_tree_id, final_drone_map_match_id, ground_tree_height = Height, ground_tree_internal_area = internal_area) %>%
    mutate(ground_tree_internal_area = as.vector(ground_tree_internal_area))
  drone_map_simple = drone_map %>%
    select(drone_tree_id, drone_tree_height = height, drone_tree_internal_area = internal_area) %>%
    mutate(drone_tree_internal_area = as.vector(drone_tree_internal_area))

  
  st_geometry(ground_map_simple) = NULL
  st_geometry(drone_map_simple) = NULL
  drone_ground_match = left_join(drone_map_simple, ground_map_simple, by = c("drone_tree_id"="final_drone_map_match_id"))
  ground_drone_match = right_join(drone_map_simple, ground_map_simple, by = c("drone_tree_id"="final_drone_map_match_id"))
  
  ## Counts of ground trees trees matched to drone trees, by size classes
  ground_drone_match = ground_drone_match %>%
    filter(ground_tree_internal_area == TRUE) %>% # make sure it's internal to the buffer of ground trees
    mutate(height_cat = cut(ground_tree_height,breaks = c(-Inf,5,10,20,30,40,Inf), labels = c("0-5","5-10","10-20","20-30","30-40","40+")))
  
  drone_ground_match = drone_ground_match %>%
    filter(drone_tree_internal_area == TRUE) %>% # make sure it's internal to the buffer of ground trees
    mutate(height_cat = cut(drone_tree_height,breaks = c(-Inf,5,10,20,30,40,Inf), labels = c("0-5","5-10","10-20","20-30","30-40","40+")))
  
  ground_drone_match_stats = ground_drone_match %>%
    group_by(height_cat) %>%
    summarize(n_ground_matched_drone = sum(!is.na(drone_tree_id) & !is.na(ground_tree_id)),
              n_ground = n())
    
  drone_ground_match_stats = drone_ground_match %>%
    group_by(height_cat) %>%
    summarize(n_drone_matched_ground = sum(!is.na(drone_tree_id) & !is.na(ground_tree_id)),
              n_drone = n())
  
  match_stats = full_join(ground_drone_match_stats, drone_ground_match_stats, by = "height_cat")
  
  over10_match = match_stats %>%
    filter(height_cat != "0-5" & height_cat != "5-10") %>%
    summarize_at(vars(-height_cat),sum) %>%
    mutate(height_cat = "10+")
  
  over20_match = match_stats %>%
    filter(height_cat != "0-5" & height_cat != "5-10" & height_cat != "10-20") %>%
    summarize_at(vars(-height_cat),sum) %>%
    mutate(height_cat = "20+")
  
  over30_match = match_stats %>%
    filter(height_cat != "0-5" & height_cat != "5-10" & height_cat != "10-20" & height_cat != "20-30") %>%
    summarize_at(vars(-height_cat),sum) %>%
    mutate(height_cat = "30+")
  
  over40_match = match_stats %>%
    filter(height_cat == "40+") %>%
    summarize_at(vars(-height_cat),sum) %>%
    mutate(height_cat = "40+")
  
  match_stats = bind_rows(over10_match,over20_match,over30_match,over40_match)
  
  # get the height difference of the matched trees
  trees_matched = ground_drone_match %>%
    filter(!is.na(drone_tree_id)) %>%
    mutate(height_err = drone_tree_height - ground_tree_height)
  
  over10trees = trees_matched %>%
    filter(ground_tree_height >= 10) %>%
    summarize(height_mae = mean(abs(height_err)),
              height_bias = mean(height_err),
              height_mean_ground = mean(ground_tree_height),
              height_mean_drone = mean(drone_tree_height),
              height_cor = cor(ground_tree_height,drone_tree_height),
              height_slope = get_slope(ground_tree_height,drone_tree_height)) %>%
    mutate(height_cat = "10+")
  
  over20trees = trees_matched %>%
    filter(ground_tree_height >= 20) %>%
    summarize(height_mae = mean(abs(height_err)),
              height_bias = mean(height_err),
              height_mean_ground = mean(ground_tree_height),
              height_mean_drone = mean(drone_tree_height),
              height_cor = cor(ground_tree_height,drone_tree_height),
              height_slope = get_slope(ground_tree_height,drone_tree_height)) %>%
    mutate(height_cat = "20+")
  
  over30trees = trees_matched %>%
    filter(ground_tree_height >= 30) %>%
    summarize(height_mae = mean(abs(height_err)),
              height_bias = mean(height_err),
              height_mean_ground = mean(ground_tree_height),
              height_mean_drone = mean(drone_tree_height),
              height_cor = cor(ground_tree_height,drone_tree_height),
              height_slope = get_slope(ground_tree_height,drone_tree_height)) %>%
    mutate(height_cat = "30+")
  
  over40trees = trees_matched %>%
    filter(ground_tree_height >= 40) %>%
    summarize(height_mae = mean(abs(height_err)),
              height_bias = mean(height_err),
              height_mean_ground = mean(ground_tree_height),
              height_mean_drone = mean(drone_tree_height),
              height_cor = cor(ground_tree_height,drone_tree_height),
              height_slope = get_slope(ground_tree_height,drone_tree_height)) %>%
    mutate(height_cat = "40+")
  
  height_stats = bind_rows(over10trees,over20trees,over30trees,over40trees) %>%
    mutate(height_mae_percent = height_mae / height_mean_ground)
  

  
  match_stats = match_stats %>%
    mutate(sensitivity = n_ground_matched_drone/n_ground,
           precision = n_drone_matched_ground/n_drone) %>%
    mutate(f_score = 2*sensitivity*precision/(sensitivity+precision))
  
  match_stats = full_join(match_stats,height_stats, by = "height_cat")

  return(match_stats)
}


plot_based_comparison = function(prepped_ground, prepped_drone) {
  
  # formerly: ground_hull = prepped_ground %>% st_union %>% st_convex_hull %>% st_buffer(-25)
  ground_hull = st_read(data("study_area_perimeter/ground_map_mask_precise.geojson")) %>% st_transform(st_crs(prepped_ground)) %>% st_buffer(-25)
  
  grid = st_make_grid(ground_hull, cellsize=25)
  grid_inside = st_contains(ground_hull,grid, sparse=FALSE)
  grid_full = grid[grid_inside[1,]]
  grid_full = st_sf(grid_full)
  grid_full$grid_id = 1:nrow(grid_full)
  prepped_ground = st_intersection(prepped_ground,grid_full)
  prepped_drone = st_intersection(prepped_drone,grid_full)
  
  ground_map_simple = prepped_ground %>%
    select(ground_tree_id, final_drone_map_match_id, height = Height, grid_id)
  drone_map_simple = prepped_drone %>%
    select(drone_tree_id, height = height, drone_tree_internal_area = internal_area, grid_id)
  st_geometry(ground_map_simple) = NULL
  st_geometry(drone_map_simple) = NULL
  drone_ground_match = bind_rows(drone_map_simple, ground_map_simple)
  ##!! note this does not actually join, just rbinds, because the trees have not been matched yet, but that's ok
  
  ### within each grid cell, compute number of trees by each size class
  
  drone_ground_match_over10 = drone_ground_match %>%
    filter(height > 10) %>%
    mutate(height_cat = "10+")
    
  drone_ground_match_over20 = drone_ground_match %>%
    filter(height > 20) %>%
    mutate(height_cat = "20+")
  
  drone_ground_match_over30 = drone_ground_match %>%
    filter(height > 30) %>%
    mutate(height_cat = "30+")
  
  drone_ground_match_over40 = drone_ground_match %>%
    filter(height > 40) %>%
    mutate(height_cat = "40+")
  
  drone_ground_match = bind_rows(drone_ground_match_over10,
                                 drone_ground_match_over20,
                                 drone_ground_match_over30,
                                 drone_ground_match_over40)
  
  
  density_by_cell = drone_ground_match %>%
    group_by(grid_id, height_cat) %>%
    summarize(density_ground = sum(!is.na(ground_tree_id)),
              density_drone = sum(!is.na(drone_tree_id))) %>%
    mutate(error_abs = density_drone-density_ground,
           error_pct =(density_drone-density_ground)/density_ground)
  
  density_summary = density_by_cell %>%
    group_by(height_cat) %>%
    summarize(mean_abs_err = mean(abs(error_abs)),
              mean_bias = mean(error_abs),
              mean_ground_trees = mean(density_ground),
              mean_drone_trees = mean(density_drone),
              correlation = cor(density_ground,density_drone)) %>%
    mutate(mean_abs_err_pct = mean_abs_err/mean_ground_trees)
    
  
  return(density_summary)
}



#### BONUS: make lines connecting the pairs ####
make_lines_between_matches = function(ground_map, drone_map, drone_map_name) {
  
  matched_ground_trees = ground_map[!is.na(ground_map$final_drone_map_match_id),]
  
  drone_map_indexes = match(matched_ground_trees$final_drone_map_match_id, drone_map$drone_tree_id)
  
  matched_drone_trees = drone_map[drone_map_indexes,]
  
  
  lines = list()
  
  for(i in 1:nrow(matched_ground_trees)) {
    
    ground = matched_ground_trees[i,]
    drone = matched_drone_trees[i,]
    both = st_union(ground,drone)
    line = st_cast(both,"LINESTRING")
    
    lines[[i]] = line
    
  }
  
  lines_df = do.call("rbind",lines)
  
  lines_df = lines_df %>%
    select(ground_tree_id,drone_tree_id)
  
  st_write(lines_df %>% st_transform(4326),data(paste0("drone_map_evals/stem_map_pairing_lines/pairings_", drone_map_name, ".geojson")), delete_dsn = TRUE, quiet=TRUE)
  
}



## Function to match and compare a single drone map to the ground map (also make a lines shapefile with connections between trees)
match_compare_single = function(data_prepped, drone_map_name) {
  
  cat("Comparing to ground map:",drone_map_name,"\n")
  
  # need to skip one CHM that is bad (no real trees in it)
  if(str_detect(drone_map_name,fixed("paramset27b_15215"))) {
    return(FALSE)
  }

  ground_map_compared = compare_tree_maps(data_prepped$ground_map, data_prepped$drone_map)
  match_stats_alltrees = calc_match_stats(ground_map_compared,data_prepped$drone_map)
  match_stats_alltrees$tree_position = "all"
  
  ## make a shapefile of lines connecting the drone-ground tree pairs
  #make_lines_between_matches(ground_map_compared, data_prepped$drone_map, paste0(drone_map_name,"_all"))
  
  ground_map_compared = compare_tree_maps(data_prepped$ground_map  %>% filter(under_neighbor == FALSE), data_prepped$drone_map)
  match_stats_singletrees = calc_match_stats(ground_map_compared,data_prepped$drone_map)
  match_stats_singletrees$tree_position = "single"
  
  ## make a shapefile of lines connecting the drone-ground tree pairs
  #make_lines_between_matches(ground_map_compared, data_prepped$drone_map, paste0(drone_map_name,"_single"))
  
  plot_stats_alltrees = plot_based_comparison(prepped_ground = data_prepped$ground_map, prepped_drone = data_prepped$drone_map)
  plot_stats_alltrees$tree_position = "all"
  
  plot_stats_singletrees = plot_based_comparison(prepped_ground = data_prepped$ground_map  %>% filter(under_neighbor == FALSE), prepped_drone = data_prepped$drone_map)
  plot_stats_singletrees$tree_position = "single"
  
  match_stats = bind_rows(match_stats_alltrees, match_stats_singletrees)
  plot_stats = bind_rows(plot_stats_alltrees, plot_stats_singletrees)
  
  match_stats = left_join(match_stats,plot_stats,by=c("height_cat","tree_position")) %>%
    select(tree_position, height_cat, everything())
  
  match_stats$drone_map_name = drone_map_name
  
  match_stats = match_stats %>%
    mutate_if(is.numeric, round, digits=3)
  
  write_csv(match_stats, data(paste0("drone_map_evals/individual/stats_", drone_map_name, ".csv")))
  
  
  


}




match_compare_single_wrapper = function(ground_map, drone_map_file) {
  
  #cat("running for", drone_map_file, "\n")
  
  ## get drone map name from filename
  parts = str_split(drone_map_file,"/")[[1]]
  filename = parts[length(parts)]
  drone_map_name = str_split(filename,"\\.")[[1]][1]
  
  ## what would the output file be? So that we can skip if it exists 
  outfile = data(paste0("drone_map_evals/individual/stats_", drone_map_name, ".csv"))
  
  if(file.exists(outfile)) {  
    cat("Already exists:",outfile,". Skipping.\n")
    return(FALSE)
  }

  
  drone_map = st_read(drone_map_file, quiet=TRUE) %>% st_transform(3310)
  
  #### For the reduced-area drone surveys, clip the ground map to the smaller mask
  
  ## projects applicable: 14b, 15b, 19b, 20b, 
  reduced_area = grepl("set14b_|set15b_|set19b_|set20b_|set41_|set42_", drone_map_file)
  if(reduced_area) {
    cat("Using reduced-area polygon for",drone_map_file,"\n")
    smaller_project_mask = st_read(data("study_area_perimeter/smaller_project_mask.geojson")) %>% st_transform(st_crs(ground_map))
    smaller_project_mask = smaller_project_mask %>% st_buffer(2*search_distance)
    ground_map_new = st_intersection(ground_map,smaller_project_mask)
  } else{
    ground_map_new = ground_map
  }
  
  #### Filter drone map data to only trees within the height search distance of the smallest size category
  drone_map = drone_map %>%
    filter(height >= (smallest_size-smallest_size*search_height_proportion))
  
  if(nrow(drone_map) == 0) {
    cat("Drone map", drone_map_file, "has zero trees after height filtering; skipping\n")
    return(FALSE)
  }
  
  #### Prep the maps by cropping etc
  data_prepped = prep_data(ground_map_new, drone_map, reduced_area = reduced_area) # takes about 1 min
  
  ### Of the trees > 10 m tall, If the drone map has > 5x as many trees as the ground map, or < 1/10, skip it
  n_drone_trees = nrow(data_prepped$drone_map %>% filter(height > 10, internal_area == TRUE))
  n_ground_trees = nrow(data_prepped$ground_map %>% filter(Height > 10, internal_area == TRUE))
  if((n_drone_trees > 5*n_ground_trees) | (n_drone_trees < 0.1*n_ground_trees)) {
    return(FALSE)
  }
  
  ## Run comparison/eval ##
  match_compare_single(data_prepped, drone_map_name = drone_map_name)

  return(TRUE)
  
}

