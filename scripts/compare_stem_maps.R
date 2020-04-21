## Use the shift amounts and directions learned from the manual ground stem map alignment (check_reference_stem_map_alignment.R) to shift all trees in ground map

data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(sf)
library(here)


#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))


#### Load and clean data ####

ground_map = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson")) %>% filter(Height > 5) %>% st_transform(3310)
ground_map$ground_tree_id = 1:nrow(ground_map)
ground_map$final_drone_map_match_id = NA


#### Constants ####

search_distance = 5
search_height = 10




#### Data to test with ####

drone_map = st_read(data("reference_drone_stem_map/treetops_vwf001.geojson")) %>% st_transform(3310)

# assign tree IDs
drone_map$drone_tree_id = 1:nrow(drone_map)

# Clip to extent of ground map (buffered by search radius)
ground_map_footprint = ground_map %>% st_union %>% st_buffer(search_distance)
drone_map_clip = st_intersection(drone_map,ground_map_footprint)



#### Comparison ####


## For each ground-truth tree, get the closest drone tree within 5 m distance and 2 m height

get_closest_tree = function(ground_tree_index, ground_map, drone_map_clip) {
  
  ground_tree = ground_map[ground_tree_index,]
  ground_tree$drone_map_match_id = NA
  ground_tree$drone_map_match_distance = NA
  
  # if we already matched that ground tree
  if(!is.na(ground_tree$final_drone_map_match_id)) return(ground_tree)
  
  # thin drone map to trees within size bounds
  drone_map_candidates = drone_map_clip %>%
    filter(height %>% between(ground_tree$Height-search_height,ground_tree$Height+search_height))
  
  if(nrow(drone_map_candidates) == 0) return(ground_tree)
  
  # get distance to each
  drone_map_candidates$distance = st_distance(ground_tree, drone_map_candidates)[1,] %>% as.numeric()
  
  # thin to those within distance
  drone_map_candidates = drone_map_candidates %>%
    filter(distance < search_distance)
  
  if(nrow(drone_map_candidates) == 0) return(ground_tree)
  
  # take the closest
  drone_map_match = drone_map_candidates[which(drone_map_candidates$distance == min(drone_map_candidates$distance)),]

  ground_tree$drone_map_match_id = drone_map_match$drone_tree_id
  ground_tree$drone_map_match_distance = drone_map_match$distance
  
  return(ground_tree)
  
}


## apply to all ground trees

get_closest_matches = function(ground_map, drone_map_clip, drone_trees_exclude) {

  # ground_map = ground_map %>%
  #   mutate(drone_map_match_id = NA,
  #          drone_map_match_distance = NA) %>%
  #   select(-geometry,geometry) # get the geometry column to the end
  
  drone_map_clip_dropexclude = drone_map_clip %>%
    filter(!(drone_tree_id %in% drone_trees_exclude))
  
  a = map(1:nrow(ground_map) , get_closest_tree, ground_map = ground_map, drone_map_clip = drone_map_clip_dropexclude)

  ground_map = do.call(rbind,a)

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



ground_map = get_closest_matches(ground_map, drone_map_clip = drone_map_clip, drone_trees_exclude = NULL)
ground_map = remove_overmatched_drone_trees(ground_map)

# get the matches that are off-limits (already matched)
drone_trees_matched = unique(ground_map$final_drone_map_match_id)
ground_map = get_closest_matches(ground_map, drone_map_clip = drone_map_clip, drone_trees_exclude = drone_trees_matched)
ground_map = remove_overmatched_drone_trees(ground_map)  
sum(!is.na(ground_map$final_drone_map_match_id))









#### BONUS: make lines connecting the pairs ####

#testing

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

st_write(lines_df %>% st_transform(4326),data("dev/pairings.geojson"), delete_dsn = TRUE)
  



