## Use the shift amounts and directions learned from the manual ground stem map alignment (check_reference_stem_map_alignment.R) to shift all trees in ground map

data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(sf)
library(here)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))

#### Load and clean data ####

ground_trees_orig = st_read(data("ground_truth_stem_map/processed/ept_trees_01_uncorrected.geojson")) %>% filter(Height > 5) %>% st_transform(3310)
tree_shifts = read_csv(data("reference_alignment_eval/tree_shift_dir_summary.csv"))


#### Shift trees ####

ground_trees = ground_trees_orig
ground_trees$x = st_coordinates(ground_trees)[,1]
ground_trees$y = st_coordinates(ground_trees)[,2]
st_geometry(ground_trees) = NULL

# Pull in shifts
ground_trees_w_shifts = left_join(ground_trees,tree_shifts, by = c(Plot="ground_tree_plot", data_col_location = "ground_tree_loc"))

# Execute shifts
ground_trees_shifted = ground_trees_w_shifts %>%
  mutate(x_shifted = x + derived_shift_x,
         y_shifted = y + derived_shift_y) %>%
  filter(!is.na(x_shifted)) # need to drop the trees with no inferred shift

# Change back to spatial )
ground_trees_shifted_sf = st_as_sf(ground_trees_shifted,coords = c("x_shifted","y_shifted"), crs = 3310)

#### Write shifted trees ####
st_write(ground_trees_shifted_sf %>% st_transform(4326), data("ground_truth_stem_map/rectified/ept_trees_01_rectified.geojson"), delete_dsn = TRUE)

         