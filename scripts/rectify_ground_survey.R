## Use the shift amounts and directions learned from the manual ground stem map alignment (check_ground_survey_rectification.R) to shift all trees in ground map

library(tidyverse)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))

#### Load and clean data ####

ground_trees_orig = st_read(data("ground_truth_stem_map/processed/ept_trees_01_uncorrected.geojson")) %>% filter(Height > 5) %>% st_transform(3310)
tree_shifts = read_csv(data("reference_alignment_eval/tree_shift_dir_summary.csv"))

## manually remove trees from plots/subplos that aren't possible to do well
ground_trees_orig = ground_trees_orig %>%
  filter(!(Plot == "B3.5" | Plot == "B3.5a" | (Plot == "Bx3.25" & data_col_location == 2) | Plot == "C1.5" | Plot == "D3.25" | Plot == "D3.5" | Plot == "Bx1.5" |  Plot == "B3.25") | Plot == "D3")


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


  #### Write buffer of excluded plots ####

plots = st_read(data("ground_truth_stem_map/processed/ept_plots_01_uncorrected.geojson")) %>% st_transform(3310)

plots_excluded = plots %>%
  filter((Plot == "B3.5" | Plot == "B3.5a" | (Plot == "Bx3.25" & loc == 2) | Plot == "C1.5" | Plot == "D3.25" | Plot == "D3.5" | Plot == "Bx1.5" |  Plot == "B3.25") | Plot == "D3")

plots_excluded_buffer = plots_excluded %>% st_buffer(25) %>% st_union()
st_write(plots_excluded_buffer %>% st_transform(4326), data("ground_truth_stem_map/rectified/plots_excluded_buffer.geojson"), delete_dsn = TRUE)
  
       