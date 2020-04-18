## Use the shift amounts and directions learned from the manual ground stem map alignment (check_reference_stem_map_alignment.R) to shift all trees in ground map

data_dir = "/storage/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(sf)
library(here)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))

#### Load and clean data ####

ground_trees_orig = st_read(data("ground_truth_stem_map/processed/ept_trees_01_uncorrected.geojson")) %>% filter(Height > 5) %>% st_transform(3310)
tree_shifts = read_csv(data("reference_alignment_eval/tree_shift_dir_summary.csv"))

