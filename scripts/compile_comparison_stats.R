## Compile all the individual ground-drone map comparison stats files

data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(here)

source(here("scripts/convenience_functions.R"))



stats_files = list.files(data("drone_map_evals/comparison_stats/individual"), pattern="\\.csv", full.names = TRUE)

## Compile stats

stats = map_dfr(stats_files, read_csv)


stats = stats %>%
  arrange(tree_position, height_cat, -f_score)

stats = stats %>%
  arrange(tree_position, height_cat, mean_abs_err)
