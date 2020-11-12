## Compile all the individual ground-drone map comparison stats files

library(tidyverse)
library(here)


source(here("scripts/convenience_functions.R"))

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

# Get data file names
stats_files = list.files(data("drone_map_evals/individual"), pattern="\\.csv", full.names = TRUE)

### Compile stats and write

stats = map_dfr(stats_files, read_csv)

write_csv(stats,data("drone_map_evals/compiled/comparison_stats_metashapeEval.csv"))


