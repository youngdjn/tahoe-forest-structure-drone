## Compile all the individual ground-drone map comparison stats files

library(tidyverse)
library(here)
library(viridis)


source(here("scripts/convenience_functions.R"))

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)


## Load the config definitions
config_files = list.files(data("parameter_set_definitions"), pattern="defs_.*\\.csv", full.names = TRUE)

configs = map_dfr(config_files, read_csv)





stats_files = list.files(data("drone_map_evals/comparison_stats/individual"), pattern="\\.csv", full.names = TRUE)

### Compile stats

stats = map_dfr(stats_files, read_csv)

## Pull in config defs
stats = stats %>%
  mutate(config_name = str_split(drone_map_name,"-") %>% map_chr(2))

stats = left_join(stats, configs, by = c("config_name"="detection_params_name"))


### Arrange for inspection

stats = stats %>%
  arrange(tree_position, height_cat, -f_score)

stats = stats %>%
  arrange(tree_position, height_cat, mean_abs_err)


### Plotting of best params

vwf_dat = stats %>%
  filter(tree_position == "all",
         height_cat == "20+",
         )


ggplot(vwf_dat, aes(x = a, y = b, fill = f_score)) +
  geom_tile() +
  facet_wrap("smooth") +
  scale_fill_viridis_c()


### Make a list of the best paramsets

## For each combo of tree position & height, get the parameter set that maximizes f_score and correlation




