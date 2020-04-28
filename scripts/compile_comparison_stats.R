## Compile all the individual ground-drone map comparison stats files

data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(here)

source(here("scripts/convenience_functions.R"))


## Load the config definitions
config_files = list.files(data("parameter_set_definitions"), pattern="defs_[0-9][0-9][0-9]\\.csv", full.names = TRUE)

configs = map_dfr(config_files, read_csv)





stats_files = list.files(data("drone_map_evals/comparison_stats/individual"), pattern="\\.csv", full.names = TRUE)

### Compile stats

stats = map_dfr(stats_files, read_csv)

## Pull in config defs
stats = stats %>%
  mutate(config_name = str_split(drone_map_name,"-") %>% map_chr(2))

stats = left_join(stats, configs, by = c("config_name"="vwf_name"))


### Arrange for inspection

stats = stats %>%
  arrange(tree_position, height_cat, -f_score)

stats = stats %>%
  arrange(tree_position, height_cat, mean_abs_err)


### Plotting of best params

vwf_dat = stats %>%
  filter(tree_position == "all",
         height_cat == "30+",
         )

library(viridis)

ggplot(vwf_dat, aes(x = a, y = b, fill = correlation)) +
  geom_tile() +
  facet_wrap(smooth) +
  scale_fill_viridis_c()





