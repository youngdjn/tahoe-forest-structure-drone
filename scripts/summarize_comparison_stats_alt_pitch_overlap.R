
library(tidyverse)
library(here)
library(viridis)
library(ggpubr)


source(here("scripts/convenience_functions.R"))

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)


## Load the config definitions
config_files = list.files(data("parameter_set_definitions"), pattern="best_las\\.csv|vwfdefs_fullrange\\.csv", full.names = TRUE)

configs = map_dfr(config_files, read_csv)


## Load the compiled comparison stats
stats_files = list.files(data("drone_map_evals/compiled"), pattern="comparison_stats_metashapeEval\\.csv", full.names = TRUE)

### Compile stats
stats = map_dfr(stats_files, read_csv)

## Pull in config defs
stats = stats %>%
  mutate(config_name = str_split(drone_map_name,"-") %>% map_chr(2)) %>%
  mutate(metashape_layer_name = str_split(drone_map_name,"-") %>% map_chr(1)) %>%
  mutate(metashape_run_name_pt1 = str_split(metashape_layer_name,"_") %>% map_chr(c(1))) %>%
  mutate(metashape_run_name_pt2 = str_split(metashape_layer_name,"_") %>% map_chr(c(2))) %>%
  mutate(metashape_run_name = paste(metashape_run_name_pt1,metashape_run_name_pt2, sep="_"))

stats = left_join(stats, configs, by = c("config_name"="config_name"))

stats = stats %>%
  rename(photoset = metashape_run_name_pt1,
         metashape_config = metashape_run_name_pt2) %>%
  mutate(metashape_config = as.numeric(metashape_config)) %>%
  filter(metashape_config %in% 15000:15999)

## make sure the stats are unique
stats_count_unique = stats %>%
  group_by(config_name,photoset,metashape_config,tree_position,height_cat) %>%
  summarize(n = n())


#### Get the best metashape paramsets 

stats_summ_pre = stats %>%
  filter(height_cat %in% c("10+","20+")) %>%
  group_by(metashape_config,photoset, height_cat, tree_position) %>%
  summarize(sens_config = config_name[which(sensitivity == quantile(ifelse(f_score > (max(f_score-0.2)),sensitivity,0),1))][1], ## in the future, need to slect the config with best F score if multiple have sens == 1
            sensitivity = quantile(ifelse(f_score > (max(f_score-0.2)),sensitivity,0),1),
            f_config = config_name[which(f_score == quantile(f_score,1))][1],
            f_score = quantile(f_score,1),
            height_config = config_name[which(height_cor == quantile(height_cor,1))][1],
            height_cor = quantile(height_cor,1))


### simpler alternative
stats_summ_pre = stats %>%
  mutate(thin_code = str_sub(metashape_config,3,3) %>% as.numeric,
         set_code = str_sub(metashape_config,4,5) %>% as.numeric) %>%
  mutate(thin = dplyr::recode(thin_code,
                       "0" = "80/80",
                       "1" = "90/80",
                       "2" = "80/90",
                       "3" = "90/90",
                       "4" = "95/90",
                       "5" = "90/95",
                       "6" = "95/95"),
         altitude_pitch = recode(photoset,
                                 "paramset14" = "120m_nadir",
                                 "paramset15" = "90m_nadir",
                                 "paramset26b" = "120m_25deg",
                                 "paramset27b" = "90m_25deg")) %>%
  filter(height_cat %in% c("10+","20+")) %>%
  group_by(altitude_pitch, height_cat, tree_position, thin) %>%
  summarize(f_config = config_name[which(f_score == quantile(f_score,1))][1],
            meta_config = set_code[which(f_score == quantile(f_score,1))][1],
            f_score = quantile(f_score,1),
            height_cor = quantile(height_cor,1),
            sensitivity = quantile(sensitivity,1)) %>%
  mutate(altitude_pitch = factor(altitude_pitch,levels = c("120m_nadir",
                                               "90m_nadir",
                                               "120m_25deg",
                                               "90m_25deg")))

stats_summ_plot = stats_summ_pre %>%
  filter(tree_position == "all", height_cat == "10+")


## plot

ggplot(stats_summ_plot, aes(x = thin, y = f_score, color=altitude_pitch)) +
  geom_line(aes(group=altitude_pitch),size=1) +
  theme_bw(14) +
  labs(x = "Overlap", y = "F score") +
  scale_color_viridis_d(end = 0.9)






