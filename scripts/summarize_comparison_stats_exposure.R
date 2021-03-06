
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

stats_exp = stats %>%
  rename(photoset = metashape_run_name_pt1,
         metashape_config = metashape_run_name_pt2) %>%
  mutate(metashape_config = as.numeric(metashape_config)) %>%
  filter(metashape_config %in% 15000:16999) %>%
  filter(photoset %in% c("paramset14b","paramset15b","paramset20b","paramset19b")) # excluding the composite "paramset41" because it incorrectly combines 120m_auto with 90m_low

## make sure the stats are unique
stats_count_unique = stats_exp %>%
  group_by(config_name,photoset,metashape_config,tree_position,height_cat) %>%
  summarize(n = n())


#### Get the best metashape paramsets 


### simpler alternative
stats_summ_pre = stats_exp %>%
  mutate(thin_code = str_sub(metashape_config,3,3) %>% as.numeric,
         set_code = str_sub(metashape_config,4,5) %>% as.numeric) %>%
  mutate(thin = dplyr::case_when((thin_code=="1") & (photoset %in% c("paramset14b","paramset15b")) ~ "95/95",
                                 (thin_code=="2") & (photoset %in% c("paramset14b","paramset15b")) ~ "90/90",
                                 (thin_code=="1") & (photoset %in% c("paramset19b","paramset20b")) ~ "90/90",
                                 (thin_code=="2") & (photoset %in% c("paramset19b","paramset20b")) ~ "80/80")) %>%
  mutate(altitude_exposure = recode(photoset,
                                 "paramset14b" = "120m_auto",
                                 "paramset15b" = "90m_auto",
                                 "paramset20b" = "120m_low",
                                 "paramset19b" = "90m_low")) %>%
  filter(height_cat %in% c("10+","20+"))



## make a heatmap plot of f score by vwf x metashape

d_plot = stats_summ_pre %>%
  filter(#altitude_pitch == "120m_nadir",
         height_cat == "10+",
         tree_position == "all") %>%
         #thin == "90/90") %>%
  mutate(set_code = factor(set_code, levels=c("9","11","15","16")))

ggplot(d_plot,aes(x=set_code,y=config_name,fill=f_score)) +
  geom_tile() +
  scale_fill_viridis() +
  facet_grid(thin~altitude_exposure)







stats_alt_pitch_summ = stats_summ_pre %>%
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

stats_alt_pitch_summ_plot = stats_alt_pitch_summ %>%
  filter(tree_position == "single", height_cat == "10+")


## plot

ggplot(stats_alt_pitch_summ_plot, aes(x = thin, y = f_score, color=altitude_pitch)) +
  geom_line(aes(group=altitude_pitch),size=1) +
  theme_bw(14) +
  labs(x = "Overlap", y = "F score") +
  scale_color_viridis_d(end = 0.9)



#### Get the composite pitch stats ####

stats_composite_pitch = stats %>%
  rename(photoset = metashape_run_name_pt1,
         metashape_config = metashape_run_name_pt2) %>%
  mutate(metashape_config = as.numeric(metashape_config)) %>%
  filter(metashape_config %in% 16000:16999)

## make sure the stats are unique
stats_count_unique = stats_composite_pitch %>%
  group_by(config_name,photoset,metashape_config,tree_position,height_cat) %>%
  summarize(n = n())


#### Get the best metashape paramsets 

### simpler alternative
stats_summ_pre = stats_composite_pitch %>%
  mutate(thin_code = str_sub(metashape_config,3,3) %>% as.numeric,
         set_code = str_sub(metashape_config,4,5) %>% as.numeric) %>%
  mutate(thin = dplyr::recode(thin_code,
                              "1" = "90/90",
                              "2" = "80/80",
                              "5" = "95/95",
                              "6" = "90/90",
                              "3" = "95/95-",
                              "4" = "90/90-",
                              "7" = "90/90"),
         altitude_pitch = recode(photoset,
                                 "paramset31" = "120m_multipitch",
                                 "paramset32" = "90m_multipitch",
                                 "paramset41" = "120m_multiexp",
                                 "paramset14b" = "120m_normalexp",
                                 "paramset19b" = "120m_lowexp")) %>%
  filter(height_cat %in% c("10+","20+")) %>%
  filter(!(thin %in% c("95/95-","90/90-")))



## make a heatmap plot of f score by vwf x metashape

d_plot = stats_summ_pre %>%
  filter(#altitude_pitch == "120m_nadir",
    height_cat == "10+",
    tree_position == "single") %>%
  #thin == "90/90") %>%
  mutate(set_code = factor(set_code, levels=c("9","11","15","16")))

ggplot(d_plot,aes(x=set_code,y=config_name,fill=f_score)) +
  geom_tile() +
  scale_fill_viridis() +
  facet_grid(thin~altitude_pitch)



stats_composite_pitch_summ = stats_summ_pre %>%
  filter(!(thin == "80/80" & altitude_pitch == "120m_lowexp")) %>%
  group_by(altitude_pitch, height_cat, tree_position, thin) %>%
  summarize(#f_config = config_name[which(f_score == quantile(f_score,1))][1],
            #meta_config = set_code[which(f_score == quantile(f_score,1))][1],
            f_score = quantile(f_score,1, na.rm=TRUE),
            height_cor = quantile(height_cor,1, na.rm=TRUE),
            sensitivity = quantile(sensitivity,1, na.rm=TRUE))



## Combine multi-pitch with normal pitch for figure

alt_pitch_p = bind_rows(stats_alt_pitch_summ,
                        stats_composite_pitch_summ %>% filter(altitude_pitch %in% c("120m_multipitch",
                                                                                    "90m_multipitch"))) %>%
  filter(height_cat == "10+",
         tree_position == "single") %>%
  ## separate altitude and pitch
  separate(altitude_pitch, into = c("altitude","pitch"), sep=fixed("_")) %>%
  mutate(pitch = factor(pitch,levels=c("nadir","25deg","multipitch")))

ggplot(alt_pitch_p, aes(x = thin, y = f_score, color=altitude, linetype = pitch)) +
  geom_line(aes(group=paste0(altitude,pitch)),size=1) +
  theme_bw(14) +
  labs(x = "Overlap", y = "F score") +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  scale_linetype_manual(values = c("solid","longdash","dotted"))





