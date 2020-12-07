## Compile all the individual ground-drone map comparison stats files

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
  mutate(metashape_config = as.numeric(metashape_config)) #%>%
  #mutate(metashape_config = factor(metashape_config,levels=c(1:36)))

## make sure the stats are unique
stats_count_unique = stats %>%
  group_by(config_name,photoset,metashape_config,tree_position,height_cat) %>%
  summarize(n = n())

## manual checking for best
man = stats %>%
  filter(metashape_config == 31,
         tree_position == "single",
         height_cat == "20+")


## Temporary: all metashape 14_028 remove, replace with 14_228

stats = stats %>%
  filter(metashape_run_name != "paramset14_028")

stats[stats$metashape_run_name == "paramset14_228",c("metashape_config")] = 28
stats[stats$metashape_run_name == "paramset14_228",c("metashape_run_name")] = "paramset14_028"


stats = stats %>%
  filter(metashape_run_name != "paramset14_032")

stats[stats$metashape_run_name == "paramset14_232",c("metashape_config")] = 32
stats[stats$metashape_run_name == "paramset14_232",c("metashape_run_name")] = "paramset14_0328"


stats = stats %>%
  filter(metashape_run_name != "paramset15_028")

stats[stats$metashape_run_name == "paramset15_228",c("metashape_config")] = 28
stats[stats$metashape_run_name == "paramset15_228",c("metashape_run_name")] = "paramset15_028"


#### Get the best metashape paramsets for 10 m tree height, F score


stats_summ_pre = stats %>%
  filter(height_cat %in% c("10+","20+"),
         photoset != "paramset15a") %>%
  group_by(metashape_config,photoset, height_cat, tree_position) %>%
  summarize(sens_config = config_name[which(sensitivity == quantile(ifelse(f_score > (max(f_score-0.2)),sensitivity,0),1))][1], ## in the future, need to slect the config with best F score if multiple have sens == 1
            sensitivity = quantile(ifelse(f_score > (max(f_score-0.2)),sensitivity,0),1),
            f_config = config_name[which(f_score == quantile(f_score,1))][1],
            f_score = quantile(f_score,1),
            height_config = config_name[which(height_cor == quantile(height_cor,1))][1],
            height_cor = quantile(height_cor,1))


stats_summ_scores = stats_summ_pre %>%
  select(-f_config, -height_config, -sens_config) %>%
  pivot_longer(cols=c(f_score,height_cor,sensitivity), names_to="metric",values_to = "value")

stats_summ_configs = stats_summ_pre %>%
  select(-f_score,-height_cor,-sensitivity) %>%
  rename(f_score = f_config, height_cor = height_config, sensitivity = sens_config) %>%
  pivot_longer(cols=c(f_score,height_cor,sensitivity), names_to="metric",values_to = "config")


stats_summ = left_join(stats_summ_scores,stats_summ_configs,by=c("metashape_config","photoset","height_cat","tree_position","metric")) %>%
  mutate(height_position = paste(height_cat,tree_position,sep="_")) %>%
  mutate(cfg_num = str_split(config,pattern=fixed("_")) %>% map(2))%>%
  mutate(metashape_config = as.factor(metashape_config))


# %>%
#   arrange(photoset_height_position) %>%
#   pivot_wider(id_cols = c(-metric,-photoset,-height_cat,-tree_position),names_from = photoset_height_position, values_from = "value")
#   
  

### F-score plot

d_plot = stats_summ %>%
  #filter(metashape_config %in% c(2,8,30,32, 532, 632)) %>% ##!! optional filtering to best sets
  filter(metric == "f_score")


p_a = ggplot(d_plot,aes(y=height_position,x=metashape_config,fill=value)) +
  geom_tile() +
  geom_text(aes(label=cfg_num), size=2) +
  facet_grid(photoset~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis()


### Height-cor plot

d_plot = stats_summ %>%
  #filter(metashape_config %in% c(2,8,30,32, 532, 632)) %>% ##!! optional filtering to best sets
  filter(metric == "height_cor")

p_b = ggplot(d_plot,aes(y=height_position,x=metashape_config,fill=value)) +
  geom_tile() +
  geom_text(aes(label=cfg_num), size=2) +
  facet_grid(photoset~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis()


### Sens plot

d_plot = stats_summ %>%
  #filter(metashape_config %in% c(2,8,30,32, 532, 632)) %>% ##!! optional filtering to best sets
  filter(metric == "sensitivity")

p_c = ggplot(d_plot,aes(y=height_position,x=metashape_config,fill=value)) +
  geom_tile() +
  geom_text(aes(label=cfg_num), size=2) +
  facet_grid(photoset~.) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis()

ggarrange(p_a,p_b,p_c,ncol=1)


#### Find best Metashape paramsets ####

stats

# ## For each cell of F and Sens, rank tree detection params and keep top 10%.
# 
# stats = stats %>%
#   mutate(phot_ht_pos_met = paste(photoset, height_cat, tree_position, metashape_config,sep="-"))

#### Select best Metashape parameter sets ####

stats_summ_main = stats_summ %>%
  filter(metashape_config %in% 1:36)

### For each height cat and photoset (F and sens), get all sets which are within 5% of the best

height_positions = c("10+_single","10+_all","20+_single","20+_all")
photosets = c("paramset14","paramset15")
metrics = c("f_score") # ,"sensitivity"

categories = expand.grid(height_position = height_positions,photoset = photosets,metric = metrics)

good_cfgs = list()

for(i in 1:nrow(categories)) {
  
  category = categories[i,]
  
  stats_summ_foc = stats_summ_main %>%
    filter(height_position == category$height_position,
           photoset == category$photoset,
           metric == category$metric)
  
  max = max(stats_summ_foc$value)
  
  if(category$metric == "f_score")  {
    min = max - 0.01
  } else {
    min = max - 0.02
  }
  
  good = stats_summ_foc[stats_summ_foc$value >= min,"metashape_config"] %>% pull() %>% as.character %>% as.numeric 
  
  good_cfgs[[i]] = good

}

## Select the config found in the most categories
## Which categories still need to be satisfied?
## Of the configs from those categories, which are the most common across all categories?
## Select that one


# Convenience function: which cats still need to be satisfied by selecting a config for them?
satisfied_cat = function(selected_cfgs, good_cfgs) {
  satisfied = NULL
  for(i in 1:length(good_cfgs)) {
    incommon = (intersect(good_cfgs[i] %>% unlist(),selected_cfgs) %>% length) > 0
    satisfied = c(satisfied,incommon)
  }
  return(satisfied)
}

selected_cfgs = NULL

all_cfgs_byfrequency = table(unlist(good_cfgs)) %>% sort(decreasing=TRUE) %>% names

cats_satisfied = satisfied_cat(selected_cfgs, good_cfgs)

while(sum(!cats_satisfied) > 0 ) {
  #which cats still need to be satisfied
  cats_satisfied = satisfied_cat(selected_cfgs, good_cfgs)
  #get the configs from the unsatisfied cats
  cfgs_from_unsatisfied = good_cfgs[which(!cats_satisfied)] %>% unlist %>% unique
  # of those configs, which is the most common across all cats (even satisfied ones)
  next_cfg = intersect(all_cfgs_byfrequency,cfgs_from_unsatisfied)[1]
  selected_cfgs = c(selected_cfgs,next_cfg)
}

selected_cfgs






#### Get the best tree detection params ####
### Across F and Sens, photosets, height cats

stats

###!!! within each height cat and config and photoset, need to remove the tree detection sets which get a F more than 0.2 less than the best F
## pull in the best

stats_for_treedet = stats %>%
  select(height_cat,tree_position,metashape_config,photoset,sensitivity, f_score, config_name) %>%
  rename(config= config_name)

stats_summ_foc = stats_summ %>%
  filter(metric == "f_score") %>%
  select(metashape_config,photoset,height_cat,tree_position,best_f = value) %>%
  mutate(metashape_config = metashape_config %>% as.character %>% as.numeric)

stats_for_treedet = left_join(stats_for_treedet, stats_summ_foc, by=c("height_cat","tree_position","metashape_config","photoset"))

## Select focal Metashape parameter sets, tree heights, etc ##
stats_main = stats_for_treedet %>%
  filter(metashape_config %in% c(15, 30, 32, 34, 532)) %>%
  filter(height_cat %in% c("10+","20+")) %>%
  filter(photoset %in% c("paramset15","paramset14"))

## Exclude if it's a sensitivity one and it's < 0.2 less than the best f
stats_main = stats_main %>%
  mutate(exclude_sens = f_score < (best_f - 0.1)) %>%
  mutate(sensitivity = ifelse(exclude_sens,NA,sensitivity)) %>%
  select(-exclude_sens, -best_f)

## Make longer so we can look for the best tree detection by metric
stats_main = stats_main %>%
  pivot_longer(c(sensitivity,f_score),names_to = "metric") %>%
  filter(!is.na(value))



### For each height cat and photoset (F and sens), get all detection params which are within 5% of the best

height_cats = c("10+","20+")
tree_positions = c("single","all")
photosets = c("paramset14","paramset15")
metrics = c("f_score","sensitivity") #,"sensitivity"
metashape_configs = c(15, 30, 32, 34, 532)

categories = expand.grid(height_cat = height_cats, tree_position = tree_positions,photoset = photosets,metric = metrics, metashape_config = metashape_configs)

good_cfgs = list()

for(i in 1:nrow(categories)) {
  
  category = categories[i,]
  
  stats_summ_foc = stats_main %>%
    filter(height_cat == category$height_cat,
           tree_position == category$tree_position,
           photoset == category$photoset,
           metric == category$metric,
           metashape_config == category$metashape_config)
  
  max = max(stats_summ_foc$value)
  if(category$metric == "f_score")  {
    min = max - 0.01
  } else {
    min = max - 0.1
  }
  
  good = stats_summ_foc[stats_summ_foc$value >= min,"config"] %>% pull() %>% as.character
  
  good_cfgs[[i]] = good
  
}



# Convenience function: which cats still need to be satisfied by selecting a config for them?
satisfied_cat = function(selected_cfgs, good_cfgs) {
  satisfied = NULL
  for(i in 1:length(good_cfgs)) {
    incommon = (intersect(good_cfgs[i] %>% unlist(),selected_cfgs) %>% length) > 0
    satisfied = c(satisfied,incommon)
  }
  return(satisfied)
}

selected_cfgs = NULL

all_cfgs_byfrequency = table(unlist(good_cfgs)) %>% sort(decreasing=TRUE) %>% names

cats_satisfied = satisfied_cat(selected_cfgs, good_cfgs)

while(sum(!cats_satisfied) > 0 ) {
  #which cats still need to be satisfied
  cats_satisfied = satisfied_cat(selected_cfgs, good_cfgs)
  #get the configs from the unsatisfied cats
  cfgs_from_unsatisfied = good_cfgs[which(!cats_satisfied)] %>% unlist %>% unique
  # of those configs, which is the most common across all cats (even satisfied ones)
  next_cfg = intersect(all_cfgs_byfrequency,cfgs_from_unsatisfied)[1]
  selected_cfgs = c(selected_cfgs,next_cfg)
}

selected_cfgs


## Metashape configs selected
# c(15, 30, 32, 34, 530, 532, 534)

## Tree detection sets selected
# c("vwf_196", "vwf_186", "vwf_197", "vwf_131", "vwf_188", "vwf_120", "vwf_055", "vwf_110", "vwf_045")











### Manual inspection of stats to compare methods

stats_man = stats %>%
  filter(metashape_config == 32 & photoset == "paramset14" & height_cat == "10+" & tree_position == "single" & config_name %in% c("las_1020","vwf_196"))

stats_man = stats %>%
  filter(metashape_config %in% c(30,32) & photoset == "paramset14" & height_cat == "10+" & tree_position == "single" & config_name %in% c("vwf_196"))



