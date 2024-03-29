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
  mutate(metashape_run_name = paste(metashape_run_name_pt1,metashape_run_name_pt2, sep="_")) %>%
  ## remove an old layer that was re-run but original not removed
  filter(metashape_layer_name != "paramset15_15609_20210202T0655_dsm_chm")

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
  filter(metashape_config == 1011,
         tree_position == "single",
         height_cat == "20+")

stats = stats %>%
  mutate(treedet_type = str_sub(config_name,1,3))


stats = stats %>%
  #### OPTIONAL filtering to only focal Metashape sets and focal VWF sets: for testing effect of doubling max_neighbors (for thins 2_2 and 1_2) ####
  #filter(config_name %in% c("vwf_196", "vwf_186", "vwf_185", "vwf_197", "vwf_176", "vwf_120", "vwf_121", "vwf_207", "vwf_109")) %>%
  #filter(metashape_config %in% c(11:16, 29:34, 411:416, 429:434)) %>%
  
  ## filter to 1007:1018 for VWF and 7:18 for las
  filter(((treedet_type == "las") & (metashape_config %in% c(07:18))) | ((treedet_type == "vwf") & (metashape_config %in% c(1007:1018)))) %>%
  
  ## rename the 1007-1018 to 7-18 so they can also work for las
  mutate(metashape_config = ifelse(metashape_config > 1000,metashape_config - 1000,metashape_config)) %>%
  
  ## optional filter to las only
  #filter(treedet_type == "las") %>%
  
  filter(photoset %in% c("paramset14","paramset15"))



#### Get a table of F scores for all tree detection methods, for Meta16, 120m, nadir, 90/90
stats_treedet_table_1 = stats %>%
  mutate(method = ifelse(is.na(method), "VWF",method)) %>%
  filter(tree_position == "all", height_cat == "10+", photoset == "paramset14", metashape_config == 16) %>%
  arrange(desc(f_score)) %>%
  select(tree_position,height_cat, config_name, method,f_score, sensitivity, precision)

stats_treedet_table_2 = stats %>%
  mutate(method = ifelse(is.na(method), "VWF",method)) %>%
  filter(tree_position == "all", height_cat == "20+", photoset == "paramset14", metashape_config == 16) %>%
  arrange(desc(f_score)) %>%
  select(tree_position,height_cat, config_name, method,f_score, sensitivity, precision)

stats_treedet_table_3 = stats %>%
  mutate(method = ifelse(is.na(method), "VWF",method)) %>%
  filter(tree_position == "single", height_cat == "10+", photoset == "paramset14", metashape_config == 16) %>%
  arrange(desc(f_score)) %>%
  select(tree_position,height_cat, config_name, method,f_score, sensitivity, precision)

stats_treedet_table_4 = stats %>%
  mutate(method = ifelse(is.na(method), "VWF",method)) %>%
  filter(tree_position == "single", height_cat == "20+", photoset == "paramset14", metashape_config == 16) %>%
  arrange(desc(f_score)) %>%
  select(tree_position,height_cat, config_name, method,f_score, sensitivity, precision)

stats_treedet_table = bind_cols(stats_treedet_table_1,stats_treedet_table_2,stats_treedet_table_3,stats_treedet_table_4)

write_csv(stats_treedet_table, data("tables/all_treedet_scores.csv"))

#### Get the best metashape paramsets 

stats_summ_pre = stats %>%
  filter(height_cat %in% c("10+","20+")) %>%
  
  ##!! optional filtering to best sets
  #filter(metashape_config %in% c(9,11,15,16)) %>% filter(config_name %in% c("vwf_122","vwf_196")) %>%

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


## Temporarily reorder x-axis of plot so 1000s are together
order = c(7, 1007, 2007,4007,
          8, 1008, 2008,4008,
          9, 1009, 2009,4009,
          10, 1010, 2010,4010,
          11, 1011, 2011, 4011,
          12, 1012, 2012,4012,
          13, 1013, 2013,4013,
          14, 1014, 2014,4014,
          15, 1015, 2015,4015,
          16, 1016, 2016,4016,
          17, 1017, 2017,4017,
          18, 1018, 2018, 4018)


stats_summ = stats_summ %>%
  mutate(metashape_config = factor(metashape_config, levels=order))

# %>%
#   arrange(photoset_height_position) %>%
#   pivot_wider(id_cols = c(-metric,-photoset,-height_cat,-tree_position),names_from = photoset_height_position, values_from = "value")
#   
  

### F-score plot

d_plot = stats_summ %>%
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



#### Make a more detailed version of the F-score heatmap, with meaningful facets. Repeat for each tree stature category panel (a,b,c,d) ####

# load Metashape config defs
metashape_defs = read_csv(data("metashape_config_defs/metashape_config_defs.csv")) %>%
  mutate(config = as.character(config))

fig_fun = function(d_plot, title) {
  d_plot2 = left_join(d_plot,metashape_defs,by=c("metashape_config" = "config")) %>%
    mutate(align_qual = recode(align_photos_downscale,"1"="high","2"="med","4"="low"),
           depth_qual = recode(depth_map_downscale,"2"="high","4"="med"),
           depth_filter = recode(depth_map_filter,"moderate"="Depth filter: mod",
                                 "mild" = "Depth filter: mild"),
           altitude = recode(photoset,"paramset14" = "Alt: 120 m",
                             "paramset15" = "Alt: 90 m")) %>%
    mutate(config_text = paste0("a")) %>%
    mutate(align_qual = factor(align_qual,c("low","med","high")),
           depth_qual = factor(depth_qual,c("low","med","high")))
  
  #textcol = ifelse(title == "(a) All trees > 10 m", "white","black")
  scalemin = min(d_plot2$value)
  scalemax = max(d_plot2$value) + 0.001
  
  p = ggplot(d_plot2,aes(y=depth_qual,x=align_qual,fill=value)) +
    geom_tile() +
    geom_text(aes(label=cfg_num), size=3) +  #, color=textcol
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          ) +
    scale_fill_viridis(name="F score", labels = function(x) sprintf("%.2f", x), begin = 0.2, end = 0.9, limits=c(scalemin,scalemax)) + #, limits=c(0.64,0.87)
    facet_grid(altitude~depth_filter) +
    labs(x="Alignment quality",
         y="Dense cloud quality",
         title = title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0))
  
  return(p)
}


d_plot = stats_summ %>%
  filter(metric == "f_score") %>%
  filter(height_cat == "20+",
         tree_position == "single") %>%
  group_by(metashape_config,metric,photoset) %>%
  summarize(value = max(value),
            cfg_num = cfg_num[which(value == quantile(value,1))][1])

p = fig_fun(d_plot, "(d) Dominant trees > 20 m")

png(data("figures/meta-params_v3_20single.png"),res=200,width=800,height=570)
p
dev.off()


d_plot = stats_summ %>%
  filter(metric == "f_score") %>%
  filter(height_cat == "10+",
         tree_position == "single") %>%
  group_by(metashape_config,metric,photoset) %>%
  summarize(value = max(value),
            cfg_num = cfg_num[which(value == quantile(value,1))][1])

p = fig_fun(d_plot, "(c) Dominant trees > 10 m")

png(data("figures/meta-params_v2_10single.png"),res=200,width=800,height=570)
p
dev.off()














#### In each tree category and flight altitude, how much better is 200 max neighbors than 100? (this is defunct now that not making this comparison)

comp_neighbors = stats_summ %>%
  filter(metashape_config %in% c(7:18,25:36)) %>%
  mutate(metashape_config = metashape_config %>% as.character %>% as.numeric) %>%
  mutate(max_neighbors = ifelse(metashape_config < 19, 100,200)) %>%
  mutate(metashape_config = ifelse(metashape_config > 18, metashape_config - 18, metashape_config)) %>%
  select(-cfg_num,-config) %>%
  pivot_wider(names_from=max_neighbors, values_from = value) 

comp_neigh_summ = comp_neighbors %>%
  group_by(metashape_config, metric) %>%
  summarize(improvementTo200 = mean(`200`-`100`))






#### Find best Metashape paramsets ####

stats

# ## For each cell of F and Sens, rank tree detection params and keep top 10%.
# 
# stats = stats %>%
#   mutate(phot_ht_pos_met = paste(photoset, height_cat, tree_position, metashape_config,sep="-"))

#### Select best Metashape parameter sets ####

stats_summ_main = stats_summ %>%
  filter(metashape_config %in% 7:18)  ## look just at the sets with 100 max neighbors, and exclude the ones that are clearly bad (1-6)

### For each height cat and photoset (F and sens), get all sets which are within 0.005 F

height_positions = c("10+_single","10+_all","20+_single","20+_all")
photosets = c("paramset14","paramset15")
metrics = c("f_score") # ,"sensitivity"

categories = expand.grid(height_position = height_positions,photoset = photosets,metric = metrics)

good_cfgs = list()
best_cfgs = list()

for(i in 1:nrow(categories)) {
  
  category = categories[i,]
  
  stats_summ_foc = stats_summ_main %>%
    filter(height_position == category$height_position,
           photoset == category$photoset,
           metric == category$metric)
  
  max = max(stats_summ_foc$value)
  
  if(category$metric == "f_score")  {
    min = max - 0.00
  } else {
    min = max - 0.02
  }
  
  good = stats_summ_foc[stats_summ_foc$value >= min,"metashape_config"] %>% pull() %>% as.character %>% as.numeric 
  good_cfgs[[i]] = good
  
  best = stats_summ_foc[stats_summ_foc$value == max,"metashape_config"] %>% pull() %>% as.character %>% as.numeric 
  best_cfgs[[i]] = best

}

# turn the best config vector into a character string
good_str = map(good_cfgs,str_flatten,collapse=", ") %>% unlist
categories$best_meta_list = good_str

categories = categories %>%
  separate(height_position,into=c("height","position"),sep="_") %>%
  mutate(altitude = recode(photoset,"paramset14" = "120",
                           "paramset15" = "90")) %>%
  select(altitude, height, position,best_meta_list)

write_csv(categories,data("tables/best_metashape_paramset_list.csv"))


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


# selected 9, 11, 15, 16 # everything was satisfied for 16, but adding the others for comparison and backwards comparison to previous work which used them


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
  filter(metashape_config %in% c(9, 11, 15, 16)) %>%
  filter(height_cat %in% c("10+","20+")) %>%
  filter(photoset %in% c("paramset15","paramset14"))

## Exclude if it's a sensitivity one and it's < 0.1 less than the best f
stats_main = stats_main %>%
  mutate(exclude_sens = f_score < (best_f - 0.1)) %>%
  mutate(sensitivity = ifelse(exclude_sens,NA,sensitivity)) %>%
  select(-exclude_sens, -best_f)

## Make longer so we can look for the best tree detection by metric
stats_main = stats_main %>%
  pivot_longer(c(sensitivity,f_score),names_to = "metric") %>%
  filter(!is.na(value))



### For each height cat and photoset (F and sens), get all detection params which are within 0.015 F of the best

height_cats = c("10+", "20+")
tree_positions = c("single", "all")
photosets = c("paramset14","paramset15")
metrics = c("f_score") #,"sensitivity"
metashape_configs = c(9, 11, 15, 16)

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
    min = max - 0.00
  } else {
    min = max - 0.0
  }
  
  good = stats_summ_foc[stats_summ_foc$value >= min,"config"] %>% pull() %>% as.character
  
  good_cfgs[[i]] = good
  
}


# turn the best config vector into a character string
good_str = map(good_cfgs,str_flatten,collapse=", ") %>% unlist
categories$best_meta_list = good_str

categories = categories %>%
  mutate(altitude = recode(photoset,"paramset14" = "120",
                           "paramset15" = "90")) %>%
  select(altitude, height_cat, tree_position,metashape_config,best_meta_list)

write_csv(categories,data("tables/best_treedetection_paramset_list.csv"))


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
# c(11:16) # also try 29:34 for 200 max_neighbors


## For the initial selection of VWF methods based on photosets 14 and 15:
# 113 was the only one necessary for 10+, single, 14 and 15.
# To extend to "10/20+ and single/all, 14/15, need to add 196, 122.
# To maximize sensitivity, need to add: 121,185, 110, 109
# If just for 1016, 10/20, single/all, 14/15, f_score/sens: 196, 113, 120

# Total selected: c("vwf_113", "vwf_121", "vwf_122", "vwf_120", "vwf_185", "vwf_196", "vwf_110", "vwf_109")

### Now do a secondary tuning where we just run these sets, on photosets 14 and 15, but compare 100 and 200 max_neighbors both for thin 2_2 and for thin 1_2.
# We already have this for 2_2, so just need to repeat this subset of metashape and tree detection for 1_2.

## Done: the repeats of the metashape sets 11:16 and 29:34 are 411:416 and 429:434.
## There was no pronounced or consistent effect of doubling max_neighbors, so don't need to do that.

## So this means we should run sets 1009, 1011, 1015, 1016, and the above VWFs for all thins, both heights and composites, both angles
# reg: 14, 15
# 25deg: 26b, 27b
# same alt, diff angle: 31, 32   _2121,2111,2222,4242,4222


## Also need to compare the ones that use different EV (including composite: 41, 42)



### Manual inspection of stats to compare methods

stats_man = stats %>%
  filter(metashape_config == 32 & photoset == "paramset14" & height_cat == "10+" & tree_position == "single" & config_name %in% c("las_1020","vwf_196"))

stats_man = stats %>%
  filter(metashape_config %in% c(30,32) & photoset == "paramset14" & height_cat == "10+" & tree_position == "single" & config_name %in% c("vwf_196"))



