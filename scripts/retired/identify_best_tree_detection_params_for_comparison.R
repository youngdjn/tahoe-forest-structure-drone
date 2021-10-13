## This script was previously used to identify which VWF tree detection parameter sets to use, but its functionality has been incorporated into the summarize_comparision_stats_*.R scripts.

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
  mutate(config_name = str_split(drone_map_name,"-") %>% map_chr(2)) %>%
  mutate(chm_name = str_split(drone_map_name,"-") %>% map_chr(1))



stats = left_join(stats, configs, by = c("config_name"="detection_params_name"))

stats = stats

# ### Write
# write_csv(stats,data("drone_map_evals/comparison_stats/compiled/vwf_best_param_search_results_compiled.csv"))


### Arrange for inspection

stats = stats %>%
  arrange(tree_position, height_cat, -f_score)

stats = stats %>%
  arrange(tree_position, height_cat, mean_abs_err)


#### Get best params in multiple cats ####

# for smooth 0,1,2
# tree position, height, smooth
# best by: f_score, height_mae, height_bias, height_cor, mean_abs_err, mean_bias, correlation, mean_abs_err_pct

## variables to get the best methods for
varnames = c("f_score","height_mae","mean_abs_err","correlation")

stats_prep = stats %>%
  mutate(meight_mae = -height_mae,
         mean_abs_err = -mean_abs_err)

full_best_configs = data.frame()

for(i in 1:length(varnames)) {

  varname = varnames[i]
  
  
  
  best_configs = stats_prep %>%
    mutate(c = ifelse(is.na(c),0,c)) %>%
    filter(smooth %in% c(0,1,2),
           tree_position == "all",
           height_cat != "40+") %>%
    distinct(height_cat,smooth,!!rlang::sym(varname),a,b,c, .keep_all=TRUE) %>%
    group_by(height_cat, smooth, c) %>%
    filter(!!rlang::sym(varname) == max(!!rlang::sym(varname))) %>%
    arrange(height_cat, smooth) %>%
    select(height_cat,smooth,!!rlang::sym(varname),config_name,a,b,c,everything()) %>%
    ungroup() %>%
    group_by(height_cat, smooth) %>%
    filter(!!rlang::sym(varname) == max(!!rlang::sym(varname)) | c == 0) ## only keep the ones that max the f_score OR where c == 0
  
  
  
  ### if there are multiple configs of the same smooth and height cat and f_score, and there are ones where c == 0 or NA, only keep those
  
  # Get number of configs of each height cat, smooth, and f_score
  same = best_configs %>%
    group_by(height_cat, smooth, !!rlang::sym(varname)) %>%
    summarize(count = n()) %>%
    filter(count > 1) %>%
    ungroup()
  
  
  for(i in 1:nrow(same)) {
    
    same_row = same[i,]
    
    ## are there ones where c == 0 and c == 1
    
    matching_configs = best_configs %>%
      filter(height_cat == same_row$height_cat &
             smooth == same_row$smooth &
             !!rlang::sym(varname) == as.numeric(same_row[,varname]))
    
    configs_c_counts = matching_configs %>%
      mutate(c = ifelse(c == 0,0,100)) %>%
      group_by(c) %>%
      summarize(c_count = n()) %>%
      filter(c_count > 1)
    
    if(nrow(configs_c_counts) > 1) { # there is c== 0 and c != 0
      best_configs = best_configs %>%
        filter(!(smooth == same_row$smooth & height_cat == same_row$height_cat & !!rlang::sym(varname) == as.numeric(same_row[,varname]) & c!= 0))
    }
   
  }
  
  full_best_configs = bind_rows(full_best_configs,best_configs)

}

full_best_configs = full_best_configs %>%
  arrange(config_name,height_cat,smooth)

best_vwf_params = full_best_configs %>%
  select(a,b,c) %>%
  distinct()

best_vwf_params_smooth1only = full_best_configs %>%
  filter(smooth == 0) %>%
  select(a,b,c) %>%
  distinct()
  

write_csv(full_best_configs,data("parameter_set_definitions/best_vwf_w_smooths.csv"))
write_csv(best_vwf_params,data("parameter_set_definitions/best_vwf_acrossSmooths012.csv"))
write_csv(best_vwf_params_smooth1only,data("parameter_set_definitions/best_vwf_smooth1.csv"))




# ### Check
# 
# check = stats %>%
#   filter(smooth=1,
#          chm_name = "paramset14_01")


### Plotting of best params (linear)

vwf_dat = stats %>%
  filter(tree_position == "all",
         height_cat == "10+",
         is.na(c),
         smooth %in% c(0:8)) %>%
  filter(smooth==1,
         chm_name == "paramset14_01") %>%
  mutate(f_score = ifelse(f_score < 0.5, NA, f_score))


ggplot(vwf_dat, aes(x = a, y = b, fill = f_score)) +
  geom_tile() +
  facet_grid(chm_name~smooth) +
  scale_fill_viridis_c()


### Plotting of best params (quadratic)

vwf_dat = stats %>%
  filter(tree_position == "all",
         height_cat == "30+",
         !is.na(c),
         smooth %in% c(0:8),
         (c >= -0.002 & c <= 0.001)) %>%
  mutate(f_score = ifelse(f_score < 0.5, NA, f_score)) %>%
  distinct(chm_name,a,b,c,f_score) %>%
  filter(!is.na(f_score))

ggplot(vwf_dat, aes(x = a, y = b, fill = f_score, color=f_score)) +
  geom_tile(size=5) +
  facet_grid(chm_name~c) +
  scale_fill_viridis_c() +
  scale_color_viridis_c()






    ### Make a list of the best paramsets

## For each combo of tree position & height, get the parameter set that maximizes f_score and correlation




