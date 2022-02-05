
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

stats_alt_pitch = stats %>%
  rename(photoset = metashape_run_name_pt1,
         metashape_config = metashape_run_name_pt2) %>%
  mutate(metashape_config = as.numeric(metashape_config)) %>%
  filter(metashape_config %in% 15000:15999)

## make sure the stats are unique
stats_count_unique = stats_alt_pitch %>%
  group_by(config_name,photoset,metashape_config,tree_position,height_cat) %>%
  summarize(n = n())


#### Get the best metashape paramsets 

### simpler alternative
stats_summ_pre = stats_alt_pitch %>%
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
  # the grid missions need to have their overlaps defined differently
  # if it was 26b or 27b and the thin was saved as 95/95, it means it was two sets of 90/90, which really is about 92.5, 92.5
  # only do what was saved as 80/80, 90/90
  # what is saved (incorrectly) as 95/95 was two sets of 90/90, and what is saved as 90/90 is really two sets of 80/80
  filter(!(altitude_pitch %in% c("120m_25deg","90m_25deg") & thin_code %in% c("1","2","4","5"))) %>% # some of the thins for the oblique missions are not calculated right. Only two match value that can be intuitively named
  mutate(oblique = ifelse(altitude_pitch %in% c("120m_25deg","90m_25deg"),TRUE,FALSE)) # this is to keep track so we can adjust the nominal overlap. We need to keeps thins 3 and 6

# for the oblique sets, need to interpret the thin codes differently than we are for the nadir sets
stats_summ_pre[stats_summ_pre$altitude_pitch %in% c("120m_25deg","90m_25deg") & stats_summ_pre$thin == "95/95","thin"] = "[90/90+90/90]"
stats_summ_pre[stats_summ_pre$altitude_pitch %in% c("120m_25deg","90m_25deg") & stats_summ_pre$thin == "90/90","thin"] = "[80/80+80/80]"

stats_summ_pre_noncomposite = stats_summ_pre


## make a heatmap plot of f score by vwf x metashape

d_plot = stats_summ_pre %>%
  filter(#altitude_pitch == "120m_nadir",
         height_cat == "20+",
         tree_position == "single") %>%
         #thin == "90/90") %>%
  mutate(set_code = factor(set_code, levels=c("9","11","15","16")))

ggplot(d_plot,aes(x=set_code,y=config_name,fill=f_score)) +
  geom_tile() +
  scale_fill_viridis() +
  facet_grid(thin~altitude_pitch)







stats_alt_pitch_summ = stats_summ_pre %>%
  ## temporary tweak to generate a version of the overlap figure that uses only meta16, vwf196
  # filter(config_name == "vwf_196",
  #        set_code == "16") %>%
  group_by(altitude_pitch, height_cat, tree_position, thin, oblique) %>%
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
  geom_line(data=stats_alt_pitch_summ_plot %>% filter(!oblique) ,aes(group=altitude_pitch),size=1) +
  geom_point(data=stats_alt_pitch_summ_plot %>% filter(!oblique),color="grey50") +
  geom_line(data=stats_alt_pitch_summ_plot %>% filter(oblique), aes(group=altitude_pitch),size=1) +
  geom_point(data=stats_alt_pitch_summ_plot %>% filter(oblique), color="grey50") +
  theme_bw(14) +
  labs(x = "Nominal overlap", y = "F score") +
  scale_color_viridis_d(end = 0.9) +
  coord_cartesian(ylim=c(.55,NA))





#### !!!! Get the composite pitch stats ####

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
  filter(photoset %in% c("paramset31","paramset32")) %>% # composite pitch sets only. !!!! -> For exposure testing, need to change this, and remember the exposure tests are only for subset-area surveys so need to compare against a different baseline (the 14b baseline which is the clipped area version of 14)
  mutate(thin = dplyr::recode(thin_code,
                              "1" = "obsolete",# "95/95", # for the obsolete ones the overlap wasn't calculated right and didn't come out to a round number
                              "2" = "obsolete",# "90/90",
                              "5" = "[90/90+90/90] + 95/90",  # previously 95/95
                              "6" = "[80/80+80/80] + 90/80",  # previously 90/90
                              "3" = "obsolete",# "95/95-",
                              "4" = "obsolete",# "90/90-",
                              "7" = "obsolete"),# "90/90+"),
         altitude_pitch = recode(photoset,
                                 "paramset31" = "120m_multipitch",
                                 "paramset32" = "90m_multipitch")) %>%
                                 #"paramset41" = "120/90m_multiexp",   ## all the rest of these are for exposure testing, but this needs to be done in a separate pipeline once the exposure tests are redone (need to complete the metashape runs for set 43)
                                 #"paramset42" = "90/120m_multiexp",
                                 #"paramset43" = "120_multiexp",
                                 #"paramset14b" = "120m_normalexp")) %>%
                                 #"paramset19b" = "90m_lowexp",
                                 #"paramset20b" = "120m_lowexp")) %>%
  filter(height_cat %in% c("10+","20+")) %>%
  filter(!(thin %in% c("95/95-","90/90-","90/90+","obsolete")))
  ## 5 is 1121, which is 95/90 + (90/90 + 90/90) = 95/95
  ## 6 is 2242, which is 90/80 + (80/80 + 80/80) = 90/90
  ## 7 is 2142, which is 90/80 + (90/80 + 90/80) = 90/90+



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
  # ## temporary tweak to generate a version of the overlap figure that uses only meta16, vwf196
  # filter(config_name == "vwf_196",
  #        set_code == "16") %>%
  group_by(altitude_pitch, height_cat, tree_position, thin) %>%
  summarize(f_config = config_name[which(f_score == quantile(f_score,1))][1],
            meta_config = set_code[which(f_score == quantile(f_score,1))][1],
            f_score = quantile(f_score,1, na.rm=TRUE),
            height_cor = quantile(height_cor,1, na.rm=TRUE),
            sensitivity = quantile(sensitivity,1, na.rm=TRUE)) %>%
  mutate(oblique = FALSE) # this is just for the ones that are full oblique (see where oblique is set to TURE above), not for composites because composite overlaps come out to a round number. This is just for determining whether lines should be shifted in the plot



## Combine multi-pitch with normal pitch for figure

alt_pitch_p = bind_rows(stats_alt_pitch_summ,
                        stats_composite_pitch_summ %>% filter(altitude_pitch %in% c("120m_multipitch",
                                                                                    "90m_multipitch"))) %>%
  filter(height_cat %in% c("10+", "20+"),
         tree_position %in% c("single","all")) %>%
  mutate(height_cat = recode(height_cat,"10+" = "> 10 m",
                             "20+" = "> 20 m")) %>%
  mutate(tree_position = recode(tree_position,"single" = "Dominant trees",
                                "all" = "All trees")) %>%
  mutate(height_position = paste(tree_position, height_cat,sep=" ")) %>%
  ## separate altitude and pitch
  separate(altitude_pitch, into = c("altitude","pitch"), sep=fixed("_")) %>%
  mutate(pitch = recode(pitch,"25deg" = "oblique",
                        "multipitch" = "composite")) %>%
  mutate(pitch = factor(pitch,levels=c("nadir","oblique","composite"))) %>%
  mutate(alt_pitch = paste(altitude,pitch,sep="_")) %>%
  mutate(alt_pitch = factor(alt_pitch,levels=c("120m_nadir","120m_oblique","120m_composite","90m_nadir","90m_oblique","90m_composite"))) %>%
  mutate(thin = factor(thin,levels=c("80/80","80/90","[80/80+80/80]","90/80","[80/80+80/80] + 90/80","90/90","90/95","[90/90+90/90]","95/90","[90/90+90/90] + 95/90","95/95"))) %>%
  mutate(height_position = recode(height_position,"All trees > 10 m" = "a) All trees > 10 m", "All trees > 20 m" = "b) All trees > 20 m", "Dominant trees > 10 m" = "c) Dominant trees > 10 m", "Dominant trees > 20 m" = "d) Dominant trees > 20 m")) %>%
  mutate(altitude = recode(altitude,"120m" = "120 m", "90m" = "90 m"))
  
write_csv(alt_pitch_p,data("figures/fig-dataframes/alt_pitch_p.csv"))

linecol = "#c8e0e8"
linewidth = 1.8

p = ggplot(data=alt_pitch_p,mapping=aes(x = thin, y = f_score, color=altitude, linetype = pitch)) +
  scale_x_discrete() +
  geom_vline(xintercept = 1.5, color=linecol, size=linewidth) +
  geom_vline(xintercept = 4.5, color=linecol, size=linewidth) +
  geom_vline(xintercept = 6.5, color=linecol, size=linewidth) +
  geom_vline(xintercept = 9.5, color=linecol, size=linewidth) +
  # annotate("rect", xmin = 1.7, xmax = 4.3, ymin = -1, ymax = 2, alpha = .1,fill = "blue") +
  # annotate("rect", xmin = 4.7, xmax = 6.3, ymin = -1, ymax = 2, alpha = .1,fill = "blue") +
  # annotate("rect", xmin = 6.7, xmax = 9.3, ymin = -1, ymax = 2, alpha = .1,fill = "blue") +
  # annotate("rect", xmin = 9.7, xmax = 11.3, ymin = -1, ymax = 2, alpha = .1,fill = "blue") +
  geom_line(data=alt_pitch_p,aes(group=alt_pitch),size=.5) +
  geom_point(data=alt_pitch_p,color="grey50") +
  # geom_point() +
  theme_bw(12) +
  labs(x = "Overlap (front/side) (%)", y = "F score") +
  scale_color_viridis_d(begin = 0.2, end = 0.7,name="Altitude") +
  scale_linetype_manual(values = c("nadir"= "solid","oblique"= "longdash","composite"= "dotted"), breaks=c("nadir","oblique","composite"), name="Pitch") +
  coord_cartesian(ylim=c(.55,.9)) +
  facet_wrap(~height_position) +
  theme(strip.background = element_rect(fill = 'white', color="white"),
        strip.text = element_text(size=11,hjust = 0),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_text(angle=45,hjust=1))

png(data("figures/alt-pitch-overlap_v3.png"),res=220,width=1800,height=1200*1.1)
p
dev.off()






#### For each overlap/alt/pitch, get the best meta+vwf by F, the best VWF for Meta16, and specifically m16v196, and get the F score, sens, and prec of each ####

stats_presumm = bind_rows(stats_summ_pre_noncomposite,stats_summ_pre)

# get the best Fs by group
stats_best_fs = stats_presumm %>%
  group_by(altitude_pitch, height_cat, tree_position, thin) %>%
  summarize(max_f = max(f_score),
            max_m16_f = max(f_score[set_code == 16]),
            max_m16v196_f = max(f_score[set_code == 16 & config_name == "vwf_196"]))

# bring the max numbers into the main table to test if a given row is the max row
stats_presumm = left_join(stats_presumm,stats_best_fs) %>%
  mutate(is_max_f = (f_score == max_f),
         is_max_m16_f = (f_score == max_m16_f),
         is_max_m16v196_f = (f_score == max_m16v196_f))

stats_table = stats_presumm %>%
  group_by(altitude_pitch, height_cat, tree_position, thin) %>%
  summarize(max_f_meta = str_flatten(set_code[is_max_f],collapse=", "),
            max_f_detection = str_flatten(config_name[is_max_f],collapse=", "),
            max_f_f = str_flatten(f_score[is_max_f],collapse=", "),
            max_f_sens = str_flatten(sensitivity[is_max_f],collapse=", "),
            max_f_prec = str_flatten(precision[is_max_f],collapse=", "),
            
            max_m16_f_detection = str_flatten(config_name[is_max_m16_f & set_code == "16"],collapse=", "),
            max_m16_f_f = str_flatten(f_score[is_max_m16_f & set_code == "16"],collapse=", "),
            max_m16_f_sens = str_flatten(sensitivity[is_max_m16_f & set_code == "16"],collapse=", "),
            max_m16_f_prec = str_flatten(precision[is_max_m16_f & set_code == "16"],collapse=", "),
            
            max_m16v196_f_f = str_flatten(f_score[is_max_m16v196_f & set_code == "16" & config_name == "vwf_196"],collapse=", "),
            max_m16v196_f_sens = str_flatten(sensitivity[is_max_m16v196_f & set_code == "16" & config_name == "vwf_196"],collapse=", "),
            max_m16v196_f_prec = str_flatten(precision[is_max_m16v196_f & set_code == "16" & config_name == "vwf_196"],collapse=", ")
            
            ) %>%
  filter(height_cat %in% c("10+", "20+"),
         tree_position %in% c("single","all")) %>%
  mutate(height_cat = recode(height_cat,"10+" = "> 10 m",
                             "20+" = "> 20 m")) %>%
  # mutate(tree_position = recode(tree_position,"single" = "Single trees",
  #                               "all" = "All trees")) %>%
  #mutate(height_position = paste(tree_position, height_cat,sep=" ")) %>%
  ## separate altitude and pitch
  separate(altitude_pitch, into = c("altitude","pitch"), sep=fixed("_")) %>%
  mutate(pitch = recode(pitch,"25deg" = "oblique",
                        "multipitch" = "composite")) %>%
  mutate(pitch = factor(pitch,levels=c("nadir","oblique","composite"))) %>%
  #mutate(alt_pitch = paste(altitude,pitch,sep="_")) %>%
  #mutate(alt_pitch = factor(alt_pitch,levels=c("120m_nadir","120m_oblique","120m_composite","90m_nadir","90m_oblique","90m_composite"))) %>%
  
  select(thin,pitch,altitude,tree_position,height_cat,everything()) %>%
  arrange(desc(thin),pitch,altitude,desc(tree_position),desc(height_cat))

write_csv(stats_table,data("tables/f_table.csv"))





