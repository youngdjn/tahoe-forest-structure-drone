library(tidyverse)
library(sf)
library(here)
library(furrr)
library(raster)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))



#### Load and clean ground map data ####
d = read_csv("/home/derek/Documents/data/tahoe-forest-structure-drone_smalldata/drone_map_evals/matched_tree_lists/trees_matched_paramset14_15316_20210124T2155_dsm_chm-vwf_196_all.csv")

m = lm(drone_tree_height~ground_tree_height,data=d)
s = summary(m)
s

## Old code from when comparing to 
# rsq = s$r.squared
# 
# p = ggplot(d,aes(x=ground_tree_height,y=drone_tree_height,)) +
#   geom_abline(color="blue") +
#   geom_point(size=1) +
#   labs(x="Ground-measured height (m)",y="Drone-measured height (m)") +
#   ylim(0,50) +
#   theme_bw() + 
#   annotate(
#     geom = "text", x = 14, y = 46, 
#     label = expression(R^2:~0.96), size = 4, hjust=0)
#     
# 
# png("/home/derek/Documents/temp/trees_matched.png", res=210,width=510*1.2,height=500*1.2)
# p
# dev.off()

## compute mean bias

d = d %>%
  mutate(bias=drone_tree_height-ground_tree_height)

mean_bias = mean(d$bias)
mean_abs_err = mean(abs(d$bias))



### Load the detected trees (geospatial version) and extract height from (unsmoothed) chm to show how there is less bias
d2 = st_read(data("detected_trees/paramset14_15316_20210124T2155_dsm_chm-vwf_196.geojson"))

# filter to only the set of trees that had a match
d2 = d2 %>%
  filter(treeID %in% unique(d$drone_tree_id))

# extract height
chm = raster(data("metashape_outputs_postprocessed/chm/paramset14_1016_20201021T0648_dsm_chm.tif"))
#chm = raster(data("metashape_outputs_postprocessed/chm/paramset14_016_20201021T0648_dsm_chm.tif"))

d2$drone_tree_height = extract(chm,d2,method="bilinear")
d2 = left_join(d2,d %>% dplyr::select(drone_tree_id,ground_tree_height),by=c("treeID" = "drone_tree_id"))

m2 = lm(ground_tree_height~drone_tree_height,data=d2)
summary(m2)

p = ggplot(d2,aes(x=ground_tree_height,y=drone_tree_height)) +
  geom_abline(color="blue") +
  geom_point(size=1) +
  labs(x="Ground-measured height (m)",y="Drone-measured height (m)") +
  ylim(0,50) +
  theme_bw() + 
  annotate(
    geom = "text", x = 6, y = 48, 
    label = expression(R^2:~0.95), size = 3.5, hjust=0) + 
  annotate(
    geom = "text", x = 6, y = 43, 
    label = expression(Bias:~"-0.86 m"), size = 3.5, hjust=0) + 
  annotate(
    geom = "text", x = 6, y = 38, 
    label = expression(MAE:~"1.82 m"), size = 3.5, hjust=0)
p

png(data("figures/trees_height_match.png"), res=210,width=510*1.2,height=500*1.2)
p
dev.off()


d2 = d2 %>%
  mutate(bias=drone_tree_height-ground_tree_height) %>%
  mutate(err_pct = (drone_tree_height-ground_tree_height)/ground_tree_height)

mean(abs(d2$err_pct))


mean_bias2 = mean(d2$bias)
mean_abs_err2 = mean(abs(d2$bias))
mean_bias2
mean_abs_err2
cor(d2$ground_tree_height,d2$drone_tree_height) ^ 2

mean_bias
mean_abs_err





### Get ground truth data to evaluate allometry
g = read_excel(data("ground_truth_stem_map/field_data/EPT_tree_data.xlsx")) %>%
  filter(Status == "L")


g = st_read(data("figures/site-map-figure/rectified_clipped_to_focal_area.gpkg"))

g = st_read(data("ground_truth_stem_map/rectified/ept_trees_01_rectified_inclSmall.geojson"))

# clip to focal area
foc = st_read(data("study_area_perimeter/ground_map_mask_precise.geojson"))

g = st_intersection(g,foc)

g2 = g %>%
  filter(Height > 10)

g3 = g %>%
  filter(Height > 15)

table(g3$Species) %>% sort

ggplot(g,aes(x=DBH,y=Height, color=Species)) +
  geom_point()

newdat = data.frame(DBH=0)
predict(m,newdat)
