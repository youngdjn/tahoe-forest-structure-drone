## Function to compare a drone map with a ground map and, ultimately, compare all drone maps in the directory with the ground map

library(tidyverse)
library(sf)
library(here)
library(furrr)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))



#### Load and clean ground map data ####
d = read_csv("/home/derek/Documents/data/tahoe-forest-structure-drone_smalldata/drone_map_evals/matched_tree_lists/trees_matched_paramset14_15316_20210124T2155_dsm_chm-vwf_196_single.csv")

m = lm(drone_tree_height~ground_tree_height,data=d)
s = summary(m)
rsq = s$r.squared

p = ggplot(d,aes(x=ground_tree_height,y=drone_tree_height,)) +
  geom_abline(color="blue") +
  geom_point(size=1) +
  labs(x="Ground-measured height (m)",y="Drone-measured height (m)") +
  theme_bw() + 
  annotate(
    geom = "text", x = 14, y = 46, 
    label = expression(R^2~"="~0.96), size = 4)
    

png("/home/derek/Documents/temp/trees_matched.png", res=210,width=510*1.2,height=500*1.2)
p
dev.off()

## compute mean bias

d = d %>%
  mutate(bias=drone_tree_height-ground_tree_height)

mean_bias = mean(d$bias)
mean_abs_err = mean(abs(d$bias))


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



ggplot(g,aes(x=DBH,y=Height)) +
  geom_point() +
  geom_smooth(method = "lm")

newdat = data.frame(DBH=0)
predict(m,newdat)
