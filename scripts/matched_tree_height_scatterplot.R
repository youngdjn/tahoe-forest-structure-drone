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
d = read_csv("/home/derek/Documents/temp/trees_matched.csv")

m = lm(drone_tree_height~ground_tree_height,data=d)
s = summary(m)
rsq = s$r.squared

p = ggplot(d,aes(x=ground_tree_height,y=drone_tree_height,)) +
  geom_abline(color="blue") +
  geom_point(size=1) +
  labs(x="Ground-measured height (m)",y="Drone-measured height (m)") +
  theme_bw() + 
  annotate(
    geom = "text", x = 17, y = 50, 
    label = expression(R^2~"="~0.96), size = 4)
    

png("/home/derek/Documents/temp/trees_matched.png", res=210,width=510,height=500)
p
dev.off()
