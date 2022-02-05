#### Plot the ground survey data as a size-class histogram
# Author: Derek Young

library(tidyverse)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))

#### Load data ####

d = read_csv(data("ground_truth_stem_map/rectified/ept_trees_01_rectified_filtered_tabular.csv"))


d = d %>%
  mutate(height_cat = cut(Height,breaks = c(0,5,10,15,20,25,30,35,40,45,50),
                          labels = c("0","5","10","15","20","25","30","35","40","45"))) %>%
  mutate(Species_coarse = recode(Species,ABMA = "other",PI = "other", PICO = "other", PILA = "other", TSME = "other", SALSCO = "other",
                                 ABCO = "white fir", CADE = "incense cedar", PIJE = "Jeffrey pine", PIPO = "ponderosa pine")) %>%
  mutate(Species_coarse = factor(Species_coarse, levels=c("Jeffrey pine", "ponderosa pine", "white fir", "incense cedar", "other")))

p = ggplot(d,aes(x=Height,fill=Species_coarse)) +
  geom_histogram(breaks = c(5,10,15,20,25,30,35,40,45)) +
  scale_fill_viridis_d(name = "Species") +
  theme_bw(16) +
  labs(x = "Height (m)", y = "Tree count")

png(data("figures/ept_ground_map_histogram.png"), width = 1500, height = 1000, res = 300)
p
dev.off()



