## Takes a CHM and makes a map of treetops

library(sf)
library(here)
library(purrr)
library(furrr)
library(tidyverse)
library(lidR)
library(lidRplugins)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))


#### Project area boundary ####

focal_area = st_read(data("study_area_perimeter/ground_map_mask.geojson")) %>% st_transform(32610)


las_layer_name = "paramset14_01"

las_file = list.files(data("metashape_products"),pattern=paste0(las_layer_name,".*\\.las"), full.names=TRUE)
if(length(chm_file) > 1) stop("More than 1 matching CHM file in the specified metashape data products folder.")
if(length(chm_file) == 0) stop("No matching CHM files int he specified metashape data products folder.")

las = readLAS(las_file)

las = lasclip(las,focal_area)

las2 <- lastrees(las, li2012())
col <- random.colors(200)
plot(las, color = "treeID", colorPalette = col)



