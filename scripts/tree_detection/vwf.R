## Takes a USGS DEM (DTM) and a Metashape DSM and identifies treetops

# The root of the data directory
data_dir = "C:/Users/DYoung/Box/projects/forestuav_data/"

# Name of specific Metashape project for which to process Metashape products
project_dir = "paramset001/"

library(sf)
library(raster)
library(ForestTools)
library(here)

#### Convenience functions ####

source(here("scripts/convenience_functions.R"))


#### Load data ####

roi = st_read(data("study_area_perimeter/emerald_point_roi.geojson"))
dem = raster(data("dem_usgs/dem_usgs.tif"))

# find the dsm file in the metashape products direcotry
dir = data(  paste0("post_metashape_products/",project_dir ))
chm_file = list.files(dir,pattern="chm_metashape.tif", full.names=TRUE)
if(length(chm_file) > 1) stop("More than 1 matching CHM file in the specified metashape data products folder.")
if(length(chm_file) == 0) stop("No matching CHM files int he specified metashape data products folder.")

chm = raster(chm_file)



#### Identify treetops ####
#### and save to geojson

lin <- function(x){x * 0.05 + 0.4} # window filter function to use in next step
treetops <- vwf(CHM = chm, winFun = lin, minHeight = 5)
treetops = as(treetops,"sf") %>% st_transform(4326)

## Save treetops

# create dir if doesn't exist, then write
dir = data(paste0("post_metashape_products/",project_dir))
dir.create(dir)
st_write(treetops,paste0(dir,"treetops_vwf001.geojson"))






