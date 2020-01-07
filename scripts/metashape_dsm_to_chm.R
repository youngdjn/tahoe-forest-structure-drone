## Takes a USGS DEM (DTM) and a Metashape DSM and calculates a CHM

# The root of the data directory
data_dir = "C:/Users/DYoung/Box/projects/forestuav_data/"

# Name of specific Metashape project for which to process Metashape products
project_dir = "paramset001/"

library(sf)
library(raster)
library(here)

#### Convenience functions ####

source(here("scripts/convenience_functions.R"))


#### Load data ####

roi = st_read(data("study_area_perimeter/emerald_point_roi.geojson"))
dem = raster(data("dem_usgs/dem_usgs.tif"))

# find the dsm file in the metashape_products direcotry
dir = data(  paste0("metashape_products/",project_dir ))
dsm_file = list.files(dir,pattern="dsm_coarse.tif", full.names=TRUE)
if(length(dsm_file) > 1) stop("More than 1 DSM file in the specified metashape data products folder.")
if(length(dsm_file) == 0) stop("No DSM files int he specified metashape data products folder.")

dsm = raster(dsm_file)


#### Prep data ####

# crop and mask DSM to project roi
dsm = crop(dsm, roi %>% st_transform(crs(dsm)))
dsm = mask(dsm,roi %>% st_transform(crs(dsm)))

# interpolate the the DEM to the res, extent, etc of the DSM
dem_interp = resample(dem %>% projectRaster(crs=crs(dsm)),dsm)


#### Calculate canopy height model ####
#### and save to tif

# calculate canopy height model
chm = dsm - dem_interp

# create dir if doesn't exist, then write
dir = data(paste0("post_metashape_products/",project_dir))
dir.create(dir)
writeRaster(chm,paste0(dir,"chm_metashape.tif")) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud
