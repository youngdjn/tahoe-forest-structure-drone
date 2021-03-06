## Takes a USGS DEM (DTM) and a Metashape DSM and calculates a CHM

# Name of specific Metashape project for which to process Metashape products
paramset_name = "paramset27b"

library(sf)
library(raster)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions ####

source(here("scripts/convenience_functions.R"))


#### Load data ####

roi = st_read(data("study_area_perimeter/ground_map_mask.geojson")) %>% st_transform(26910)
dem = raster(data("dem_usgs/dem_usgs.tif"))

# find the dsm file in the metashape_products direcotry
dsm_file = list.files(data("metashape_products"),pattern=paramset_name, full.names=TRUE)
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
dir = data("post_metashape_products/chm/")
dir.create(dir)
writeRaster(chm,paste0(dir,"chm-",paramset_name,".tif")) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud
