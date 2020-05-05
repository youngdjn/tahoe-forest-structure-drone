## Takes all the las files in the Metashape outputs folder and crops them to project area and thins points to 50 per sq m, saves as laz

library(sf)
library(here)
library(purrr)
library(tidyverse)
library(lidR)
#library(furrr)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))



#### Project area boundary ####

focal_area = st_read(data("study_area_perimeter/ground_map_mask.geojson")) %>% st_transform(32610)


#### DTM

dtm = raster(data("dem_usgs/dem_usgs.tif")) %>% projectRaster(crs = "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")



## get las layers from metashape outputs directory
las_files = list.files("/storage/forestuav/metashape_outputs/comparison_project",pattern=".*\\.las", full.names=TRUE)  # to filter to ones matching a name: pattern=paste0(las_layer_name,".*\\.las")

## Only process LAS files < 5 GB
las_file_info = file.info(las_files)
las_file_info$too_large = las_file_info$size > 5e+9
las_files = las_files[!las_file_info$too_large]

crop_and_write_las = function(las_file) {

  cat("Starting",las_file,"...")

  file_minus_extension = str_sub(las_file,1,-5)
  fileparts = str_split(file_minus_extension,fixed("/"))[[1]]
  filename_only = fileparts[length(fileparts)]

  # file to write
  filename = paste0("/storage/forestuav/metashape_outputs_postprocessed/las/",filename_only,".laz")

  # skip if file aleady exists
  if(file.exists(filename)) {
    cat("Already exists:",filename,". Skipping.\n")
    return(FALSE)
  }

  ## Read and clip las
  las = readLAS(las_file)
  las = clip_roi(las,focal_area)
  las = filter_duplicates(las)
  las = decimate_points(las, homogenize(50,.1))
  las = normalize_elevation(las = las, algorithm = dtm, na.rm = TRUE)


  writeLAS(las,filename)

  gc()

  cat("finished.\n")

}

#plan(multiprocess,workers=3)

map(las_files %>% sample, crop_and_write_las)
