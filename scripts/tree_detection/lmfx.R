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

#### DTM

dtm = raster(data("dem_usgs/dem_usgs.tif")) %>% projectRaster(crs = "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


las_layer_name = "paramset14_01"

las_file = list.files(data("metashape_products"),pattern=paste0(las_layer_name,".*\\.las"), full.names=TRUE)
if(length(las_file) > 1) stop("More than 1 matching CHM file in the specified metashape data products folder.")
if(length(las_file) == 0) stop("No matching CHM files int he specified metashape data products folder.")

las = readLAS(las_file)
las = clip_roi(las,focal_area)

las = normalize_elevation(las = las, algorithm = dtm, na.rm = TRUE)


las_singlelas_singleparamset = function(ws, dist_2d, las) {

  ttops = find_trees(las, lmfx(ws = ws, dist_2d = dist_2d))
  gc()
  cat("Finished one.\n")

}



# Equation and coefficients taken from Popescu and Wynne (2004)'s "Pines model"
dynamicWindow_pines <- function(x) { 0.5 * (3.75105 - 0.17919*x + 0.01241 * x^2) }

# Equation and coefficients taken from Popescu and Wynne (2004)'s conifer + deciduous model: "Combined model"
dynamicWindow_combined <- function(x) { 0.5 * (2.51503 + 0.00901 * x^2 ) }

dist_2d_vals <- c(1.0, 1.5, 2.0, 2.5, 3)
ws_vals <- list(1.5, 2, 2.5, 3, dynamicWindow_pines, dynamicWindow_combined)

lmfx_params = crossing(dist_2d = dist_2d_vals, ws = ws_vals)


a = find_trees(las, lmfx(ws = 3, dist_2d = 3, hmin=8))

b = find_trees(las, LayerStacking(res=1,start=5, hmin=5))


result = pmap(lmfx_params, las_singlelas_singleparamset, las = las, .options = future_options())

a = segment_trees(las,li2012(hmin=8,dt1=1.4,dt2=1.4,R=1))

### consider thinning point cloud to test these algos

