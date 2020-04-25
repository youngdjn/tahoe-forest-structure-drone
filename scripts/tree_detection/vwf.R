## Takes a USGS DEM (DTM) and a Metashape DSM and identifies treetops

# The root of the data directory
data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

# Name of specific Metashape project for which to process Metashape products


library(sf)
library(raster)
library(ForestTools)
library(here)
library(furrr)

#### Convenience functions ####

source(here("scripts/convenience_functions.R"))

#### Specify paramset of interest ####

paramset_name = "paramset14_01"



#### Load data ####

# find the dsm file in the metashape products direcotry
chm_file = list.files(data("post_metashape_products/chm"),pattern=paramset_name, full.names=TRUE)
if(length(chm_file) > 1) stop("More than 1 matching CHM file in the specified metashape data products folder.")
if(length(chm_file) == 0) stop("No matching CHM files int he specified metashape data products folder.")

chm = raster(chm_file)
weights = matrix(1,nrow=9,ncol=9)
chm_smooth = focal(chm, weights, fun=mean)


#### Identify treetops ####
#### and save to geojson

### Define parameter values to search

a = seq(0.02, 0.10, by=0.02)
b = seq(0.2, 1, by = 0.2)
smooth = c(TRUE,FALSE)

params = expand.grid(a=a,b=b,smooth=smooth)

params$vwf_name = str_pad(1:nrow(params), width=3, pad = "0")

vwf_singlechm_singleparamset = function(chm, chm_smooth, layer_name, a, b, smooth, vwf_name) {
  
  
  if(smooth) {
    chm = chm_smooth
  }
  ## Previously for vwf001
  # a = 0.05
  # b = 0.4
  lin <- function(x){x * a + b} # window filter function to use in next step
  treetops <- vwf(CHM = chm, winFun = lin, minHeight = 5)
  treetops = as(treetops,"sf") %>% st_transform(4326)
  
  ## Save treetops
  
  # create dir if doesn't exist, then write
  
  layer_name = paste0(paramset_name,"-vwf_",vwf_name,".geojson")
  
  dir = data(paste0("post_metashape_products/detected_trees"))
  dir.create(dir)
  st_write(treetops,paste0(dir,"/",layer_name), delete_dsn=TRUE)

}


plan(multiprocess)
a = future_pmap(params, vwf_singlechm_singleparamset , chm=chm, chm_smooth = chm_smooth, layer_name = paramset_name)



#7,32