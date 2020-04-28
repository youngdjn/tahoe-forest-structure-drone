## Takes a USGS DEM (DTM) and a Metashape DSM and identifies treetops

# The root of the data directory
data_dir = "~/Documents/data/tahoe-forest-structure-drone_data/"

# Name of specific Metashape project for which to process Metashape products


library(sf)
library(raster)
library(ForestTools)
library(here)
library(purrr)
library(furrr)
library(tidyverse)

#### Convenience functions ####

source(here("scripts/convenience_functions.R"))

#### Specify paramset of interest ####

paramset_name = "paramset15a_02"



#### Load data ####

# find the dsm file in the metashape products direcotry
chm_file = list.files(data("post_metashape_products/chm"),pattern=paramset_name, full.names=TRUE)
if(length(chm_file) > 1) stop("More than 1 matching CHM file in the specified metashape data products folder.")
if(length(chm_file) == 0) stop("No matching CHM files int he specified metashape data products folder.")

chm = raster(chm_file)

# create two smoothed rasters: 1 m window width and 0.5 m window width
chm_res = res(chm) %>% mean
pixels_smooth_1 = round(((0.5/chm_res)-1)/2)*2 + 1 # round to nearest odd integer
pixels_smooth_2 = round(((1/chm_res)-1)/2)*2 + 1
pixels_smooth_3 = round(((1.5/chm_res)-1)/2)*2 + 1

weights = matrix(1,nrow=pixels_smooth_1,ncol=pixels_smooth_1)
chm_smooth_1 = focal(chm, weights, fun=mean)

weights = matrix(1,nrow=pixels_smooth_2,ncol=pixels_smooth_2)
chm_smooth_2 = focal(chm, weights, fun=mean)

weights = matrix(1,nrow=pixels_smooth_3,ncol=pixels_smooth_3)
chm_smooth_3 = focal(chm, weights, fun=mean)

#### Identify treetops ####
#### and save to geojson

### Define parameter values to search: only need to run if set defs change

# b = seq(0.00, 0.06, by=0.01)
# a = seq(0.0, 1, by = 0.1)
# smooth = c(0,1,2,3)
# 
# params = expand.grid(a=a,b=b,smooth=smooth)
# 
# # remove sets where both a and b are zero
# params = params %>%
#   filter(!(a == 0 & b == 0))
# 
# params$detection_params_name = paste0("vwf_",str_pad(1:nrow(params), width=3, pad = "0"))
# 
# params$method = "vwf"
# 
# # save vwf params
# write_csv(params,data("parameter_set_definitions/vwfdefs_fullrange.csv"))

# load vwf params
params = read_csv(data("parameter_set_definitions/vwfdefs_fullrange.csv"))



# ### dev
# a = params_foc$a
# b = params_foc$b
# smooth = params_foc$smooth


vwf_singlechm_singleparamset = function(chm, chm_smooth_1, chm_smooth_2, chm_smooth_3, layer_name, a, b, smooth, detection_params_name) {

  if(smooth == 1) {
    chm = chm_smooth_1
  } else if(smooth == 2) {
    chm = chm_smooth_2
  } else if(smooth == 3) {
    chm = chm_smooth_3
  }
  ## Previously for vwf001
  # a = 0.05
  # b = 0.4
  lin <- function(x){x * b + a} # window filter function to use in next step
  treetops <- vwf(CHM = chm, winFun = lin, minHeight = 5, maxWinDiameter = 199)
  treetops = as(treetops,"sf") %>% st_transform(4326)
  
  ## Save treetops
  
  # create dir if doesn't exist, then write
  
  file_name = paste0(layer_name,"-",detection_params_name,".geojson")
  
  dir = data(paste0("post_metashape_products/detected_trees"))
  dir.create(dir)
  st_write(treetops,paste0(dir,"/",file_name), delete_dsn=TRUE)

}


plan(multiprocess, workers = 6)

a = future_pmap(params %>% select(-method), vwf_singlechm_singleparamset , chm=chm, chm_smooth_1 = chm_smooth_1, chm_smooth_2 = chm_smooth_2, chm_smooth_3 = chm_smooth_3, layer_name = paramset_name)



    # ## error with either 14 or 39: fixed by installing from github
# Error in .local(x, ...) : w must have uneven sides
# In addition: There were 13 warnings (use warnings() to see them)

#7,32