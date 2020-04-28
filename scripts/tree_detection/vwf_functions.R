## Takes a CHM and makes a map of treetops

library(sf)
library(raster)
library(ForestTools)
library(here)
library(purrr)
library(furrr)
library(tidyverse)

## Convenience functions ####
source(here("scripts/convenience_functions.R"))


#### Main function defs ####

vwf_singlechm_singleparamset = function(chm, chm_smooth_1, chm_smooth_2, chm_smooth_3, layer_name, a, b, smooth, detection_params_name) {

  browser()
  
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


vwf_singlechm_multiparamset = function(chm_layer_name, params = paramsets) {

  #### Load data ####
  
  # find the dsm file in the metashape products direcotry
  chm_file = list.files(data("post_metashape_products/chm"),pattern=chm_layer_name, full.names=TRUE)
  if(length(chm_file) > 1) stop("More than 1 matching CHM file in the specified metashape data products folder.")
  if(length(chm_file) == 0) stop("No matching CHM files int he specified metashape data products folder.")
  
  chm = raster(chm_file)
  
  ## Get the CHM layer name from the chm_file
  
  
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

  
  
  a = pmap(params %>% select(-method), vwf_singlechm_singleparamset , chm=chm, chm_smooth_1 = chm_smooth_1, chm_smooth_2 = chm_smooth_2, chm_smooth_3 = chm_smooth_3, layer_name = chm_layer_name)

}



