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

vwf_singlechm_singleparamset = function(chm, chm_smooth_1, chm_smooth_2, chm_smooth_3, chm_smooth_4, chm_smooth_5, chm_smooth_6, chm_smooth_7, chm_smooth_8, layer_name, a, b, smooth, detection_params_name) {
  
  cat("Running for:", layer_name, detection_params_name,"\n" )
  
  
  ### see if file name already exists, and if so, skip
  file_name = paste0(layer_name,"-",detection_params_name,".geojson")
  dir = data(paste0("post_metashape_products/detected_trees"))
  if(file.exists(paste0(dir,"/",file_name)) & !(smooth %in% c(5,6,7,8)) ) {   ###!!! TEMPORARY: allow overwriting for smooths 5, 6, 7, 8
    cat("Already exists:",layer_name, detection_params_name,"\n")
    return()
  }

  
  
  if(smooth == 1) {
    chm = chm_smooth_1
  } else if(smooth == 2) {
    chm = chm_smooth_2
  } else if(smooth == 3) {
    chm = chm_smooth_3
  } else if(smooth == 4) {
    chm = chm_smooth_4
  } else if(smooth == 5) {
    chm = chm_smooth_5
  } else if(smooth == 6) {
    chm = chm_smooth_6
  } else if(smooth == 7) {
    chm = chm_smooth_7
  } else if(smooth == 8) {
    chm = chm_smooth_8
  }
  
  ## Previously for vwf001
  # a = 0.05
  # b = 0.4
  lin <- function(x){x * b + a} # window filter function to use in next step
  
  treetops <- try(
    vwf(CHM = chm, winFun = lin, minHeight = 5, maxWinDiameter = 199)
  ,silent=TRUE)

  if(class(treetops) == "try-error") {
    cat("***** Skipping:", layer_name, detection_params_name,"because of VWF error ******\n" )
    return(FALSE)
  }
  
  
  treetops = as(treetops,"sf") %>% st_transform(4326)
  
  ## Save treetops
  
  # create dir if doesn't exist, then write
  
  file_name = paste0(layer_name,"-",detection_params_name,".geojson")
  
  dir = data(paste0("post_metashape_products/detected_trees"))
  dir.create(dir)
  st_write(treetops,paste0(dir,"/",file_name), delete_dsn=TRUE, quiet=TRUE)

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
  pixels_smooth_4 = round(((2/chm_res)-1)/2)*2 + 1
  
  weights = matrix(1,nrow=pixels_smooth_1,ncol=pixels_smooth_1)
  chm_smooth_1 = focal(chm, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_2,ncol=pixels_smooth_2)
  chm_smooth_2 = focal(chm, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_3,ncol=pixels_smooth_3)
  chm_smooth_3 = focal(chm, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_4,ncol=pixels_smooth_4)
  chm_smooth_4 = focal(chm, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_1,ncol=pixels_smooth_1)
  chm_smooth_5 = focal(chm, weights, fun=median)
  #now smooth it slightly with a mean weighted primarily by the middle pixel
  middle_pixel = ceiling(pixels_smooth_1/2)
  weights[middle_pixel,middle_pixel] = 1*length(weights)
  weights = weights/(mean(weights))
  chm_smooth_5 = focal(chm_smooth_5, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_2,ncol=pixels_smooth_2)
  chm_smooth_6 = focal(chm, weights, fun=median)
  #now smooth it slightly with a mean weighted primarily by the middle pixel
  middle_pixel = ceiling(pixels_smooth_2/2)
  weights[middle_pixel,middle_pixel] = 1*length(weights)
  weights = weights/(mean(weights))
  chm_smooth_6 = focal(chm_smooth_6, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_3,ncol=pixels_smooth_3)
  chm_smooth_7 = focal(chm, weights, fun=median)
  #now smooth it slightly with a mean weighted primarily by the middle pixel
  middle_pixel = ceiling(pixels_smooth_3/2)
  weights[middle_pixel,middle_pixel] = 1*length(weights)
  weights = weights/(mean(weights))
  chm_smooth_7 = focal(chm_smooth_7, weights, fun=mean)
  
  weights = matrix(1,nrow=pixels_smooth_4,ncol=pixels_smooth_4)
  chm_smooth_8 = focal(chm, weights, fun=median)
  #now smooth it slightly with a mean weighted primarily by the middle pixel
  middle_pixel = ceiling(pixels_smooth_4/2)
  weights[middle_pixel,middle_pixel] = 1*length(weights)
  weights = weights/(mean(weights))
  chm_smooth_8 = focal(chm_smooth_8, weights, fun=mean)
  
  a = future_pmap(params %>% select(-method), vwf_singlechm_singleparamset , chm=chm, chm_smooth_1 = chm_smooth_1, chm_smooth_2 = chm_smooth_2, chm_smooth_3 = chm_smooth_3, chm_smooth_4 = chm_smooth_4, chm_smooth_5 = chm_smooth_5, chm_smooth_6 = chm_smooth_6, chm_smooth_7 = chm_smooth_7, chm_smooth_8 = chm_smooth_8, layer_name = chm_layer_name)

}



