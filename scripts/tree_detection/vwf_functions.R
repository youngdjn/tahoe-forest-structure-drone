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

vwf_singlechm_singleparamset = function(chms, layer_name, a, b, c = 0, smooth, detection_params_name) {
  
  cat("Running for:", layer_name, detection_params_name,"\n" )
  
  ### see if file name already exists, and if so, skip
  file_name = paste0(layer_name,"-",detection_params_name,".geojson")
  dir = data(paste0("detected_trees"))
  if(file.exists(paste0(dir,"/",file_name))) {  
    cat("Already exists:",layer_name, detection_params_name,"\n")
    return()
  }

  if(smooth == 0) {
    chm = chms[["smooth0"]]
  } else if(smooth == 1) {
    chm = chms[["smooth1"]]
  } else if(smooth == 2) {
    chm = chms[["smooth2"]]
  } else if(smooth == 3) {
    chm = chms[["smooth3"]]
  } else if(smooth == 4) {
    chm = chms[["smooth4"]]
  } else if(smooth == 5) {
    chm = chms[["smooth5"]]
  } else if(smooth == 6) {
    chm = chms[["smooth6"]]
  } else if(smooth == 7) {
    chm = chms[["smooth7"]]
  } else if(smooth == 8) {
    chm = chms[["smooth8"]]
  } else {
    stop("Requested smoothed chm",smooth,"not provided.")
  }
  

  lin <- function(x){x^2*c + x*b + a} # window filter function to use in next step
  
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
  
  dir = data(paste0("detected_trees"))
  dir.create(dir)
  st_write(treetops,paste0(dir,"/",file_name), delete_dsn=TRUE, quiet=TRUE)

}


vwf_singlechm_multiparamset = function(chm_layer_name, params = paramsets, parallelize_params = parallelize_params) {

  #### Load data ####
  
  # find the dsm file in the metashape products direcotry
  chm_files = list.files(data("metashape_products/chm"),pattern=chm_layer_name, full.names=TRUE)
  if(length(chm_files) > 1) {
    
    cat("More than 1 matching CHM file in the specified metashape data products folder for",chm_layer_name,". Using most recent.\n") 
    
    # get the date
    pieces = str_split(chm_files,"/")
    filenames = map(pieces,sapply(pieces,length)[1]) %>% unlist
    dates = filenames %>% str_split("_") %>% map(3) %>% unlist
    # which date is later?
    latest = which(order(dates) == max(order(dates)))
    chm_file = chm_files[latest]
    
  } else if(length(chm_files) == 0) {
      stop("No matching CHM files int he specified metashape data products folder.")
  } else {
    chm_file = chm_files
  }
  
  chm = raster(chm_file)
  
  ## Get the CHM layer name from the chm_file
  pieces = str_split(chm_file,"/")
  filename = map(pieces,sapply(pieces,length)[1]) %>% unlist
  filename_noextension = str_split(filename,fixed(".")) %>% map(1) %>% unlist()
  chm_layer_name = filename_noextension
  
  
  ### Create the needed smoothed rasters
  
  smooths = unique(params$smooth)
  
  chm_res = res(chm) %>% mean
  pixels_smooth_1 = round(((0.5/chm_res)-1)/2)*2 + 1 # round to nearest odd integer
  pixels_smooth_2 = round(((1/chm_res)-1)/2)*2 + 1
  pixels_smooth_3 = round(((1.5/chm_res)-1)/2)*2 + 1
  pixels_smooth_4 = round(((2/chm_res)-1)/2)*2 + 1
  
  chms = list()
  
  if(0 %in% smooths) {
  chms[["smooth0"]] = chm
  }
  
  if(1 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_1,ncol=pixels_smooth_1)
    chms[["smooth1"]] = focal(chm, weights, fun=mean)
  }
  
  if(2 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_2,ncol=pixels_smooth_2)
    chms[["smooth2"]] = focal(chm, weights, fun=mean)
  }
  
  if(3 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_3,ncol=pixels_smooth_3)
    chms[["smooth3"]] = focal(chm, weights, fun=mean)
  }
  
  if(4 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_4,ncol=pixels_smooth_4)
    chms[["smooth4"]] = focal(chm, weights, fun=mean)
  }
  
  if(5 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_1,ncol=pixels_smooth_1)
    chm_smooth_5 = focal(chm, weights, fun=median)
    #now smooth it slightly with a mean weighted primarily by the middle pixel
    middle_pixel = ceiling(pixels_smooth_1/2)
    weights[middle_pixel,middle_pixel] = 10*length(weights)
    weights = weights/(mean(weights))
    chms[["smooth5"]] = focal(chm_smooth_5, weights, fun=mean)
  }
  
  if(6 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_2,ncol=pixels_smooth_2)
    chm_smooth_6 = focal(chm, weights, fun=median)
    #now smooth it slightly with a mean weighted primarily by the middle pixel
    middle_pixel = ceiling(pixels_smooth_2/2)
    weights[middle_pixel,middle_pixel] = 10*length(weights)
    weights = weights/(mean(weights))
    chms[["smooth6"]] = focal(chm_smooth_6, weights, fun=mean)
  }
  
  if(7 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_3,ncol=pixels_smooth_3)
    chm_smooth_7 = focal(chm, weights, fun=median)
    #now smooth it slightly with a mean weighted primarily by the middle pixel
    middle_pixel = ceiling(pixels_smooth_3/2)
    weights[middle_pixel,middle_pixel] = 10*length(weights)
    weights = weights/(mean(weights))
    chms[["smooth7"]] = focal(chm_smooth_7, weights, fun=mean)
  }
  
  if(8 %in% smooths) {
    weights = matrix(1,nrow=pixels_smooth_4,ncol=pixels_smooth_4)
    chm_smooth_8 = focal(chm, weights, fun=median)
    #now smooth it slightly with a mean weighted primarily by the middle pixel
    middle_pixel = ceiling(pixels_smooth_4/2)
    weights[middle_pixel,middle_pixel] = 10*length(weights)
    weights = weights/(mean(weights))
    chms[["smooth8"]] = focal(chm_smooth_8, weights, fun=mean)
  }
  
  if(parallelize_params) {
    a = future_pmap(params %>% dplyr::select(-method) %>% sample_frac(), vwf_singlechm_singleparamset , chms=chms, layer_name = chm_layer_name, .options = future_options(scheduling=Inf))
  } else {
    a = pmap(params %>% dplyr::select(-method), vwf_singlechm_singleparamset , chms=chms, layer_name = chm_layer_name)
    
  }
}



