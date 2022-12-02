## Takes a CHM and makes a map of treetops

library(sf)
library(raster)
library(ForestTools)
library(here)
library(purrr)
library(furrr)
library(tidyverse)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))
source(here("scripts/tree-detection/vwf_functions.R"))




### Define parameter values to search: only need to run if set defs change

params = read_csv(data("parameter_set_definitions/vwfdefs_fullrange.csv"))

# ## Keep just the focal detection sets
# params = params %>%
#   filter(detection_params_name %in% c("vwf_196", "vwf_186", "vwf_185", "vwf_197", "vwf_176", "vwf_120","vwf_121", "vwf_207", "vwf_109"))

# ## Keep just the focal detection sets
# params = params %>%
#   filter(detection_params_name %in% c("vwf_196", "vwf_186", "vwf_120","vwf_109"))


## Keep just the focal detection sets: this was after determined the best ones by running all the 2_2 thins (the first round)
# The sets selected after downscaling
# params = params %>%
#   filter(detection_params_name %in% c("vwf_113", "vwf_121", "vwf_122", "vwf_120", "vwf_185", "vwf_196", "vwf_110", "vwf_109"))


# Run for multiple CHMs

# If running manually, specify paramset names
manual_paramset_names = NULL
# manual_paramset_names = c("paramset98_030","paramset99_030")

# read paramset from command line argument (otherwise use the hard-coded default above, or use all paramsets via code below)
command_args = commandArgs(trailingOnly=TRUE)

if(length(command_args) == 0) {
  if(!is.null(manual_paramset_names)) {
    paramset_names = manual_paramset_names
  } else { # pull from directory
    
    chm_files = list.files(data("metashape_outputs_postprocessed/chm"),pattern="chm\\.tif", full.names=TRUE)
    
    # get all the filenames from before the date
    pieces = str_split(chm_files,"/")
    filenames = map(pieces,sapply(pieces,length)[1]) %>% unlist
    pre_dates_part1 = filenames %>% str_split("_") %>% map(1) %>% unlist
    pre_dates_part2 = filenames %>% str_split("_") %>% map(2) %>% unlist
    pre_dates = paste(pre_dates_part1,pre_dates_part2,sep="_")
    
    ## Filter out the 5xxx and 6xxx metashape sets because those are the pre-downscaled ones (the downscaled ones are prefixed with 1)
    
    pre_dates = pre_dates[!(as.numeric(pre_dates_part2 %>% as.character) >= 5000 & as.numeric(pre_dates_part2 %>% as.character) <= 6999)]

    paramset_names = unique(pre_dates)
    
  }
  
} else if (length(command_args) > 0) {
  paramset_names = command_args[1]
}


# Ramdomize paramset names so can run multiple parallel
paramset_names = paramset_names %>% sample()

### Run the search
options(future.globals.maxSize=15000*1024^2) # 15 GB
plan(multiprocess)
walk(paramset_names,.f = vwf_singlechm_multiparamset, parallelize_params = TRUE, params = params)
