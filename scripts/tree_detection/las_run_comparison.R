## Takes a las and makes a map of treetops following different methods

library(sf)
library(lidR)
library(lidRplugins)
library(here)
library(purrr)
library(furrr)
library(tidyverse)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))
source(here("scripts/tree_detection/las_functions.R"))




### Define parameter values to search: only need to run if set defs change

params = read_csv(data("parameter_set_definitions/best_las.csv"))
# params$detection_params_name = paste0("las_",str_pad(1:nrow(params)+9000, width=4, pad = "0"))
params$detection_params_name = params$config_name
params$method_general = "las"



# If running manually, specify paramset names
manual_paramset_names = NULL
# manual_paramset_names = c("paramset14b_04","paramset14_02","paramset14_03")

# read paramset from command line argument (otherwise use the hard-coded default above)
command_args = commandArgs(trailingOnly=TRUE)

if(length(command_args) == 0) {
  if(!is.null(manual_paramset_names)) {
    paramset_names = manual_paramset_names
  } else { # pull from directory
    
    las_files = list.files(data("metashape_outputs_postprocessed/las"),pattern="points\\.laz", full.names=TRUE)
    
    # get all the filenames from before the date
    pieces = str_split(las_files,"/")
    filenames = map(pieces,sapply(pieces,length)[1]) %>% unlist
    pre_dates_part1 = filenames %>% str_split("_") %>% map(1) %>% unlist
    pre_dates_part2 = filenames %>% str_split("_") %>% map(2) %>% unlist
    pre_dates = paste(pre_dates_part1,pre_dates_part2,sep="_")
    
    
    paramset_names = unique(pre_dates)
    
  }
  
} else if (length(command_args) > 0) {
  paramset_names = command_args[1]
}

# Ramdomize paramset names so can run multiple parallel
paramset_names = paramset_names %>% sample()

### Run the search
set_lidr_threads(1)
options(lidR.progress = FALSE)
options(future.globals.maxSize=20000*1024^2) # 100 GB

plan(multiprocess(workers=3))
  
future_map(paramset_names,.f = las_singlelas_allparams, params = params, .options=future_options(scheduling=Inf))
