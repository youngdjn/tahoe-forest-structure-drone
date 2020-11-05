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
source(here("scripts/tree_detection/vwf_functions.R"))




### Define parameter values to search: only need to run if set defs change

existing_params = read_csv(data("parameter_set_definitions/vwfdefs_quadraticsearch1.csv"))

c = seq(-0.002, 0.001, by = 0.0005)
b = seq(0.03,0.1, by = 0.01)
a = seq(-2.5, 0.5, by= 0.5)
smooth = c(1)

params = expand.grid(a=a,b=b,c=c,smooth=smooth)

a = anti_join(params,existing_params)

# # remove sets where both a and b are zero
# params = params %>%
#   dplyr::filter(!(a == 0 & b == 0))

params$detection_params_name = paste0("vwf_",str_pad(1:nrow(params)+2000, width=4, pad = "0"))

params$method = "vwf"

params = bind_rows(params, existing_params)


# save vwf params
write_csv(params,data("parameter_set_definitions/vwfdefs_quadraticsearch1and2.csv"))


# Run for multiple CHMs

# If running manually, specify paramset name
manual_paramset_names = c("paramset14_01")

# read paramset from command line argument (otherwise use the hard-coded default above)
command_args = commandArgs(trailingOnly=TRUE)

if(length(command_args) == 0) {
  paramset_names = manual_paramset_names
} else if (length(command_args) > 0) {
  paramset_names = command_args[1]
}


### Run the search 
plan(multiprocess)
walk(paramset_names,.f = vwf_singlechm_multiparamset, parallelize_params = TRUE, params = params)


