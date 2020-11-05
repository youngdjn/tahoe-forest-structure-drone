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

b = seq(0.00, 0.06, by=0.01)
a = seq(0.0, 1, by = 0.1)
smooth = c(0,1,2,3,4,5,6,7,8,9,10)

params = expand.grid(a=a,b=b,smooth=smooth)

# remove sets where both a and b are zero
params = params %>%
  dplyr::filter(!(a == 0 & b == 0))

params$detection_params_name = paste0("vwf_",str_pad(1:nrow(params), width=3, pad = "0"))

params$method = "vwf"

# save vwf params
write_csv(params,data("parameter_set_definitions/vwfdefs_fullrange.csv"))



# ### Define parameter values to search (simple version)
# 
# b = seq(0.05, 0.05, by=0.02)
# a = seq(0.6, 0.8, by = 0.2)
# smooth = c(0,2)
# 
# params = expand.grid(a=a,b=b,smooth=smooth)
# 
# # remove sets where both a and b are zero
# params = params %>%
#   filter(!(a == 0 & b == 0))
# 
# params$detection_params_name = paste0("vwf_",str_pad(1:nrow(params)+500, width=3, pad = "0"))
# 
# params$method = "vwf"
# 
# # save vwf params
# write_csv(params,data("parameter_set_definitions/vwfdefs_simp.csv"))

# # load vwf params
# params = read_csv(data("parameter_set_definitions/vwfdefs_simp.csv"))





# Run for multiple CHMs
plan(multiprocess)

paramset_names = c("paramset15a_02", "paramset26b_01")

walk(paramset_names,.f = vwf_singlechm_multiparamset, params = params)


