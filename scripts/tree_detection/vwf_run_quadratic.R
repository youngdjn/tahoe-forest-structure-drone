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

dynamicWindow_pines <- function(x) {
  window_radius <- 0.5 * (3.75105 - 0.17919*x + 0.01241 * x^2)
  window_radius
}

c = seq(-0.002, 0.002, by = 0.001)
b = seq(-0.02, 0.02, by=0.01)
a = seq(-1, 1, by = 0.5)
smooth = c(1)

params = expand.grid(a=a,b=b,c=c,smooth=smooth)

# # remove sets where both a and b are zero
# params = params %>%
#   dplyr::filter(!(a == 0 & b == 0))

params$detection_params_name = paste0("vwf_",str_pad(1:nrow(params)+1000, width=4, pad = "0"))

params$method = "vwf"

# save vwf params
write_csv(params,data("parameter_set_definitions/vwfdefs_quadraticsearch1.csv"))


# Run for multiple CHMs
plan(multiprocess)

paramset_names = c("paramset14_01")

walk(paramset_names[1],.f = vwf_singlechm_multiparamset, parallelize_params = TRUE, params = params)


