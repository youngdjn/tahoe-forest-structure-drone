#### Convert Weeks' Emerald Point ground-truth survey data into GeoJSON
# Author: Derek Young

data_dir = "C:/Users/DYoung/Box/projects/tahoe-forest-structure-drone_data/"

library(tidyverse)
library(readxl)
library(sf)
library(here)

#### Conveinence functions ####

source(here("scripts/convenience_functions.R"))



#### Load and clean data ####

trees = read_excel(data("ground_truth/field_data/EPT_tree_data.xlsx"),sheet=1)
plots = read_excel(data("ground_truth/field_data/EPT_tree_data.xlsx"),sheet=2)

trees = trees %>%
  mutate(Distance = as.numeric(Distance))

plots = plots %>%
  filter(!is.na(Data_collection_location_1))


## Check for duplicated plots
cbind(plots$Plot,duplicated(plots$Plot))

## Check for duplicated trees
trees_simp = trees %>%
  select(Plot,data_col_location,Status,Species,DBH,Height)

trees_simp$duplicated = duplicated(trees_simp)


## TEMPORARY exclude plot B2 because it has incomplete tree data
trees = trees %>%
  filter(Plot != "B2")

## TEMPORARY exclude the second plot By2.75 because it is duplicated
plots = plots %>%
  filter(!(Plot == "By2.75" & `Data_collection_location_2_distance_from plot center_m` == 11.1))



#### For each plot, get coords of each data collection location ####

plots_locs = data.frame()

for(i in 1:nrow(plots)) {
  
  plot = plots[i,]  
  trees_plot = trees %>%
    filter(Plot == plot$Plot)
  
  locs = unique(trees_plot$data_col_location)
  
  for(loc in locs) {

    
    if(loc == 1) {
      
      if(plot$Data_collection_location_1 != "plot center") {
        cat("Data collection location 1 is not plot center for plot",plot$Plot,"\n")
      }
      
      loc_x = plot$Easting
      loc_y = plot$Northing
      
      plot_loc = data.frame(Plot = plot$Plot,
                            loc = loc,
                            col_loc_easting = loc_x,
                            col_loc_northing = loc_y)
      
      plots_locs = rbind(plots_locs,plot_loc)
      
    } else {
      
      loc_distance_column_name = paste0("Data_collection_location_",loc,"_distance_from plot center_m")
      loc_distance_column_number = which(names(plot) == loc_distance_column_name)
      if(is.null(loc_distance_column_number)) {
        cat("No column properly named for distance for plot: ",plot$Plot,", location: ",loc)
      }
      
      loc_azimuth_column_name = paste0("Data_collection_location_ ",loc,"_Azimuth_from_plot_center(corrected for declination)")
      loc_azimuth_column_number = which(names(plot) == loc_azimuth_column_name)
      if(is.null(loc_azimuth_column_number)) {
        cat("No column properly named for azimuth for plot: ",plot$Plot,", location: ",loc)
      }
      
      loc_azimuth = plot[,loc_azimuth_column_number]
      loc_distance = plot[,loc_distance_column_number]
      
      loc_x = plot$Easting + sin(deg2rad(loc_azimuth %>% as.numeric())) * loc_distance %>% as.numeric()
      loc_y = plot$Northing + cos(deg2rad(loc_azimuth %>% as.numeric())) * loc_distance %>% as.numeric()
      
      plot_loc = data.frame(Plot = plot$Plot,
                            loc = loc,
                            col_loc_easting = loc_x %>% as.numeric,
                            col_loc_northing = loc_y %>% as.numeric) 
      
      plots_locs = rbind(plots_locs,plot_loc)
    }
  }
}




#### Pull in the plot data colleciton location info for each tree to simplify the mapping step ####
trees_col_locs = left_join(trees,plots_locs,by=c("Plot"="Plot","data_col_location"="loc"))



#### Get the coords of each tree ####
trees_locs = trees_col_locs %>%
  mutate(Easting = col_loc_easting + sin(deg2rad(Azimuth_converted)) * Distance,
         Northing = col_loc_northing + cos(deg2rad(Azimuth_converted)) * Distance)
  

#### Convert to spatial ####

# need to drop trees without coordinates (they exist because of incomplete survey data)
trees_locs = trees_locs %>%
  filter(!is.na(Easting))


trees_sp <- st_as_sf(trees_locs, coords = c("Easting","Northing"), crs = 32610)

st_write(trees_sp %>% st_transform(4326),data("ground_truth/field_data/ept_trees.geojson"),delete_dsn=TRUE)
