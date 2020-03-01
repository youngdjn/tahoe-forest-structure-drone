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


### Make plots spatial and export to determine which set of coords is better. ###

plots_sp <- st_as_sf(plots, coords = c("Easting","Northing"), crs = 32610)
st_write(plots_sp %>% st_transform(4326), "temp/plots_raw.geojson", delete_dsn=TRUE)





trees = trees %>%
  mutate(ground_tree_id = 1:nrow(trees)) %>%
  mutate(Distance = as.numeric(Distance)) %>%
  filter(is.na(REMEASURED)) %>%
  filter(ground_tree_id != 1212) %>%
  filter(!(Distance %in% c("?","NA")))

plots = plots %>%
  filter(!is.na(Data_collection_location_1))


## Check for duplicated plots
cbind(plots$Plot,duplicated(plots$Plot))

## Check for duplicated trees
trees$duplicated = duplicated( trees %>%  select(Plot,data_col_location,Status,Species,DBH,Height) )



#### Map plot centers ####

plot_centers_sp <- st_as_sf(plots, coords = c("Easting","Northing"), crs = 32610)

st_write(plot_centers_sp %>% st_transform(4326),data("ground_truth/field_data/plot_centers.geojson"),delete_dsn=TRUE)






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


#### Give plot loc names easy-to-read numbers, and transfer to trees ####

plots_locs$PlotID_simple = 1:nrow(plots_locs)

plots_locs = plots_locs %>%
  mutate(Plot_Loc = paste(Plot,loc,sep="_"))

plots_names = plots_locs %>%
  select(PlotID_simple,Plot_Loc)
trees_locs = trees_locs %>%
  mutate(Plot_Loc = paste(Plot,data_col_location,sep="_"))

trees_locs = left_join(trees_locs,plots_names)



#### Convert to spatial and write ####

## Trees
# need to drop trees without coordinates (they exist because of incomplete survey data)
trees_locs = trees_locs %>%
  filter(!is.na(Easting))

trees_sp <- st_as_sf(trees_locs, coords = c("Easting","Northing"), crs = 32610)

st_write(trees_sp %>% st_transform(4326),data("ground_truth/field_data/ept_trees_05.geojson"),delete_dsn=TRUE)


## Plots
plots_sp <- st_as_sf(plots_locs, coords = c("col_loc_easting","col_loc_northing"), crs = 32610)

st_write(plots_sp %>% st_transform(4326),data("ground_truth/field_data/ept_plots_05.geojson"),delete_dsn=TRUE)


