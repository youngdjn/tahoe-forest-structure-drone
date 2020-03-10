## Export a photoset clipped to the boundaries of a provided shapefile

## TODO: tidy up script

library(tidyverse)
library(exifr)
library(sf)
library(here)


# Load the boundary
bounds = st_read("C:/Users/DYoung/Box/projects/tahoe-forest-structure-drone_data/study_area_perimeter/smaller_mission_mask.geojson")



#### Parameters to set for each run (specific to a given photo set) ####

# Top-level folder of all mission images to be clipped. Do not include trailing slash.
photoset_path_start = "C:/Users/DYoung/Box/projects/uav_data/imagery/missions_thinned/"

# Path to save the cilpped photoset to. Exclude the actual photoset folder(s) as they will be appended to the path provided here. Do not include trailing slash.
destination_path_start = "C:/Users/DYoung/Box/projects/uav_data/imagery/missions_thinned/"




photosets = data.frame(photoset_name = c("set14_120m_95_95_nadir_0ev_thin12","set14_120m_95_95_nadir_0ev_thin21","set14_120m_95_95_nadir_0ev_thin22","set14_120m_95_95_nadir_0ev_thin24","set14_120m_95_95_nadir_0ev_thin42","set14_120m_95_95_nadir_0ev_thin44"),
                       destination = c("set14b_thin12","set14b_thin21","set14b_thin22","set14b_thin24","set14b_thin42","set14b_thin44"))


photosets = photosets %>%
  mutate(photoset_name = paste0(photoset_path_start,photoset_name),
         destination = paste0(destination_path_start,destination))


for(i in 1:nrow(photosets)) {

  photoset_path = photosets[i,"photoset_name"]
  destionation_path = photosets[i,"destination"]
  
  
  
  source(here("scripts/convenience_functions.R"))
  
  
  ## Find all original drone photos (use regex to search for DJI photos in case there are some non-drone photos in the folder)
  photo_files = list.files(photoset_path,recursive=TRUE,pattern="DJI_[0-9]{4}.JPG",full.names = TRUE)
  
  get_last_2_and_join = function(x) {
    last_2 = (x[(length(x)-1):length(x)])
    joined = paste(last_2,collapse = "/")
  }
  
  photo_folder_file = str_split(photo_files,"/") %>% map(get_last_2_and_join) %>% unlist
  
  d_exif = read_exif(photo_files , tags = c("ImageDescription","GPSLatitude","GPSLongitude","CreateDate"))
  
  d = d_exif %>%
    select(ImageDescription,GPSLatitude,GPSLongitude,CreateDate) %>%
    #separate(ImageDescription,c(NA,"Folder","File"), sep = "\\\\") %>%
    mutate(Folder_File = photo_folder_file) %>%
    arrange(CreateDate,Folder_File)    # sort by time and then by name (in case any were from the same exact time, the name should increment)
  
  
  d_sp = st_as_sf(d,coords=c("GPSLongitude","GPSLatitude"), crs=4326)
  
  ## Convert to meter units (CA Albers projection)
  d_sp = st_transform(d_sp,3310)
  
  ## Get x and y coords
  d_sp = cbind(d_sp,st_coordinates(d_sp))
  
  d_coords = d_sp
  st_geometry(d_coords) = NULL # remove geom (convert to normal data frame)
  
  
  
  
  
  #### Save only those photos falling in bounds ####
  
  d_sp_in = st_intersection(d_sp,bounds %>% st_transform(st_crs(d_sp)))
  
  
  
  
  
  
  
  #### Generate thinned photoset copies ####
  
    
  ## Copy thinned set to destination path
  
  # Get the necessary paths
  photos = d_sp_in %>%
    mutate(source_path = paste0(photoset_path,"/",Folder_File),
           dest_path = paste0(destination_path,"/",Folder_File)) %>%
    mutate(dest_directory = dest_path %>% map(path_drop_last) %>% unlist ) # this is the destination file without the file at the end: for creating the directory for it via dir.create below
  
  # Make sure all needed directories exist
  dest_directories = unique(photos$dest_directory)
  walk(dest_directories,dir.create,recursive=TRUE)
  
  file.copy(photos$source_path,photos$dest_path, overwrite=FALSE)

}
