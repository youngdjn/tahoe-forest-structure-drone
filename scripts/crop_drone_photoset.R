#### Began to prepare for use but never actually used. Almost complete but the last 3 lines need to be adjusted!


library(tidyverse)
library(exifr)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Parameters to set for each run (specific to a given photo set) ####

# Top-level folder of all mission images. Do not include trailing slash.
photoset_path = "/home/derek/Documents/data/uav_data/missions_thinned/set14_thin44"

# Path to save the cropped photoset to. Exclude the actual photoset folder(s) as they will be appended to the path provided here. Do not include trailing slash.
destination_path = "/home/derek/Documents/data/uav_data/missions_thinned/temp"

# Name to prepend to all cropped sets based on this photoset
photoset_name = "set27btest"

#### Convenience functions ####

source(here("scripts/convenience_functions.R"))


#### Load focal perimeter
perim = st_read(data("study_area_perimeter/ground_map_mask_precise.geojson")) %>% st_transform(3310) %>% st_buffer(84*1.5)


#### Assign transect IDs to photos ####
#### This is to allow thinning by transect


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


## Make it spatial
d_sp = st_as_sf(d,coords=c("GPSLongitude","GPSLatitude"), crs=4326)

## Convert to meter units (CA Albers projection)
d_sp = st_transform(d_sp,3310)


## See which ones are in the perimeter
d_in = st_intersection(d_sp,perim)






#### Generate thinned photoset copies ####



thinned_photoset_name = paste0(photoset_name,"_crop")

## Copy thinned set to destination path

# Get the necessary paths
d_in = d_in %>%
  mutate(source_path = paste0(photoset_path,"/",Folder_File),
         dest_path = paste0(destination_path,"/",thinned_photoset_name,"/",Folder_File)) %>%
  mutate(dest_directory = dest_path %>% map(path_drop_last) %>% unlist ) # this is the destination file without the file at the end: for creating the directory for it via dir.create below

# Make sure all needed directories exist
dest_directories = unique(d_in$dest_directory)
walk(dest_directories,dir.create,recursive=TRUE)

file.copy(photos$source_path,photos$dest_path, overwrite=FALSE)

