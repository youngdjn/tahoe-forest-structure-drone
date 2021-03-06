library(tidyverse)
library(exifr)
library(sf)
library(here)


#### Parameters to set for each run (specific to a given photo set) ####

# Top-level folder of all mission images. Do not include trailing slash.
photoset_path = "/storage/temp/27_EmPo_90_90_90m_25deg_-03ev_merged"

# Path to save the thinned photoset to. Exclude the actual photoset folder(s) as they will be appended to the path provided here. Do not include trailing slash.
destination_path = "/storage/temp/thinned"

# Name to prepend to all thinned sets based on this photoset
photoset_name = "set27b"

## Should transects be thinned in pairs? Use for angled-gimbal photosets.
transect_pairs = TRUE

# Specify manual stringer images (images that MapPilot collects along the project boundary when moving from one transect to the next) to exclude if they're not picked up by the algorithm
# for 15a: manual_stringer_photos = c("2019:09:10 11:12:42","2019:09:10 11:12:44","2019:09:10 11:12:47","2019:09:10 11:12:49","2019:09:10 11:12:52")
# for 16: manual_stringer_photos = c("2019:09:11 11:34:10","2019:09:11 12:01:49","2019:09:11 12:02:45","2019:09:11 12:02:46","2019:09:11 12:03:39","2019:09:11 11:34:10","2019:09:11 11:12:05","2019:09:11 11:12:12","2019:09:11 11:12:13","2019:09:11 11:12:15","2019:09:11 11:12:18","2019:09:11 11:15:08","2019:09:11 11:15:10","2019:09:11 11:15:18","2019:09:11 11:12:07","2019:09:11 11:15:19","2019:09:11 11:12:19")
# for 26: manual_stringer_photos = c("2019:09:11 14:47:32","2019:09:11 14:47:29","2019:09:11 14:47:27","2019:09:11 14:47:25","2019:09:11 14:47:22","2019:09:11 14:47:19","2019:09:11 14:47:17","2019:09:11 14:47:15","2019:09:11 14:47:11")
# for 21: manual_stringer_photos = c("2019:09:12 11:01:47", "2019:09:12 11:01:49", "2019:09:12 11:01:51", "2019:09:12 11:01:54")
# for 22: manual_stringer_photos = NULL
# for 19: manual_stringer_photos = NULL
# for 20: manual_stringer_photos = NULL
# for 27:
manual_stringer_photos = c("2019:09:12 11:03:21","2019:09:12 11:03:22","2019:09:12 11:01:47","2019:09:12 11:01:49","2019:09:12 11:01:51","2019:09:12 11:01:54")

#manual_stringer_photos = NULL

# How many degrees (angle) change in transect path signals a new transect?
change_thresh = 8

## Stringer detection:
# Within how many degrees (in terms of the orientation of the transect) does a focal transect have to be from other transects to not be considered a stringer
tolerance = 3
# What proportion of other transects have to be within the tolerance for the focal transect to not be considered a stringer transect?
proportion_matching_threshold = .2

# Min photos per transect to consider it a transect
# for 27
min_photos = 6

# for rest
min_photos = 4

## Specify thinning factors (forward then side, one row per thinned set)
thins = matrix(c(#1,1,
                 #1,2,
                 #2,1,
                 #2,2
                 2,4,
                 4,2,
                 4,4
                 ),
               ncol=2,
               byrow=TRUE)


#### Convenience functions ####

source(here("scripts/convenience_functions.R"))




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


### Delete repeat photo locations (they seem to only be at the end of transects when the drone is pivoting to the next angle)
d$d_repeat = d %>%
  select(GPSLatitude,GPSLongitude) %>%
  duplicated()
d = d[!d$d_repeat,]

## filtering for set 16
d = d %>%
  filter(CreateDate != "2019:09:11 11:34:10")

## Make it spatial

d_sp = st_as_sf(d,coords=c("GPSLongitude","GPSLatitude"), crs=4326)

## Convert to meter units (CA Albers projection)
d_sp = st_transform(d_sp,3310)

## Get x and y coords
d_sp = cbind(d_sp,st_coordinates(d_sp))

d_coords = d_sp
st_geometry(d_coords) = NULL # remove geom (convert to normal data frame)

## Compute the angle from each point to the next point
for(i in 1:(nrow(d_coords)-1)) {
  
  focal_x = d_coords[i,"X"]
  focal_y = d_coords[i,"Y"]
  next_x = d_coords[i+1,"X"]
  next_y = d_coords[i+1,"Y"]
  
  x_dist = next_x - focal_x
  y_dist = next_y - focal_y
  
  hypotenuse = sqrt(x_dist^2 + y_dist^2)
  
  angle = atan2(   x_dist,y_dist  ) %>% rad2deg %>% abs # angle from one point to next
  #angle = ifelse(x_dist < 0, angle + 180,angle)
  
  # Some times two photo points are directly on top of each other (DJI bug?), so consider them for transect-delineation purposes to be in the same transect. To do this need to save a code as -1 which the angle-difference computing script looks for
  if(x_dist == 0 & y_dist == 0) {
    angle = -1
  }
  
  d_coords[i,"angle"] = angle
  
}

## Get average angle
avg_angle = mean(d_coords$angle,na.rm=TRUE)

## Compute change in angle from one point to the next
for(i in 2:(nrow(d_coords))) {
  
  last_angle = d_coords[i-1,"angle"]
  current_angle = d_coords[i,"angle"]
  
  angle_change = 360-(360-abs(current_angle-last_angle))
  
  if (is.na(current_angle) | current_angle == -1 | last_angle == -1) {
    angle_change = 0
  }
  
  d_coords[i,"angle_change"] = angle_change
  
}


## Give a unique ID to every string of plots with < X degrees angle change from one photo to the next

transect_id = 1 # starting value
just_incremented = FALSE # variable to keep track of whether we just incremented the transect ID (hit a new transect). if increment twice in a row, it's the end of a transect and we shouldn't increment the second time

for(i in 1:nrow(d_coords)) {
  
  d_coords[i,"transect_id"] = transect_id
  
  point = d_coords[i,]
  
    if(is.na(point$angle_change)) next()
  
  if(point$angle_change > change_thresh) {  # if the next point is a large angle different from the current point, increment transect ID so the next point is assigned to a new transect
    if(just_incremented) { # we incremented on the previous point and also this point, so it's the end of a transect so we shouldn't increment for this point
      just_incremented = FALSE
    } else {
      transect_id = transect_id + 1
      just_incremented = TRUE
    }

  } else {
    just_incremented = FALSE
  }
  
}

## Eliminate the stringers of points that MapPilot places along perimeter of flight area when going from one transect to the next
## Get average angle of each transect. Count number of transects with average angle within 3 degrees. If < 10% of transects are within 3 degree, it's a stringer

transect_summ = d_coords %>%
  filter(angle != -1) %>%  # Don't include points with no angle (two points on top of each other)
  group_by(transect_id) %>%
  slice(2:n()) %>% # drop the first photo of each group because it probably has a crazy angle
  slice(1:(n()-1)) %>% # drop the last row of each group because it could have a crazy angle
  summarize(avg_angle = mean(angle),
            n_photos = n())

# only compare against transects that are not very short (probably MapPilot edge stringers)
transects_long = transect_summ %>%
  filter((n_photos + 2) > min_photos)

n_transects = nrow(transects_long)


for(i in 1:nrow(transect_summ)) {
  
  transect = transect_summ[i,]
  angle = transect$avg_angle
  transect_id = transect$transect_id
  
  lower_bound = (angle-tolerance)
  upper_bound = (angle+tolerance)
  
  matching_transects = transects_long %>%
    filter((avg_angle < upper_bound | avg_angle < (upper_bound %% 360)) &
             avg_angle > lower_bound | avg_angle > (lower_bound %% 360))
  
  n_matching = nrow(matching_transects)
  
  proportion_matching = n_matching/n_transects
  
  if(proportion_matching < proportion_matching_threshold | (transect$n_photos + 2) < min_photos) {
    d_coords[d_coords$transect_id == transect_id,"stringer"] = TRUE
  } else {
    d_coords[d_coords$transect_id == transect_id,"stringer"] = FALSE
  }
  
}


# assign manual stringer photos
d_coords[d_coords$CreateDate %in% manual_stringer_photos,"stringer"] = TRUE

## give each point the mean x and y coordinate of all its photos, also the mean angle
transect_summ = d_coords %>%
  group_by(transect_id) %>%
  summarize(mean_x_coord = mean(X),
            mean_y_coord = mean(Y),
            mean_angle = mean(angle))

d_coords = left_join(d_coords,transect_summ)

# ## Assign new transect IDs, but only to non-stringer transects, and do it by incrementing transects based on their average x coordinate 
# d_coords = d_coords %>%
#   arrange(mean_x_coord,CreateDate,Folder_File)

## Alternatively, when transects not N-S, assign transect IDs just by CreateDate. This means all must be consecutive. 
d_coords = d_coords %>%
  arrange(CreateDate,Folder_File)




#### !!!! here need to loop through each d_coord.
# whenver hit a new transect_id, increment the count. assign the count as sorted_transect_id

## to use for assigning paired transects the same ID for purposes of thinning angled gimbal datasets

transect_id_lookup = rep(1:1000,each=2)

transect_ids_encountered = NULL

for(i in 1:nrow(d_coords)) {
  
  #if(i == 82) { browser() }
  
  photo = d_coords[i,]
  if(photo$stringer) next()
  
  transect_id = photo$transect_id
  
  if(!(transect_id %in% transect_ids_encountered)) {
    transect_ids_encountered = c(transect_ids_encountered,transect_id)
  }
  
  if(!transect_pairs) {
    d_coords[i,"transect_id_new"] = length(transect_ids_encountered)
  } else {
    d_coords[i,"transect_id_new"] = transect_id_lookup[length(transect_ids_encountered)]
  }
  
}


# # redo numbering for only 
# d_coords[!d_coords$stringer,"transect_id_new"] = d_coords[!d_coords$stringer,] %>% group_indices(transect_id)

d_coords = d_coords %>%
  mutate(odd_transect_new = (transect_id_new %% 2))

## Assign incrementing photo IDs
d_coords$photo_id = 1:nrow(d_coords)




## Make it spatial again for checking results on a map
d_tsect_sp = st_as_sf(d_coords,coords=c("X","Y"), crs=3310)

plot(d_tsect_sp)

st_write(d_tsect_sp %>% st_transform(4326), "/storage/temp/temp_transect_eval.geojson",delete_dsn=TRUE)



#### Generate thinned photoset copies ####

# copy photo  sets with specified front and side thinning factor combinations (exclude stringers)
# always generate a set with thinning factors of 1 and 1 which exclude stringers

# reverse the thins so we do the small photosets first
thins_rev = thins[nrow(thins):1,]

thins_rev = as.data.frame(thins_rev)
names(thins_rev) = c("forward_thin","side_thin")

thins_rev = thins_rev %>%
  dplyr::mutate(thin_name = paste(forward_thin,side_thin,sep="_"))

## Give all photos an incrementing number in sequence so the front-thin photo selections are the same in each dataset
d_coords$image_sequence_number = 1:nrow(d_coords)



for(i in 1:nrow(thins_rev)) {
  
  thin = thins_rev[i,]
  
  photos_side_thin = d_coords %>%
    filter(!stringer) %>%  # exclude stringers
    filter((transect_id_new %% thin$side_thin) == 0)  # perform side thinning
  
  # perform forward thinning
  photos = photos_side_thin[(photos_side_thin$image_sequence_number %% thin$forward_thin) == 0,]
  
  thinned_photoset_name = paste0(photoset_name,"_thin",thin$forward_thin,thin$side_thin)
  
  ## Copy thinned set to destination path
  
  # Get the necessary paths
  photos = photos %>%
    mutate(source_path = paste0(photoset_path,"/",Folder_File),
           dest_path = paste0(destination_path,"/",thinned_photoset_name,"/",Folder_File)) %>%
    mutate(dest_directory = dest_path %>% map(path_drop_last) %>% unlist ) # this is the destination file without the file at the end: for creating the directory for it via dir.create below
  
  # Make sure all needed directories exist
  dest_directories = unique(photos$dest_directory)
  walk(dest_directories,dir.create,recursive=TRUE)
  
  file.copy(photos$source_path,photos$dest_path, overwrite=FALSE)
  
}
