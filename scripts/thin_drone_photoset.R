library(tidyverse)
library(exifr)
library(sf)


#### Parameters to set for each run (specific to a given photo set) ####

# Top-level folder of all mission images
photoset_path = "C:/Users/DYoung/Box/projects/uav_data/imagery/missions/14_EmPo_120m_95_95"

# Specify manual stringer images (images that MapPilot collects along the project boundary when moving from one transect to the next) to exclude if they're not picked up by the algorithm
manual_stringer_photos = c("100MEDIA/DJI_0031.JPG","100MEDIA/DJI_0032.JPG","100MEDIA/DJI_0033.JPG","100MEDIA/DJI_0034.JPG","100MEDIA/DJI_0035.JPG","100MEDIA/DJI_0036.JPG")

# How many degrees (angle) change in transect path signals a new transect?
change_thresh = 10

## Stringer detection:
# Within how many degrees (in terms of the orientation of the transect) does a focal transect have to be from other transects to not be considered a stringer
tolerance = 3
# What proportion of other transects have to be within the tolerance for the focal transect to not be considered a stringer transect?
proportion_matching_threshold = .1





#### Convenience functions ####

rad2deg = function(rad) {
  return(rad*180/pi)
}




#### Assign transect IDs to photos ####
#### This is to allow thinning by transect


## Find all original drone photos (use regex to search for DJI photos in case there are some non-drone photos in the folder)
photo_files = list.files(photoset_path,recursive=TRUE,pattern="DJI_[0-9]{4}.JPG",full.names = TRUE)

d_exif = read_exif(photo_files, tags = c("ImageDescription","GPSLatitude","GPSLongitude"))

d = d_exif %>%
  select(ImageDescription,GPSLatitude,GPSLongitude) %>%
  separate(ImageDescription,c(NA,"Folder","File"), sep = "\\\\") %>%
  unite("Folder_File",Folder,File,sep="/") %>%
  arrange(Folder_File)    # put in the order they were flow


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
  
  angle = atan(y_dist/x_dist) %>% rad2deg # angle from one point to next
  angle = ifelse(x_dist < 0, angle + 180,angle)
  
  d_coords[i,"angle"] = angle
  
}

## Get average angle
avg_angle = mean(d_coords$angle,na.rm=TRUE)

## Compute change in angle from one point to the next
for(i in 2:(nrow(d_coords))) {
  
  last_angle = d_coords[i-1,"angle"]
  current_angle = d_coords[i,"angle"]
  
  angle_change = 360-(360-abs(current_angle-last_angle))
  
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
  group_by(transect_id) %>%
  slice(2:n()) %>% # drop the first photo of each group because it probably has a crazy angle
  slice(1:(n()-1)) %>% # drop the last row of each group because it could have a crazy angle
  summarize(avg_angle = mean(angle))

n_transects = nrow(transect_summ)


for(i in 1:nrow(transect_summ)) {
  
  transect = transect_summ[i,]
  angle = transect$avg_angle
  transect_id = transect$transect_id
  
  lower_bound = (angle-tolerance) %% 360
  upper_bound = (angle+tolerance) %% 360
  
  matching_transects = transect_summ %>%
    filter((avg_angle > lower_bound) & (avg_angle < upper_bound))
  
  n_matching = nrow(matching_transects)
  
  proportion_matching = n_matching/n_transects
  
  if(proportion_matching < proportion_matching_threshold) {
    d_coords[d_coords$transect_id == transect_id,"stringer"] = TRUE
  } else {
    d_coords[d_coords$transect_id == transect_id,"stringer"] = FALSE
  }
  
}


# assign manual stringer photos
d_coords[d_coords$Folder_File %in% manual_stringer_photos,"stringer"] = TRUE


## Assign new transect IDs, but only to non-stringer transects
d_coords[!d_coords$stringer,"transect_id_new"] = d_coords[!d_coords$stringer,] %>% group_indices(transect_id)

d_coords = d_coords %>%
  mutate(odd_transect_new = (transect_id_new %% 2))


## Make it spatial again for checking results on a map


d_tsect_sp = st_as_sf(d_coords,coords=c("X","Y"), crs=3310)

plot(d_tsect_sp)

st_write(d_tsect_sp %>% st_transform(4326), "temp/temp_transect_eval.geojson",delete_dsn=TRUE)



#### Generate thinned photoset copies ####

# copy photosets with specified front and side thinning factor combinations (exclude stringers)
# always generate a set with thinning factors of 1 and 1 which exclude stringers







