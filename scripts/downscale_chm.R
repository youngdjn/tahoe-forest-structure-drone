### Create alternate versions of CHMs that are downscaled to higher resolution with bilinear interpolation

library(raster)
library(here)
library(tidyverse)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions ####
source(here("scripts/convenience_functions.R"))




### Find files to process
chms = list.files(data("metashape_outputs_postprocessed/chm/"))
chms = chms[grepl("dsm_chm",chms)]


### Get metashape paramset names
name = str_split(chms,fixed(".")) %>% map(1)
nameparts = str_split(name,fixed("_"))

names_df = as.data.frame(do.call(rbind,nameparts))
names(names_df) = c("photoset","metashape_parameters", "timestamp", "dsm","chm")
names_df = names_df %>%
  mutate(photoset_number = str_sub(photoset,start=-2,end=-1),
         full_name = chms)


### Target resolutions: 0.03, 0.06, 0.12
tgt_res = c(0.03, 0.06, 0.12) # names for these should be 3xxx, 2xxx, 1xxx
tgt_name = c(3, 2, 1)

# New:
tgt_res = c(0.25)
tgt_name = c(4)

### Downscale each raster to each resolution
for(i in 1:nrow(names_df)) {
  names_row = names_df[i,]
  r = raster(data(paste0("metashape_outputs_postprocessed/chm/",names_row$full_name)))

  for(j in 1:length(tgt_res)) {
    
    # if it's an already downscaled raster, skip
    if(as.numeric(as.character(names_row$metashape_parameters)) > 999) {
      next()
    }
    
    # What will the downscaled raster be named?
    metashape_set_name = paste0(tgt_name[j],names_row$metashape_parameters)
    filename = paste(names_row$photoset, metashape_set_name, names_row$timestamp, "dsm_chm.tif",sep = "_")
    path_filename = data(paste0("metashape_outputs_postprocessed/chm/",filename))
    
    # Does it exist already? If so, skip
    if(file.exists(path_filename)) {
      next()
    }
    
    r_ds = projectRaster(r,res=tgt_res[j], crs = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", method="bilinear")

    writeRaster(r_ds,path_filename)

  }
}

#### By what factor do we need to downscale (disaggregate)?

# ## Get the lowest res for each photoset
# res_summ = names_df %>%
#   group_by(photoset_number) %>%
#   summarize(min_res = min(res))
#
# ## Pull this in to the main df
# names_df = left_join(names_df,res_summ)
#
# ## Compute target resolution (1/2 of min) and necessary disaggregation factor
# names_df = names_df %>%
#   mutate(target_res = min_res/2) %>%
#   mutate(max_disagg = (res/target_res) %>% round)
#
# ### New names for downscaled layers should be a modified metashape parameter set number (it's the easiest to change)


# 
# for(i in 1:nrow(names_df)) {
#   
#   
#   
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# r = raster("/home/derek/Documents/data/uav_data/metashape_outputs_postprocessed/chm/paramset14_012_20201020T2040_dsm_chm.tif")
# 
# r_d = disaggregate(r,fact=2,method="bilinear")
# r_d2 = disaggregate(r,fact=4,method="bilinear")
# 
# writeRaster(r_d,"paramset96_030_20201022T0003_dsm_chm.tif")
# writeRaster(r_d2,"paramset97_030_20201022T0003_dsm_chm.tif")
