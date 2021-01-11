# If there is a directory with drone maps for more Metashape and/or tree detection parameter sets than need to be tested, this script can filter through it to keep only the desired ones (in a separate directory)

library(tidyverse)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


from_dir = data("detected_trees_beforeFixGroundTruthMap")
to_dir = data("detected_trees_filtered")


#### Load and clean ground map data ####
detected = list.files(from_dir, full.names=TRUE)

filesplits = str_split(detected, pattern=fixed("/"))
file_index = length(filesplits[[1]])
file = map(filesplits,file_index)






splits = str_split(file,pattern=fixed("_"))

photoset = map(splits,1)
metaset = map(splits,2) %>% as.numeric

detected_keep = which(photoset %in% c("paramset15","paramset14") &
                        metaset %in% 7:18)

to_copy = paste0(from_dir,"/",file[detected_keep])
destination = paste0(to_dir,"/",file[detected_keep])

dir.create(to_dir)

### Copy them over


file.copy(from= to_copy, to = destination)



