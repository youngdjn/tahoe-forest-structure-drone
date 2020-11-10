## Takes a CHM and makes a map of treetops
  

## Convenience functions ####
source(here("scripts/convenience_functions.R"))


las_singlelas_allparams = function(las_layer_name, params) {

  #### Load data ####
  
  # find the las file in the metashape products direcotry
  las_files = list.files(data("metashape_outputs_postprocessed/las"),pattern=las_layer_name, full.names=TRUE)
  if(length(las_files) > 1) {
    
    cat("More than 1 matching LAS file in the specified metashape data products folder for",las_layer_name,". Using most recent.\n") 
    
    # get the date
    pieces = str_split(las_files,"/")
    filenames = map(pieces,sapply(pieces,length)[1]) %>% unlist
    dates = filenames %>% str_split("_") %>% map(3) %>% unlist
    # which date is later?
    latest = which(order(dates) == max(order(dates)))
    las_file = las_files[latest]
    
  } else if(length(las_files) == 0) {
      stop("No matching LAS files int he specified metashape data products folder.")
  } else {
    las_file = las_files
  }
  
  las = readLAS(las_file)
  
  # temp for dev
  #las = decimate_points(las, homogenize(10,1))
  
  ## Get the LAS layer name from the las_file
  pieces = str_split(las_file,"/")
  filename = map(pieces,sapply(pieces,length)[1]) %>% unlist
  filename_noextension = str_split(filename,fixed(".")) %>% map(1) %>% unlist()
  las_layer_name = filename_noextension
  
  ## randomize the params so if multiple threads try this las file, they will do different las params
  params = params %>%
    slice_sample(prop=1)
  
  ### Run all the tree detection algorithms
  
  for(i in 1:nrow(params)) {
    
    params_current = params[i,]
    
    ### If the output already exists, skip
    # First, what is the file?
    file_name = paste0(las_layer_name,"-",params_current$detection_params_name,".geojson")
    dir = data(paste0("detected_trees"))
    path_file = paste0(dir,"/",file_name)
    if(file.exists(path_file)) {
      cat("Output already exists:",path_file,", skipping.\n")
      next()
    }
    
    cat("Starting tree detection for", paste0(las_layer_name,"-",params_current$detection_params_name),"\n")
    
    ### Thin the LAS as specified by the params
    las_thinned = decimate_points(las, homogenize(params_current$decimate,1))
    

    if(params_current$method == "layerstacking") {
      
      ttops = try({find_trees(las_thinned,LayerStacking(hmin=5,start = 1,hardwood=params_current$hardwood))}, silent = TRUE)
      
      if(class(ttops) == "try-error") {
        cat("Point cloud too poor for LayerStacking for", paste0(las_layer_name,"-",params_current$detection_params_name),"\n")
        next()
      }
      
      ttops_sf = ttops %>%
        st_as_sf()
      
    } else if(params_current$method == "li") {
      
      segmented = segment_trees(las_thinned,li2012(hmin=5,dt1=params_current$dt1,dt2=params_current$dt2, Zu = params_current$zu, R=params_current$r, speed_up = params_current$speedUp))
      
      ttops_sf = segmented %>% 
        slot("data") %>% 
        dplyr::group_by(treeID) %>%
        dplyr::filter(Z == max(Z)) %>%
        dplyr::ungroup() %>%
        st_as_sf(coords = c("X", "Y"),
                 crs = proj4string(las))

    } else if(params_current$method == "lmfx") {
      
      ttops = try({find_trees(las_thinned, lmfx(hmin=5,dist_2d=params_current$dist2d,ws=params_current$ws))}, silent = TRUE)
      
      if(class(ttops) == "try-error") {
        cat("Point cloud too poor for lmfx for", paste0(las_layer_name,"-",params_current$detection_params_name),"\n")
        next()
      }
      
      ttops_sf = ttops %>%
        st_as_sf()

    } else if(params_current$method == "multichm") {
      
      ttops = try({find_trees(las_thinned, multichm(hmin=5,dist_2d=params_current$dist2d,ws=params_current$ws, res = params_current$res, layer_thickness = params_current$layer_thickness, dist_3d = params_current$dist3d, use_max = params_current$use_max))}, silent = TRUE)

      if(class(ttops) == "try-error") {
        cat("Point cloud too poor for multichm for", paste0(las_layer_name,"-",params_current$detection_params_name),"\n")
        next()
      }
      
      ttops_sf = ttops %>%
        st_as_sf()

    } else if(params_current$method == "lmfauto") {
      
      ttops = find_trees(las_thinned, lmfauto(hmin=5))
      
      ttops_sf = ttops %>%
        st_as_sf()
      
    } else if(params_current$method == "hamraz") {
    
      segmented = try({segment_trees(las_thinned,hamraz2016())}, silent = TRUE)
      
      if(class(segmented) == "try-error") {
        cat("Point cloud too poor for hamraz for", paste0(las_layer_name,"-",params_current$detection_params_name),"\n")
        next()
      }
      
      ttops_sf = segmented %>% 
        slot("data") %>% 
        dplyr::group_by(treeID) %>%
        dplyr::filter(Z == max(Z)) %>%
        dplyr::ungroup() %>%
        st_as_sf(coords = c("X", "Y"),
                 crs = proj4string(las))
    }
    
    if(nrow(ttops_sf) == 0) {
      cat("No trees detected in",paste0(las_layer_name,"-",params_current$detection_params_name),", skipping.\n")
      next()
    }

    ttops_sf = ttops_sf %>%
      rename(height = Z) %>%
      st_set_agr("constant") %>%
      dplyr::select(height) %>%
      st_transform(4326)
    
    ## make file name to save, and save
    dir.create(dir)
    st_write(ttops_sf,path_file, delete_dsn=TRUE, quiet=TRUE)

  }
  
  return(TRUE)
  
}



