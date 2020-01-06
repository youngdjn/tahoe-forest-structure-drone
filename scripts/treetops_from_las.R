## Takes a USGS DEM and a point cloud and identifies treetops

# The root of the data directory
data_dir = "C:/Users/DYoung/Box/projects/forestuav_data/"

# Name of specific Metashape project for which to process Metashape products
metashape_products_dir = "test/"

library(sf)
library(raster)
library(lidR)
library(ForestTools)

#### Convenience functions ####

# prepend a given path with the path to the data folder
data = function(dir) {
  return (paste0(data_dir,dir))
}


#### Load data ####

roi = st_read(data("roi/emerald_point_roi.geojson"))
dem = raster(data("dem_usgs/emerald_point/dem_usgs.tif"))

# find the LAS file in the metashape products direcotry
dir = data(  paste0("metashape_products/",metashape_products_dir ))
las_file = list.files(dir,pattern=".las", full.names=TRUE)
if(length(las_file) > 1) stop("More than 1 las file in the specified metashape data products folder.")
if(length(las_file) == 0) stop("No las files int he specified metashape data products folder.")

las = readLAS(las_file)
gc()
las_clip = lasclip(las,roi %>% st_transform(26910)) # TODO: get CRS from the las file
gc()
rm(las)
gc()

thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- grid_canopy(las_clip, 0.3, pitfree(thr, edg))
chm2 <- grid_canopy(las_clip, 0.3, p2r(0.05))
gc()

writeRaster(chm2, data( paste0("dsm/",metashape_products_dir,"chm2.tif" )))

lin <- function(x){x * 0.05 + 0.6}
treetops <- vwf(CHM = chm, winFun = lin, minHeight = 3)





############# OLD ##############




library(lidR)
library(sf)
library(ForestTools)

roi = st_read("roi.shp")
roi = roi %>% as("Spatial")






rm(las)
# 
# plot(las_clip)
# 
# las_clip

thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- grid_canopy(las_clip, 0.3, pitfree(thr, edg))
# ter =  grid_terrain(las_clip, algorithm=knnidw(p=12))
ter_class = lasground(las_clip,algorithm=csf(rigidness=2, cloth_resolution=0.1))

las_ground = lasfilter(ter_class, Classification!=2)

dtm = grid_terrain(ter_class,algorithm=tin())


min = min(values(chm),na.rm=TRUE)
chm = chm - min

lin <- function(x){x * 0.05 + 0.6}

ttops <- vwf(CHM = chm, winFun = lin, minHeight = 3)

crowns <- mcws(treetops = ttops, CHM = chm, minHeight = 3, verbose = FALSE, format="polygons")


# Plot CHM
plot(chm, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

# Add crown outlines to the plot
plot(crowns, border = "blue", lwd = 0.5, add = TRUE)


st_write(ttops %>% as("sf"),"ttops2.gpkg",delete_dsn=TRUE)
st_write(crowns %>% as("sf"),"crowns2.gpkg",delete_dsn=TRUE)


