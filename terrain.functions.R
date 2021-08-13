## terrain.functions.R
## terrain metrics from raster and rsaga
## for each site
## 1. fill the sinks
## 2. calc terrain metrics (save as tif)
## 3. stack and sample at field xy's (save as txt) 
## 

library(raster)
library(RSAGA)
library(sf)
library(dplyr)

env<-rsaga.env()
path1<-"./data/dems/"
homedir<-getwd()

## made a folder for each site, except north rim which has two sites
dirs<-c("burnt_cabin", "cerro_grande", "clear_creek", "complex_747", "hash_rock", "hayman", "missionary_ridge",
        "north_rim", "ponil", "pumpkin", "roberts_creek", "rodeo")
## change to the level above these site folders
setwd(path1)

## loop through the dirs
for (j in 2:length(dirs)) { # ran j=1 as test
  s<-stack()# initialize for stack of terrain layers
  ## read in dem and save in file type req by saga
  dname<-list.files(paste(dirs[j], sep=""), pattern=".tif")
  dem.root<-tools::file_path_sans_ext(dname) # get the root name
  x<-raster(paste(dirs[j], "/", dname, sep=""))
  writeRaster(x, paste(dirs[j], "/", dem.root, ".sdat", sep=""), overwrite=TRUE)
  # fill sinks
  rsaga.fill.sinks(in.dem=paste(dirs[j], "/", dem.root, sep=""), out.dem=paste(dirs[j], "/dem_no_sinks", sep=""), env=env)
  x2<-raster(paste(dirs[j], "/dem_no_sinks.sdat", sep=""))
  crs(x2)<-crs(x) # assign prj to dem raster (no sinks)
  # calc metrics and save
    terrain(x2, opt="slope", neighbors=8, filename=paste(dirs[j], "/", dem.root, "_slope.tif", sep=""))
    terrain(x2, opt="aspect", neighbors=8, filename=paste(dirs[j], "/",dem.root, "_aspect.tif", sep=""))
    terrain(x2, opt="TRI", filename=paste(dirs[j], "/", dem.root, "_TRI.tif", sep="")) # uses 8 neighbors
    ## rel pos need ascii input
    writeRaster(x2, paste(dirs[j], "/dem_no_sinks.asc", sep=""))
    # relative position
    input<-paste(dirs[j], "/dem_no_sinks.asc", sep="")
    focal.function(input, fun=relative.position, radius=5, out.grid.prefix=dem.root, out.path=paste(dirs[j]))
    # calc tci and twi with carea and slope
    input<-paste(dirs[j], "/dem_no_sinks", sep="")
    output<-paste(dirs[j], "/", dem.root, "_carea", sep="") # save for tci/twi calc
    rsaga.topdown.processing(in.dem=input, out.carea=output, env=env)
    slope<-raster(paste(dirs[j], "/", dem.root, "_slope.tif", sep=""))
    carea<-raster(paste(dirs[j], "/", dem.root, "_carea.sdat", sep=""))
    twi <- log(carea / tan(slope / 180)); writeRaster(twi, paste(dirs[j],"/",dem.root, "_twi.tif", sep=""))
    tci <- log(carea / slope); writeRaster(tci, paste(dirs[j],"/",dem.root, "_tci.tif", sep=""))
    ## stack, incl dem
    tri<-raster(paste(dirs[j], "/", dem.root, "_TRI.tif", sep=""))
    relpos<-raster(paste(dirs[j], "/", dem.root, "_relpos.asc", sep=""))
    aspect<-raster(paste(dirs[j], "/", dem.root, "_aspect.tif", sep=""))
    ## stack the outputs
    s<-stack(x2, slope, aspect, twi, tci, tri, relpos, carea)
    ## read in the point data for this site/dir
    list.shp<-list.files(dirs[j], pattern=".shp")
    pts<-read_sf(paste(dirs[j], "/", list.shp, sep=""))
    ex1<-extract(s, as_Spatial(pts), method='bilinear', df=TRUE)
    dat<-cbind(pts[,1], ex1)
    ## sample and write to file
    write.table(dat, paste(homedir, "/data/tab/terrain.sample.txt", sep=""), append = TRUE, row.names=FALSE, col.names=FALSE)
 }


