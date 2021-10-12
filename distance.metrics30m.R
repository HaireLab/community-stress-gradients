## distance.metrics30m.R
## kernels + euc dist
## use the pt shp files in the demdirs--they have unique_id
library(sf)
library(raster)
library(rgdal)

# 30 m images except clear creek...see below
imgpath<-"/media/sandra/My Passport/NFPdata/all.trees/trees30m/"
lfimg<-list.files(imgpath, pattern=".grd")
# point shp for ea burn
shp.path<-"./data/dems/"
## directories ea burn, match order w lfimg...neet nrim 2x for outlet and poplar
demdirs<-c("complex_747", "burnt_cabin", "cerro_grande", "hash_rock", "hayman", "missionary_ridge", "outlet", 
           "ponil", "poplar", "pumpkin", "roberts_creek", "rodeo")

## loop thru the images and calc euc dist + kernel
for (i in 1:length(lfimg)) {
  trees<-raster(paste(imgpath, lfimg[i], sep=""))
  ##  pts burn in geo
  list.shp<-list.files(paste(shp.path, demdirs[i], sep=""), pattern=".shp")
  pts1<-read_sf(paste(shp.path, demdirs[i], "/", list.shp, sep=""))
  ## need geo and utm...
  pts1.utm<-st_transform(pts1, st_crs(trees))
  ## set up for focal
  wts=focalWeight(trees,d=150,type='Gauss')
  treefocal<-focal(trees,w=wts, na.rm=TRUE) # need this for edge (padvalue didn't do the trick)
  
  # sample and write output to file
  ex1<-raster::extract(treefocal, pts1.utm, df=TRUE, method='bilinear')
  ## run dist fnc utm
  trees[trees==0]<-NA
  treedist<-distance(trees)
  ex2<-raster::extract(treedist, pts1.utm, df=TRUE, method='bilinear')
  dat<-cbind(data.frame(pts1.utm$unique_id), ex1, ex2)
  write.table(dat, "./data/tab/distance_metrics30m.txt", append=TRUE, col.names=FALSE, row.names=FALSE)
}

## clear creek
trees<-raster("./data/trees/clear_creek30m.tif")
##  pts burn in geo
pts1<-read_sf("./data/dems/clear_creek/ccdpts_geo.shp")
## need geo and utm...
pts1.utm<-st_transform(pts1, st_crs(trees))
## set up for focal
wts=focalWeight(trees,d=150,type='Gauss')
treefocal<-focal(trees,w=wts, na.rm=TRUE) # need this for edge (padvalue didn't do the trick)

# sample and write output to file
ex1<-raster::extract(treefocal, pts1.utm, df=TRUE, method='bilinear')
## run dist fnc utm
trees[trees==0]<-NA
treedist<-distance(trees)
ex2<-raster::extract(treedist, pts1.utm, df=TRUE, method='bilinear')
dat<-cbind(data.frame(pts1.utm$unique_id), ex1, ex2)
write.table(dat, "./data/tab/distance_metrics30m.txt", append=TRUE, col.names=FALSE, row.names=FALSE)

