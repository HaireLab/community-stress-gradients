## terraclimate.data.R
## url: http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/data/catalog.html
### ncdf files by year
## download netCDF data and crop to regions with sample points from 13 study sites
## extract point values across post-fire years
## revised 19 may 2021 for exploring "stress gradients"
## see def.data.assembly.R for next steps
## then plot.def.stats.R for plotting the data


homedir<-getwd()
#datadir<-"./data/def/"
setwd(homedir)

library(ncdf4)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(readr)

## get bounding box for all sites
## ## process by regions
## read in blue mtn pts
blupts<-read_sf("./data/sp/blues_plotcoords.shp")
st_crs(blupts)<-"+proj=longlat + datum=WGS84" 

## ccd 
ccdpts<-read_sf("./data/sp/ccd_plotcoords.shp")
st_crs(ccdpts)<-"+proj=utm +zone=11"
ccdpts.geo<-st_transform(ccdpts, st_crs(blupts))

## utm 12
sw12pts<-read_sf("./data/sp/swz12_plotcoords.shp")
sw12.geo<-st_transform(sw12pts, st_crs(blupts))

## utm 13
sw13pts<-read_sf("./data/sp/swz13_plotcoords.shp")
sw13.geo<-st_transform(sw13pts, st_crs(blupts))

## don't combine--get separate bb for ea
bb1<-st_bbox(blupts)
bb2<-st_bbox(ccdpts.geo)
bb3<-st_bbox(sw12.geo)
bb4<-st_bbox(sw13.geo)

#
# NOTE:
# option 'wb' is needed because nc data are binary!!!!!!!
## .nc files are saved in first loop
yr=2000:2017 
for (i in 1:length(yr)) {   
  # Get the url for yr i
  url.aux<-paste(
    "http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_def_", yr[i], ".nc", sep="")
  dest<-paste("def", yr[i], ".nc", sep="")
  download.file(url.aux, dest, mode="wb")
    b<-brick(dest)
    bcrop<-crop(b, bb1, snap='out')
    writeRaster(bcrop, filename=paste("./data/def_blues/def_blues.",yr[i], ".grd",sep=""))
  } 

b=brick("./data/def/def.2000.grd")# ok

## use the saved .nc files for the rest...
lfnc<-list.files('.', pattern=".nc") # delete these filse when done
######## AZ 
for (i in 1:length(lfnc)) {
  b<-brick(lfnc[i])
  bcrop<-crop(b, bb3, snap='out')
  writeRaster(bcrop, filename=paste("./data/def_AZ/def.",yr[i], ".grd",sep=""))
  }

######## CO & NM
for (i in 1:length(lfnc)) {
  b<-brick(lfnc[i])
  bcrop<-crop(b, bb4, snap='out')
  writeRaster(bcrop, filename=paste("./data/def_CO-NM/def.",yr[i], ".grd",sep=""))
  
}

## sample at the plot locations--all years (subset later to match fire and sample year)
setwd('./data/def_AZ')
lfaz<-list.files(".", pattern=".grd")
saz<-stack(lfaz)
ex.az<-raster::extract(saz, sw12.geo, df=TRUE, method='bilinear')
ex.az2<-raster::extract(saz, sw12.geo, df=TRUE) # method='simple' is default, means etc are a little bit higher than bilinear
ex.az2<-ex.az2[,2:217]# del id col, now 249 rows, 216 cols
azpts_df<-st_drop_geometry(sw12.geo)
#plotid<-c(azpts_df[,1])## need the plotid as col name, rows are def values if transposing, but not doing that here
# put all data together 
az_dat<-bind_cols(azpts_df, ex.az2)

etwd(homedir)
write.table(az_dat, "./data/tab/def.az.txt")

## NM , CO
setwd('./data/def_CO-NM')
lfconm<-list.files(".", pattern=".grd")
sconm<-stack(lfconm)
ex.conm<-raster::extract(sconm, sw13.geo, df=TRUE) # method='simple' is default, means etc are a little bit higher than bilinear
ex.conm<-ex.conm[,2:217]# del id col, now 249 rows, 216 cols
conm_df<-st_drop_geometry(sw13.geo)
#plotid<-c(azpts_df[,1])## need the plotid as col name, rows are def values if transposing, but not doing that here
# put all data together 
conm_dat<-bind_cols(conm_df, ex.conm)

setwd(homedir)
write.table(conm_dat, "./data/tab/def.conm.txt")

## blues 
setwd('./data/def_blues')
lfblues<-list.files(".", pattern=".grd")
sblues<-stack(lfblues)
#ex.az<-raster::extract(saz, sw12.geo, df=TRUE, method='bilinear')
ex.blues<-raster::extract(sblues, blupts, df=TRUE) # method='simple' is default, means etc are a little bit higher than bilinear
ex.blues<-ex.blues[,2:217]# del id col, now 187 rows, 216 cols
blupts_df<-st_drop_geometry(blupts)
#plotid<-c(azpts_df[,1])## need the plotid as col name, rows are def values if transposing, but not doing that here
# put all data together 
blupts_dat<-bind_cols(blupts_df, ex.blues)
#names(az_datT) <- plotid$PLOT_ID
setwd(homedir)
write.table(blupts_dat, "./data/tab/def.blues.txt")


## ccd
setwd('./data/def_ccd')
lfccd<-list.files(".", pattern=".grd")
sccd<-stack(lfccd) # the extent incl blues but no matter
ex.ccd<-raster::extract(sccd, ccdpts.geo, df=TRUE)
ex.ccd<-ex.ccd[,2:229]# del id col, now 60 rows, 228 cols
ccdpts_df<-st_drop_geometry(ccdpts.geo)
#plotid<-c(ccdpts_df[,1])
## need the plotid as col name, rows are def values
# all data
ccd_dat<-bind_cols(ccdpts_df, ex.ccd)
#
setwd(homedir)
write.table(ccd_dat, "./data/tab/def.ccd.txt")


#### old#########################################################
ex.blues<-raster::extract(s, blupts, df=TRUE) #187 rows, 229 cols
ex.blues<-ex.blues[,-1]#187 rows, 228 cols
ex.bluesg<-tidyr::gather(ex.blues) #42636 rows
bdf<-st_drop_geometry(blupts)
bdf<-as_tibble(bdf)
bdf<-dplyr::select(bdf, Plot_ID, Fire)# 187 rows
names(bdf)[2]<-"Burn"
## replicate the point data table
bdf_rep<-
  bdf %>% slice(rep(1:nrow(bdf), 228))  %>% select(-2)
bdf_def<-bind_cols(bdf_rep, ex.bluesg)
write.table(dat1, "./data/tab/def.blues.txt")

## get bounding box for test area
## start w the blue mtns + ccd points
## read in blue mtn pts
blupts<-read_sf("./data/sp/blues_plotcoords.shp")
st_crs(blupts)<-"+proj=longlat + datum=WGS84" 
## ccd 
ccdpts<-read_sf("./data/sp/ccd_plotcoords.shp")
st_crs(ccdpts)<-"+proj=utm +zone=11"
ccdpts.geo<-st_transform(ccdpts, st_crs(blupts))
allxy<-st_union(st_geometry(ccdpts.geo), st_geometry(blupts))
bb1<-st_bbox(allxy)
