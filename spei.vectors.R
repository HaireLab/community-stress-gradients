## spei.vectors.R
## make a polygon from sample pts at ea field site
## use the bbox to sample spei
## hash rock and rodeo pts fall in > 1 spei raster cell
## pumpkin has an outlier (deleted for now)
## there is no doubt a more efficent way to do this, but for now I separated the sites by 
## year of fire and year sampled
## then generated a raster stack w the years of interest before sampling

library(sf)
library(dplyr)
library(raster)

## 1901-2018 data SPEI
## n = 1416 layers
b<-brick("d:/climate_indices/download2018/spei01.nc") 

## for 2002 fires:  747 Complex, Roberts Creek, Hayman, Missionary Ridge, Ponil Complex, Rodeo
## 2002-2017
## delete early years (1901-2001) and latest year (2018)
## calc num months to exclude 101 * 12 = 1212
## and 12 mos at the end 1416 - 12
b2002<-b[[1213:1404]] ## 2002.01 - 2017.12..............192 layers

## read in blue mtn pts
blupts<-read_sf("./data/sp/blues_plotcoords.shp")
st_crs(blupts)<-"+proj=longlat + datum=WGS84" 

## 747 & Robert Creek, 2002 fires
u=unique(blupts$Fire)
#"747"           "Burnt_Cabin"   "Hash_Rock"     "Roberts_Creek"
i=1 ############################# 747 ################################
print(u[i])
b1<-filter(blupts, Fire==u[i]) # select site
bb1<-st_bbox(b1) # bounding box
b02.crop<-crop(b2002, bb1, snap='out') # get all the data cells in bbox
## check
plot(b02.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
b747.u<-unique(values(b02.crop)) # named vector
cat(b747.u, file="b747.spei.txt")
# check: 
v1=scan("b747.spei.txt")

i=4 ############################## roberts creek ####################
print(u[i])
b1<-filter(blupts, Fire==u[i]) # select site
bb1<-st_bbox(b1) # bounding box
b02.crop<-crop(b2002, bb1, snap='out') # get all the data cells in bbox
## check
plot(b02.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
rob.u<-unique(values(b02.crop)) # named vector
cat(rob.u, file="rob.spei.txt")
# check: 
v1=scan("rob.spei.txt")

## soutwest z 12
sw12<-read_sf("./data/sp/swz12_plotcoords.shp")
st_crs(sw12)<-"+proj=utm +zone=12"
sw12.geo<-st_transform(sw12, st_crs(b2002))
u=unique(sw12$Burn)
#"Outlet"  "Poplar"  "Pumpkin" "Rodeo" 
# rodeo hits more than one cell
i=4 ################################## rodeo ##########################
print(u[i])
b1<-filter(sw12.geo, Burn==u[i])
bb1<-st_bbox(b1)
b02.crop<-crop(b2002, bb1, snap='out') # get all the data cells in bbox
  ## check
plot(b02.crop[[1]])
plot(st_geometry(b1), add=TRUE)
##  output brick with mean of each layer
rodeo.mean<-cellStats(b02.crop, stat='mean') 
cat(rodeo.mean, file="rodeo.spei.txt")
  # check: 
v1=scan("rodeo.spei.txt")

## soutwest z 13
sw13<-read_sf("./data/sp/swz13_plotcoords.shp")
st_crs(sw13)<-"+proj=utm +zone=13"
sw13.geo<-st_transform(sw13, st_crs(b.one))
u=unique(sw13$Burn)
#""Cerro"   "Hayman"  "Mission" "Ponil"
## all these land in just one raster cell/burn
############################## missionary ridge ##################
i=3
print(u[i])
b1<-filter(sw13.geo, Burn==u[i])
bb1<-st_bbox(b1)
b02.crop<-crop(b2002, bb1, snap='out') # get all the data cells in bbox
  ## check
plot(b02.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
mis.u<-unique(values(b02.crop)) # named vector
cat(mis.u, file="mis.spei.txt")
# check: 
v1=scan("mis.spei.txt")

i=4 ########################## ponil #################################
print(u[i])
b1<-filter(sw13.geo, Burn==u[i])
bb1<-st_bbox(b1)
b02.crop<-crop(b2002, bb1, snap='out') # get all the data cells in bbox
## check
plot(b02.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
pon.u<-unique(values(b02.crop)) # named vector
cat(pon.u, file="pon.spei.txt")
# check: 
v1=scan("pon.spei.txt")

#################################### hayman ##########################
i=2
print(u[i])
b1<-filter(sw13.geo, Burn==u[i])
bb1<-st_bbox(b1)
b02.crop<-crop(b2002, bb1, snap='out') # get all the data cells in bbox
## check
plot(b02.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
hay.u<-unique(values(b02.crop)) # named vector
cat(hay.u, file="hayman.spei.txt")
hv=scan("hayman.spei.txt")


###########################Clear Creek Divide ######################
#- 2000-2018 
b0018<-b[[1189:1416]] ## 2000.01 - 2018.12......228 layers
ccdpts<-read_sf("./data/sp/ccd_plotcoords.shp")
st_crs(ccdpts)<-"+proj=utm +zone=11"
ccdpts.geo<-st_transform(ccdpts, st_crs(b0018))
bb1<-st_bbox(ccdpts.geo)
b0018.crop<-crop(b0018, bb1, snap='out')
## check
plot(b0018.crop[[1]])
plot(st_geometry(ccdpts.geo), add=TRUE)
## use the extent poly and get unique values for all the layers
ccd.u<-unique(values(b0018.crop)) # named vector
cat(ccd.u, file="ccd.spei.txt")
v1=scan("ccd.spei.txt")


#2003-2017 
b0317<-b[[1225:1404]] ## 2003.01 - 2017.12......180 layers
#######################Poplar Complex ##############################
## soutwest z 12
sw12<-read_sf("./data/sp/swz12_plotcoords.shp")
st_crs(sw12)<-"+proj=utm +zone=12"
sw12.geo<-st_transform(sw12, st_crs(b2002))
u=unique(sw12$Burn)
#"Outlet"  "Poplar"  "Pumpkin" "Rodeo" 

i=2
print(u[i])
b1<-filter(sw12.geo, Burn==u[i])
bb1<-st_bbox(b1)
b03.crop<-crop(b0317, bb1, snap='out') # get all the data cells in bbox
## check
plot(b03.crop[[1]])
plot(st_geometry(b1), add=TRUE)
pop.u<-unique(values(b03.crop)) # named vector 
cat(pop.u, file="pop.spei.txt")
# check: 
v1=scan("pop.spei.txt")

#2005-2017 
b0517<-b[[1249:1404]] ## 2005.01 - 2017.12......156 layers
#######################Burnt Cabin ##################################
## read in blue mtn pts
blupts<-read_sf("./data/sp/blues_plotcoords.shp")
st_crs(blupts)<-"+proj=longlat + datum=WGS84" 

## 747 & Robert Creek, 2002 fires
u=unique(blupts$Fire)
#"747"           "Burnt_Cabin"   "Hash_Rock"     "Roberts_Creek"

i=2
print(u[i])
b1<-filter(blupts, Fire==u[i]) # select site
bb1<-st_bbox(b1) # bounding box
b05.crop<-crop(b0517, bb1, snap='out') # get all the data cells in bbox
## check
plot(b05.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
bc.u<-unique(values(b05.crop)) # named vector
cat(bc.u, file="bc.spei.txt")
# check: 
v1=scan("bc.spei.txt")

#- 2000-2017 
b0017<-b[[1189:1404]] ## 2000.01 - 2017.12......216 layers
########################Hash Rock ####################################
i=3
print(u[i])
b1<-filter(blupts, Fire==u[i]) # select site
bb1<-st_bbox(b1) # bounding box
b00.crop<-crop(b0017, bb1, snap='out') # get all the data cells in bbox
## check
plot(b00.crop[[1]])
plot(st_geometry(b1), add=TRUE)
##  output brick with mean of each layer
hr.mean<-cellStats(b00.crop, stat='mean') 
cat(hr.mean, file="hr.spei.txt")
# check: 
v1=scan("hr.spei.txt")

########################Outlet #######################################
## soutwest z 12
sw12<-read_sf("./data/sp/swz12_plotcoords.shp")
st_crs(sw12)<-"+proj=utm +zone=12"
sw12.geo<-st_transform(sw12, st_crs(b2002))
u=unique(sw12$Burn)
#"Outlet"  "Poplar"  "Pumpkin" "Rodeo" 

i=1
print(u[i])
b1<-filter(sw12.geo, Burn==u[i])
bb1<-st_bbox(b1)
b00.crop<-crop(b0017, bb1, snap='out') # get all the data cells in bbox
## check
plot(b00.crop[[1]])
plot(st_geometry(b1), add=TRUE)
out.u<-unique(values(b00.crop)) # named vector 
cat(out.u, file="out.spei.txt")
# check: 
v1=scan("out.spei.txt")


####################### Pumpkin ######################################
i=3
print(u[i])
b1<-filter(sw12.geo, Burn==u[i])
## for now, delete outlier point row 17
pu.xy<-read.table("./data/tab/pumpkinxy.txt")
pu.xy<-pu.xy[-17,]
summary(pu.xy) # min 111.9, 35.4 max -111.8, 35.44
b1.crop<-st_crop(b1, xmin=min(pu.xy[,1]), xmax=max(pu.xy[,1]), ymin=min(pu.xy[,2]), ymax=max(pu.xy[,2]))
bb1<-st_bbox(b1)
bb2<-st_bbox(b1.crop)
b00.crop<-crop(b0017, bb2, snap='out') # get all the data cells in bbox
## check
plot(b00.crop[[1]])
plot(st_geometry(b1), add=TRUE)
pum.u<-unique(values(b00.crop)) # named vector 
cat(pum.u, file="pum.spei.txt")
# check: 
v1=scan("pum.spei.txt")

#######################Cerro Grande###################################
## soutwest z 13
sw13<-read_sf("./data/sp/swz13_plotcoords.shp")
st_crs(sw13)<-"+proj=utm +zone=13"
sw13.geo<-st_transform(sw13, st_crs(b.one))
u=unique(sw13$Burn)
#""Cerro"   "Hayman"  "Mission" "Ponil"
## all these land in just one raster cell/burn
i=1
print(u[i])
b1<-filter(sw13.geo, Burn==u[i])
bb1<-st_bbox(b1)
b00.crop<-crop(b0017, bb1, snap='out') # get all the data cells in bbox
## check
plot(b00.crop[[1]])
plot(st_geometry(b1), add=TRUE)
## use the extent poly and get unique values for all the layers
cer.u<-unique(values(b00.crop)) # named vector
cat(cer.u, file="cer.spei.txt")
# check: 
v1=scan("cer.spei.txt")
