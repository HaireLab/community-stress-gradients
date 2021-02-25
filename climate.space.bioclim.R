## climate.space.bioclim.R
## use the polygon (fire perimeter) data
## the plot coordinates are going to take time to sort out--some utm (3 zones) 
## first run bioclim.dataprep.R (reads in bioclim layers and crop to extent of burned sites)

rm(list=ls()) ### start over
homedir<-getwd()
library(sf)
library(rgdal)
library(raster)

## use this shp to extract data: perims.lcc
perims<-readOGR('./data/sp', 'all_burns_incl_CCD')

## bioclim prj
bio.prj<-"+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # from metadata lambert conformal conic
perims.lcc<-spTransform(perims, bio.prj)

##setup for principal components analysis
##Set working directory to the location of your climate raster data.
setwd("C:/Users/sandr/Documents/Rfiles/stress.gradients/data/bioclim.1981-2010/")

## read in the layers w study region extext
files.to.read<-list.files(pattern='1.asc')
name.raster<-c("AHM", "DD_5", "EMT","MAP", "MAT","NFFD", "PPT_sm", "PPT_wt","TD")
#
for (i in 1:length(files.to.read)) {
    a<-raster(files.to.read[i])
  # then assign a name to each layer into the console
  assign(name.raster[i],a)
} 

################ Principal components Analysis ######################################
## adapted from @EllWhitman
## the objective is to be able to assign to each pixel a value for each PCA axis, so first an index with a 'pixel.code' is generated
## this code is the one used after to set back the PCA values to rasters

# 1. obtain an index that id's the position of each pixel including NAs (i.e., code from 1 to N = number of cells in the rasters)
ID.for.cell<-c(1:ncell(MAP)) 

# 2. obtain the coodinates of all the cells including NAs (no values), that's why NAs are first set to a numeric value
# new raster where NAs will be modified
for.x.y<-MAP
# assign the value '99' to all pixels that are NA
for.x.y<-reclassify(for.x.y,c(NA,NA,99))
# get the x-y coordinates of each pixel
xy.cont<-rasterToPoints(for.x.y) 

# 3. combine ID and xy coordinates
ID.xy<-cbind(ID.for.cell,xy.cont); colnames(ID.xy)<-c('ID','x','y','NA')

# 4. to work more efficiently, and because of how PCA formulas work, NAs need to be removed (the rows where NA = 99)  
## there are no nas because I'm using raster cropped to extent, but will need this if I run with mask version
ID.xy.cont<-ID.xy[!(ID.xy[,'NA'] %in% c(99)),]

# 5. loop that will extract the climate data in all the xy.coordinates
# to avoid generating really big matrices, variables will be added to a new matrix called 'mat.pca'
# (note that the name generated to import the climate rasters is used here again)
MAP.pca<-NULL

for (i in 1:length(name.raster)){
  # selecting each climate variable
  a<-get(name.raster[i])
  # intermediate step to select only those pixels that are in 'ID.xy.cont'
  x<-cbind(ID.for.cell,a[])
  x.clean<-x[x[,'ID.for.cell'] %in% ID.xy.cont[,'ID'],2]  # this is saying, keep only the pixels that has and ID equal than any ID in ID.xy.cont
  # adding the climate values of each variable to the matrix 'mat.pca'
  MAP.pca<-cbind(MAP.pca,x.clean)
  # to make sure a different variable is used at each time, better to remove the intermediate steps
  rm(a,x,x.clean)
}

# the 'name.raster' vector is used to give the proper colnames to 'mat.pca'
colnames(MAP.pca)<-name.raster

# 6. IMPORTANT - make sure that there are no NAs in your climate data 
## there are no NAs
MAP.pcadf<-as.data.frame(MAP.pca)

# 7. before the PCA the ID matrix needs to be adjusted in relation to the additional NAs removed in step 6.
##Skip if you skipped step 6, or if you do not have any NA values within the data
## SKIP
#ID.xy.pca.pre<-cbind(ID.xy.cont,mat.pca[,'dd5'])
#colnames(ID.xy.pca.pre) [5]<-("dd5")
#ID.xy.pca<-ID.xy.pca.pre[is.na(ID.xy.pca.pre[,'dd5'])==FALSE,c(1:4)]

# 9. using 'prcomp' function to perform the PCA
# scores=TRUE specify that you want to save the PCA values for each pixel, and scale.=TRUE is what standardize the variables ! 

pca.climate<-prcomp(~ AHM + DD_5 + EMT + MAP + MAT + NFFD + PPT_sm + PPT_wt + TD, data = MAP.pcadf, scores=TRUE, scale.=TRUE)

###########################################Convert PCA scores to raster map################################################
#1. Saving pca results into R's memory
# to know how data is organized
pca.summary<-summary(pca.climate)
str(pca.summary)
# 'rotation' contains the loadings of each variable in each PC axis
#select only PC1 and PC2, but more or fewer components could be selected and exports
rotation<-pca.summary$rotation
rotation<-rotation[,1:2]
# 'importance' contains the importance (% of explained variance) of each PC axis
importance<-pca.summary$importance
# 'x' containes the PC1, PC2, PC3, .. values for each pixel
scores<-pca.summary$x[,1:2]

#2. Combine the cell ID values with the PCA scores
#Since all data was adjusted before the PCA, the ID.xy.pca matches with the PCA results, so we can combine the two matrices
MAP.step<-cbind(ID.xy.cont[,('ID')],scores)
colnames(MAP.step)[1]<-'ID'

# 3. using 'mat.step' + 'ID.for.cell' to generate raster layers with the PCA values
# generating again 'ID.for.cell'
# first an index listing the position of non-NAs pixels within 'ID.for.cell'
xx<-match(ID.for.cell,MAP.step[,'ID'])

# this 'xx' is then used to get the PC values in the right order to set back the values to rasters
pc1<-cbind(ID.for.cell,MAP.step[xx,'PC1'])
pc2<-cbind(ID.for.cell,MAP.step[xx,'PC2'])

# 4. Convert the PC values back to rasters (using the one used to generate 'ID.for.cell' ensures that the raster will have the same chracteristics as the original data)
#Re-name and save the principal component score rasters where you wish
pc1.rast<-setValues(MAP,pc1[,2])
pc2.rast<-setValues(MAP,pc2[,2])
writeRaster(pc1.rast,paste(homedir, "/data/PC1.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pc2.rast,paste(homedir, "/data/PC2.tif", sep=""), format="GTiff", overwrite=TRUE)

########### didn't run this part, but could do next time based on Marc's suggestion
#########################################Apply varimax rotation to PCs and export #############################################
#1. Apply a varimax rotation to the principal components analysis, if the loadings are difficult to interpret
varimax.rotation<-varimax(rotation)

#2. Scale the principal component inputs (this is not necessary in prcomp because the function does it itself), and generate rotated scores
MAP.pca.rot<-as.matrix(MAP.pcadf)
MAP.pca.rot<-scale(MAP.pca.rot,center=TRUE,scale=TRUE)
MAP.var.load<-varimax.rotation$loadings
pca.var.scores<-MAP.pca.rot %*% MAP.var.load

#3. Combine the cell ID values with the rotated PCA scores
#Since all data was adjusted before the PCA, the ID.xy.pca matches with the PCA results, so we can combine the two matrices
MAP.step.var<-cbind(ID.xy.cont[,('ID')],pca.var.scores)
colnames(MAP.step.var)[1]<-'ID'

# 3. using 'mat.step' + 'ID.for.cell' to generate raster layers with the PCA values
# generating again 'ID.for.cell'
# first an index listing the position of no-NAs pixels within 'ID.for.cell' (which has a code for all pixels) is needed
xx<-match(ID.for.cell,MAP.step.var[,'ID'])

# this 'xx' is then used to get the RC values in the right order to set back the values to rasters
rc1<-cbind(ID.for.cell,MAP.step.var[xx,'PC1'])
rc2<-cbind(ID.for.cell,MAP.step.var[xx,'PC2'])

# 4. Convert the RC values back to rasters (using the one used to generate 'ID.for.cell' ensures that the raster will have the same chracteristics as the original data)
#Re-name and save the rotated principal component scores rasters where you wish
rc1.rast<-setValues(MAP,rc1[,2])
rc2.rast<-setValues(MAP,rc2[,2])
writeRaster(rc1.rast, paste(homedir, "/data/PC1rot.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(rc2.rast, paste(homedir, "/data/PC2rot.tif", sep=""), format="GTiff", overwrite=TRUE)

######################## ran this part
##look at results
summary(pca.climate) # 0.6347 %var pc1; 0.2074 %var pc2
names(pca.climate) 
dim(pca.climate$x)
pca.climate$x[1:20,1:2] 
rot.df<-pca.climate$rotation[,1:2] # pc 1 = energy/heat; pc2=moisture/
write.table(rot.df, paste(homedir, "/data/pc-rotation.txt", sep=""))

library(rasterVis)
levelplot(pc1.rast, margin=FALSE)
levelplot(pc2.rast, margin=TRUE)

## extract values of pcs w/in perimeters
s<-stack(pc1.rast, pc2.rast)
ex1<-extract(s, perims.lcc, method='bilinear', df=TRUE) # which burn is which?
samp1<-spsample(perims.lcc, n=3000, type="random") # use random sample...
ex1<-extract(s, samp1, method='bilinear', df=TRUE)
o=over(samp1, perims.lcc)
dat<-cbind(data.frame(samp1), ex1, o)
setwd(homedir)
write.table(dat, "./data/tab/sample.pcs.txt") ## use this for scatter plots in pc space
