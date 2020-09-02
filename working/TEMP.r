### TEMP.r

rm(list=ls())
library(omnibus)
library(enmSdm)
library(raster)

f <- listFiles('C:/ecology/Drive/R/fasterRaster/R')
for (ff in f) source(ff)

f <- listFiles('C:/ecology/Drive/R/fasterRaster/data', pattern='.rda')
for (ff in f) load(ff)


############
### DATA ###
############

wc <- raster::getData('worldclim', var='alt', res=5)
mad <- raster::getData('GADM', countr='MDG', level=0)
wc <- crop(wc, mad)

#############
### GRASS ###
#############

grassDir <- c('C:/Program Files/GRASS GIS 7.8')
alreadyInGrass <- FALSE

#################
### functions ###
#################

#' latRast <- faster('r.latlong', rast=madForest2000, outType='rast',
#' flags=c('quiet', 'overwrite'), grassDir=grassDir)
#' longRast <- faster('r.latlong', rast=madForest2000, outType='rast',
#' flags=c('quiet', 'overwrite', 'l'), grassDir=grassDir)
#' ll1 <- stack(latRast, longRast)
#' 
#' # same as:
#' ll2 <- fasterLongLatRasters(madForest2000, grassDir=grassDir)
#' 
#' # Example of chaining (ie, not reinitializing GRASS session):
#' # The second function uses the GRASS session initiated by the first function.
#' # It then uses the raster created in the GRASS session by the first function
#' # as the input for its module.
#' latRast <- faster('r.latlong', rast=madForest2000, outType='rast',
#' outName='lat', flags=c('quiet', 'overwrite'), grassDir=grassDir)
#' longRast <- faster('r.latlong', input='lat', outType='rast', outName='long',
#' flags=c('quiet', 'overwrite', 'l'), init=FALSE, grassDir=grassDir)
#' ll3 <- stack(latRast, longRast)



