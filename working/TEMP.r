# source('C:/Ecology/Drive/R/fasterRaster/working/TEMP.r')

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

