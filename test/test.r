rm(list=ls())
library(omnibus)
library(sf)
library(terra)

drive <- 'C:'
grassDir <- paste0('C:/Program Files/GRASS GIS 8.2') # example for a PC

# ff <- listFiles(paste0(drive, '/ecology/Drive/R/fasterRaster/R'))
# for (f in ff) source (f)

library(fasterRaster)
library(omnibus)

rastFile <- system.file('extdata', 'madElev.tif', package='fasterRaster')
madElev <- rast(rastFile)

rastFile <- system.file('extdata', 'madForest2000.tif', package='fasterRaster')
madForest2000 <- rast(rastFile)

say('#########################')
say('### exportRastToGrass ###')
say('#########################')

# start a GRASS session
initGrass(madForest2000, rastName = 'madForest2000', grassDir = grassDir)
exportRastToGrass(madElev, grassName = 'madElev', grassDir = grassDir)

