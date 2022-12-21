# Examples with scalar and scalar vector:
fasterConvertDegree(0, grassDir)
fasterConvertDegree(seq(0, 360, by=90))

\dontrun{

library(terra)

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

# Example with a raster:
# First, generate an aspect raster from an elevation raster.
# This is a contrived example because we could set the argument northIs0 in
# fasterTerrain() to TRUE and get the raster we will name aspNorthIs0 from
# that function in one step. Note that this could be done faster by
# "chaining" fasterTerrain() and fasterConvertDegree().
madElev <- fasterData('madElev')

aspEastIs0 <- fasterTerrain(madElev, metrics='aspect', northIs0=FALSE,
grassDir=grassDir)

aspNorthIs0 <- fasterConvertDegree(aspEastIs0, grassDir=grassDir)

oldPar <- par(mfrow=c(1, 2))
plot(aspEastIs0, main='0 deg = east')
plot(aspNorthIs0, main='0 deg = north')
par(oldPar)

}
