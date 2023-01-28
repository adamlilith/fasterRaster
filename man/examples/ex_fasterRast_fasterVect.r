
\dontrun{

library(sf)
library(terra)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

### Initiate a GRASS session using a raster already in R

# start a GRASS session
# (typically, using the default value of "location" is best)
madForest2000 <- fasterData('madForest2000')
initGrass(rast=madForest2000, grassDir=grassDir, location='examples')

# export a raster to GRASS
madElev <- fasterData('madElev')
fasterRast(madElev)

# re-export the raster and overwrite
fasterRast(madElev, replace=TRUE)

# export a vector to GRASS
madCoast0 <- fasterData('madCoast0')
fasterVect(madCoast0, inVectName='madCoast0')

# what did we export?
fasterLs()

# export a raster from a file
rastFile <- system.file('extdata', paste0('madForest2014.tif'),
package='fasterRaster')

fasterRast(rastFile)

# import the raster and vector back to R
importElev <- rgrass::read_RAST('madElev', flags='quiet')
importCoast0 <- rgrass::read_VECT('madCoast0', flags='quiet')

# Importing a raster that is larger than an existing GRASS region crops
# the raster to the extent of the region. Vectors that are larger than the
# region are *not* cropped.
#
# Example for a raster:

# start a GRASS session with a small raster
# Typically, using the default value of "location" is best.
madElevAnt <- fasterData('madElevAnt')
initGrass(rast=madElevAnt, restartGrass=TRUE, grassDir=grassDir,
location='examples')

# export a larger raster into this region
fasterRast(madElev, inRastName='madElev')

# import madElev raster back from the GRASS session
madElevCrop <- rgrass::read_RAST('madElev', flags='quiet')

ext(madElev) # extent of the original raster
ext(madElevCrop) # extent of the croppped raster
fasterExtRegion() # extent of the GRASS region

plot(madElev, col=paste0('gray', seq(0, 80, by=10)), legend=FALSE)
plot(madElevCrop, legend=FALSE, add = TRUE)

### Spatial vectors are *NOT* cropped!

# start a GRASS session with a small vector
madCoast4 <- fasterData('madCoast4')
ant <- madCoast4[madCoast4$NAME_4 == 'Antanambe', ]

# Typically, using the default value of "location" is best.
initGrass(vect=ant, inVectName='ant', restartGrass=TRUE, grassDir=grassDir,
location='examples')

# export a larger vector into this region
madRivers <- fasterData('madRivers')
fasterVect(madRivers, inVectName='madRivers')

# import madElev raster back from the GRASS session
madRiversCrop <- rgrass::read_VECT('madRivers', flags='quiet')

ext(madRivers) # extent of the original raster
ext(madRiversCrop) # extent of the croppped raster
fasterExt() # extent of the GRASS region

plot(madElev, legend=FALSE)
plot(st_geometry(madRivers), col='lightblue', lwd=4, add = TRUE)
plot(madRiversCrop, col='red', add = TRUE)

}
