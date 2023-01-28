\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# For fasterRaster functions below, this will be used to set up a location
# that does not interfere with any locations you may be using.
inits <- list(location='examples', restartGrass=TRUE, warn=FALSE)

library(sf)
library(terra)

# change this to where GRASS is installed on your system:
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC

madElev <- fasterData('madElev')

### Example using a raster as input, raster as output:
# "r.latlong" creates a raster with cell values equal to latitude (WGS84).
# Note that fasterLongLatRasts() is easier to use than faster() in this case.
#
lat <- faster('r.latlong', rast=madElev, out='rast', grassDir=grassDir,
inits=inits) # typically use "inits" for examples only

plot(lat)

### Example using a raster as input, vector as output
# "r.contour" creates a vector of contours
# Note that fasterContour() is easier to use.
conts <- faster('r.contour', rast=madElev, out='vect',
levels=c(100, 200, 300, 400, 500), grassDir=grassDir, 
inits=inits) # typically use "inits" for examples only

plot(madElev)
plot(conts, add=TRUE)

### Example using a vector as input, raster as output.
# "v.to.rast" creates a raster from a vector (i.e., rasterizes it).
# Note that fasterRasterize() is easier to use.

madDypsis <- fasterData('madDypsis')

rastDypsis <- faster('v.to.rast', vect=madDypsis, inVectName='madDypsis',
rast=madElev, use='val', value=1, out='rast',
outGrassName='rastDypsis', grassDir=grassDir,
inits=inits) # typically use "inits" for examples only

rastRivers <- faster('v.to.rast', vect=madRivers, outGrassName='rastRivers',
use='val', value=1, out='rast', grassDir=grassDir,
inits=inits) # typically use "inits" for examples only

plot(madElev)
plot(rastRivers, col='blue', add=TRUE)

### Example using a vector as input, vector as output.
# "v.buffer" creates a buffer around a vector.
# Note that fasterBufferVect() is easier to use.
riverBuff <- faster('v.buffer', vect=madRivers, outGrassName='riverBuff',
distance = 1000, out='vect', grassDir=grassDir,
inits=inits) # typically use "inits" for examples only

plot(riverBuff)
plot(st_geometry(madRivers), col='blue', add=TRUE)

# Example using "chaining".
# "v.buffer" creates a buffer around a vector.
# "v.to.rast" creates a raster from a vector (i.e., rasterizes it).
# Note that fasterBufferVect() and fasterRasterize() would be easier to use.
faster('v.buffer', vect=madRivers, outGrassName='riverBuff',
distance = 1000, out='vect', grassDir=grassDir, grassToR=FALSE,
inits=inits) # typically use "inits" for examples only

rastBuff <- faster('v.to.rast', vect='riverBuff', outGrassName='rastRivers',
use='val', value=1, out='rast', grassDir=grassDir)

plot(rastBuff)
plot(st_geometry(madRivers), col='blue', add=TRUE)

# Revert back to original GRASS session.
# Change to your working location if not "default" (it usually is).
initGrass(location='default')

}
