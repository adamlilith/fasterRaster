\dontrun{

library(sf)
library(terra)

# change this to where GRASS is installed on your system:
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madElev <- fasterData('madElev')

# Example using a raster as input, raster as output:
# "r.latlong" creates a raster with cell values equal to latitude (WGS84).
# Note that fasterLongLatRasts() is easier to use.
lat <- faster('r.latlong', rast=madElev, outType='rast',
flags=c('quiet', 'overwrite'), grassDir=grassDir)

plot(lat, add=TRUE)

# example using a raster as input, vector as output
# "r.contour" creates a vector of contours
# Note that fasterContour() is easier to use.
conts <- faster('r.contour', rast=madElev, outType='vect',
levels=c(100, 200, 300, 400, 500), flags=c('quiet', 'overwrite'),
grassDir=grassDir)

plot(madElev)
plot(conts, add=TRUE)

# Example using a vector as input, raster as output.
# "v.to.rast" creates a raster from a vector (i.e., rasterizes it).
# Note that fasterRasterize() is easier to use.
rastRivers <- faster('v.to.rast', vect=madRivers, outGrassName='rastRivers',
use='val', value=1,
outType='rast', flags=c('quiet', 'overwrite'), grassDir=grassDir)

plot(madElev)
plot(rastRivers, col='blue', add=TRUE)

# Example using a vector as input, vector as output.
# "v.buffer" creates a buffer around avector.
# Note that fasterBufferVect() is easier to use.
riverBuff <- faster('v.buffer', vect=madRivers, outGrassName='riverBuff',
distance = 1000,
outType='vect', flags=c('quiet', 'overwrite'), grassDir=grassDir)

plot(riverBuff)
plot(st_geometry(madRivers), col='blue', add=TRUE)

# Example using "chaining".
# "v.buffer" creates a buffer around avector.
# "v.to.rast" creates a raster from a vector (i.e., rasterizes it).
# Note that fasterBufferVect() and fasterRasterize() would be easier to use.
faster('v.buffer', vect=madRivers, outGrassName='riverBuff',
distance = 1000,
outType='vect', flags=c('quiet', 'overwrite'),
grassDir=grassDir, grassToR=FALSE)

rastBuff <- faster('v.to.rast', vect='riverBuff', outGrassName='rastRivers',
use='val', value=1,
outType='rast', flags=c('quiet', 'overwrite'), grassDir=grassDir)

plot(rastBuff)
plot(st_geometry(madRivers), col='blue', add=TRUE)

}
