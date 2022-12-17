\donttest{

library(terra)

# change this to where GRASS is installed on your system:
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

madForest2000 <- fasterData('madForest2000')

latRast <- faster('r.latlong', rast=madForest2000, outType='rast',
flags=c('quiet', 'overwrite'), grassDir=grassDir)

longRast <- faster('r.latlong', rast=madForest2000, outType='rast',
flags=c('quiet', 'overwrite', 'l'), grassDir=grassDir)

ll1 <- c(latRast, longRast)

# In this case, the above is the same as:
ll2 <- fasterLongLatRasts(madForest2000, grassDir=grassDir)

# Example of chaining (ie, not reinitializing GRASS session):
# The second function uses the GRASS session initiated by the first function.
# It then uses the raster created in the GRASS session by the first function
# as the input for its module.

latRast <- faster('r.latlong', rast=madForest2000, outType='rast',
output='lat', flags=c('quiet', 'overwrite'), grassDir=grassDir)

longRast <- faster('r.latlong', input='lat', outType='rast', output='long',
flags=c('quiet', 'overwrite', 'l'), init=FALSE, grassDir=grassDir)

ll3 <- c(latRast, longRast)

}
