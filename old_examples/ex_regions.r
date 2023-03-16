\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# startFaster(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC

library(sf)
library(terra)

# get data
madElev <- fasterData('madElev') # large raster
madElevAnt <- fasterData('madElevAnt') # small raster
madRivers <- fasterData('madRivers') # small vector

### initiate a GRASS session and export a large raster
startFaster(rast=madElev, grassDir=grassDir,
restartGrass=TRUE, warn=FALSE, location='examples')

# export the smaller raster and vector
fasterRast(madElevAnt)
fasterVect(madRivers, inVectName='madRivers')

### region properties
regionExt() # extent
regionNcell() # number of cells
regionDim() # dimensions
regionNrow() # rows
regionNcol() # columns

# extent of spatial objects
fasterExt('madElev') # large raster
fasterExt('madElevAnt') # small raster
fasterExt('madElevMan') # small raster
fasterExt('madRivers') # small vector
fasterExt() # extent of all combined

# extent of region (same as largest raster)
regionExt()

### resize region using small raster already in GRASS
regionChangeExt('madElevAnt')
fasterExtRegion()

# import the large raster to R using this region
cropped <- rgrass::read_RAST('madElev', flags='quiet')

# plot: larger raster has been cropped!
plot(madElev, col=paste0('gray', 1:80), main='Cropped')
plot(cropped, legend=FALSE, add=TRUE)

### resize region using small vector already in GRASS
regionResize('madRivers')
fasterExtRegion()

# import the large raster using this region
cropped <- rgrass::read_RAST('madElev', flags='quiet')

# plot: larger raster has been cropped!
plot(madElev, col='red', main='Cropped', legend=FALSE)
plot(cropped, add=TRUE)
plot(st_geometry(madRivers), col='yellow', add=TRUE)

### resize region using a small raster in R
regionResize(madElevMan)
fasterExtRegion()

# import the large raster into R using this region
cropped <- rgrass::read_RAST('madElev', flags='quiet')

# plot: larger raster has been cropped!
plot(madElev, col=paste0('gray', 0:80), main='Cropped', legend=FALSE)
plot(cropped, add=TRUE)

### resize region using user-defined coordinates
extent <- c(740000, 750000, 1060000, 1070000)
regionResize(extent)
fasterExtRegion()

# import the large raster using this region
cropped <- rgrass::read_RAST('madElev', flags='quiet')

# plot: larger raster has been cropped!
plot(madElev, col=paste0('gray', 0:80), main='Cropped', legend=FALSE)
plot(cropped, add=TRUE)

### resize region by growing by a certain number of cells
grow <- 200
regionResize() # first, expand region to encompass all objects
regionResize(grow) # now, grow
fasterExtRegion()

# import the large raster using this region
expanded <- rgrass::read_RAST('madElev', flags='quiet')

# plot: raster has been extended!
plot(expanded, main='Expanded')

# Revert back to original GRASS session.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}