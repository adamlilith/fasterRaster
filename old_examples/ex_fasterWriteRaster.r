\dontrun{
# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# initGrass(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(terra)

### save a raster to disk to from GRASS

### start GRASS session with a raster
madElev <- fasterData('madElev')

startFaster(madElev, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# save the raster to disk (using temporary folder here)
filename <- paste0(tempdir(), '/madElev.tif')
madElevWritten <- fasterWriteRaster('madElev', filename)

me <- rast(filename)
me <- setMinMax(me)

madElev
me

# Can also use read_RAST from 'rgrass', but this brings in the color table
# and may not set the data type correctly (truncating values; not shown here).
madElevImport <- rgrass::read_RAST('madElev', flags='quiet')

madElev
madElevImport

# brings in its own color table
plot(c(madElev, madElevImport))

### saving a raster imported from GRASS to R using writeRaster4() and ~8()
##########################################################################

# Save in single- and double-precision floating-point formats.
# This is overkill and for this raster will result in a much larger file
# size than is necessary. This raster only has integers, so it could be
# saved with an appropriate "datatype" argument.
writeRaster4(madElevImport, './madElevImport4.tif')
writeRaster8(madElevImport, './madElevImport8.tif')

# best option for this example (smallest file size)
writeRaster(madElevImport, './madElevImportBest.tif', datatype='INT2S')

# save GeoTIFF directly from GRASS... actually smaller than the original!
fasterWriteRaster('madElev', filename='./madElevFromGrass.tif',
datatype='INT2S')

# save ASCII directly from GRASS
# NB Sometimes creates an error about XML, in which case you may need to
# restart R again... sorry, unsure what's going on here!
fasterWriteRaster('madElev', filename='./madElevFromGrass.asc')

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
