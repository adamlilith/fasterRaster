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

library(sf)
library(terra)

# load data used in examples
madElev <- fasterData('madElev') # large raster
madElevAnt <- fasterData('madElevAnt') # small raster
madElevMan <- fasterData('madElevMan') # small raster
madDypsis <- fasterData('madDypsis') # point occurrences

### Crop a raster using another smaller raster
cropped <- fasterCropRast(madElevAnt, madElevMan, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# make some color palettes for plots
grays <- colorRampPalette(c('gray0', 'gray80'))
grays <- grays(10)

greens <- colorRampPalette(c('lightgreen', 'darkgreen'))
greens <- greens(10)

reds <- colorRampPalette(c('pink', 'darkred'))
reds <- reds(10)

plot(madElev, col=grays)
plot(madElevAnt, col=greens, legend=FALSE, add=TRUE)
plot(madElevMan, col=reds, legend=FALSE, add=TRUE)
plot(cropped, legend=FALSE, add=TRUE)

# import larger raster
fasterRast(madElev)

# retreive larger raster
madElevCrop <- rgrass::read_RAST('madElev')

# compare extents
plot(madElev, col=paste0('gray', 0:80), legend=FALSE)
plot(madElevCrop, legend=FALSE, add=TRUE)

### Resample a raster

resampled <- fasterResampleRast(madElev, c(20, 25), inits=inits)






# EXAMPLE 2: Trim NA columns and rows
# First, we set up a new GRASS session using a raster with a large extent.
# Then, we trim the smaller raster using fasterTrimRast(). This also crops
# the larger raster to the smaller raster's extent!

# initialize GRASS session with larger raster
initGrass(madElev, restartGrass=TRUE, grassDir=grassDir)
fasterRast(madElevAnt)

# get copy of raster that has been "padded" with NA columns/rows
padded <- rgrass::read_RAST('madElevAnt')

# trim
# Note that madElevAnt is already in the GRASS session, so we just tell
# the function its name in the session to speed this up.
trimmed <- fasterTrimRast('madElevAnt', grassDir=grassDir)

# compare extents
oldPar <- par(mfrow=c(1, 2))
plot(untrimmed, main='Padded')
plot(trimmed, main='Trimmed')
par(oldPar)

# EXAMPLE 3: Crop and/or extend a raster using fasterCropExtendRast()
# The elevation raster from the Antanambe and Manompana communes
# of Madagascar overlap somewhat. We will extend and crop the Antanambe
# by the Manompana raster.
# Note that this function restarts the GRASS session and thus removes all
# existing rasters and vectors from the session.

cropped <- fasterCropRast(madElevAnt, template=madElevMan,
grassDir=grassDir)

# plot... create new color palletes so we can see the new rasters
reds <- colorRampPalette(c('pink', 'darkred'))(10)
greens <- colorRampPalette(c('lightgreen', 'darkgreen'))(10)

plot(madElev, col=paste('gray', 0:80))
plot(madElevAnt, col=reds, legend=FALSE, add=TRUE)
plot(madElevMan, col=greens, legend=FALSE, add=TRUE)
plot(cropped, legend=FALSE, add=TRUE)

}
