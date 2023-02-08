\dontrun{

# IMPORTANT: These function use the "location", "restartGrass", and
# "warn" arguments to avoid interfering with an existing GRASS session.
# WHEN YOU ARE DONE WITH THE EXAMPLES, run this line to revert to your
# active GRASS session:
# startFaster(location='default') # change "location" if not "default"

# IMPORTANT: Change this to where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

library(sf)
library(terra)

madElev <- fasterData('madElev')

### mean of square neighborhood with all cells of equal weight
# Package terra is faster for small neighborhoods, but fasterRaster is faster
# for large neighborhoods (w > ~100 in this example) if using >= 4 cores.

square <- fasterFocal(madElev, w=3, fun='mean', cores=2, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

terraSquare <- terra::focal(madElev, w=3, fun='mean', na.rm=TRUE)

# same
global(abs(square - terraSquare), 'sum', na.rm=TRUE)


### maximum in circular vs square neighborhood
circle <- fasterFocal(madElev, w=3, fun='max', circle=TRUE, cores=2,
outGrassName='circleFocal', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

square <- fasterFocal('madElev', w=3, fun='max', cores=2, 
outGrassName='squareFocal', grassDir=grassDir,
location='examples')

plot(square - circle)


### focal mean of square neighborhood with cells of arbitrary weight
w <- matrix(c(NA, 1, NA, 1, NA, 1, NA, 1, NA), nrow=3)
w 

custom <- fasterFocal(madElev, w=w, fun='mean', cores=2, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

terraCustom <- terra::focal(madElev, w=w, fun='mean', na.rm=TRUE)

# same
global(abs(custom - terraCustom), 'sum', na.rm=TRUE)


### population standard deviation vs standard deviation
sdPop <- fasterFocal(madElev, w=3, fun='sdPop', cores=2,
outGrassName='sdPopFocal', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

sdSamp <- fasterFocal('madElev', w=3, fun='sd', cores=2,
outGrassName='sdSampFocal', grassDir=grassDir,
location='examples') # line for examples only

plot(sdSamp - sdPop)

# terra calculates the sample standard deviation
sdSampTerra <- focal(madElev, w=3, 'sd', na.rm=TRUE)

# same to within more than floating precision
sdSamp - sdSampTerra
global(abs(sdSamp - sdSampTerra), 'sum', na.rm=TRUE)


### median, mode, max, min, range, count, diversity, interspersion
median <- fasterFocal(madElev, w=3, fun='median', cores=2,
outGrassName='medianFocal', grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

mode <- fasterFocal('madElev', w=3, fun='mode', cores=2,
outGrassName='modeFocal', grassDir=grassDir,
location='examples') # line for examples only

max <- fasterFocal('madElev', w=3, fun='max', cores=2,
outGrassName='maxFocal', grassDir=grassDir,
location='examples') # line for examples only

min <- fasterFocal('madElev', w=3, fun='min', cores=2,
outGrassName='minFocal', grassDir=grassDir,
location='examples') # line for examples only

range <- fasterFocal('madElev', w=3, fun='range', cores=2,
outGrassName='rangeFocal', grassDir=grassDir,
location='examples') # line for examples only

count <- fasterFocal('madElev', w=3, fun='count', cores=2,
outGrassName='countFocal', grassDir=grassDir,
location='examples') # line for examples only

diversity <- fasterFocal('madElev', w=3, fun='diversity', cores=2,
outGrassName='diversityFocal', grassDir=grassDir,
location='examples') # line for examples only

interspersion <- fasterFocal('madElev', w=3, fun='interspersion', cores=2,
outGrassName='interspersionFocal', grassDir=grassDir,
location='examples') # line for examples only

quant25 <- fasterFocal('madElev', w=3, fun='quantile', cores=2,
quantile=0.25, outGrassName='quant25Focal', grassDir=grassDir,
location='examples') # line for examples only

quant75 <- fasterFocal('madElev', w=3, fun='quantile', cores=2,
quantile=0.75, outGrassName='quant75Focal', grassDir=grassDir,
location='examples') # line for examples only

outs <- c(median, mode, max, min, range, count,
	diversity, interspersion, quant25, quant75)
plot(outs)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
