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

# load data used in examples
madElev <- fasterData('madElev') # large raster
madElevAnt <- fasterData('madElevAnt') # small raster
madElevMan <- fasterData('madElevMan') # small raster
madDypsis <- fasterData('madDypsis') # point occurrences

### Crop a raster using another smaller raster
cropped <- fasterCropRast(madElevAnt, template=madElevMan, grassDir=grassDir,
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

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
