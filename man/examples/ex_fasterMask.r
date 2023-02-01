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
madForest2000 <- fasterData('madForest2000') # large raster
madCoast4 <- fasterData('madCoast4') # commune outlines

# get a commune
madAnt <- madCoast4[madCoast4$NAME_4 == 'Antanambe', ]

### mask with a vector: fasterMask
##################################

# make a mask from a commune
masked <- fasterMask(madElev, mask=madAnt, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# plot
grays <- colorRampPalette(c('white', 'gray20'))
grays <- grays(10)

plot(madElev, col=grays, legend=FALSE)
plot(masked, add=TRUE)
plot(st_geometry(madAnt), add=TRUE)

### mask with a raster: fasterMask
##################################

# make a mask from a raster
masked <- fasterMask(madElevAnt, mask=madElevMan, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# plot
grays <- colorRampPalette(c('white', 'gray20'))
grays <- grays(10)

reds <- colorRampPalette(c('pink', 'darkred'))
reds <- reds(10)

blues <- colorRampPalette(c('lightblue', 'darkblue'))
blues <- blues(10)

plot(madElev, col=grays, legend=FALSE)
plot(madElevAnt, col=reds, legend=FALSE, add=TRUE)
plot(madElevMan, col=blues, legend=FALSE, add=TRUE)
plot(masked, add=TRUE)

### create a persistent mask: fasterMask
########################################

# create mask from raster
fasterMask(madElevAnt, mask=madElevMan, removeMask=FALSE,
grassDir=grassDir, grassToR=FALSE,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# What did we make? The mask is always named "MASK".
fasterLs()

# importing then exporting a raster will crop it
fasterRast(madForest2000)
forestMasked <- importFromGrass('madForest2000')

plot(madForest2000, col='gray', legend=FALSE)
plot(forestMasked, col='darkgreen', add=TRUE)

# Doing operations on a raster masks it.
slope <- fasterTerrain(madElev, v='slope', cores=2, grassDir=grassDir,
location='examples') # line for examples only

grays <- colorRampPalette(c('white', 'gray20'))
grays <- grays(10)

plot(madElev, col=grays, legend=FALSE)
plot(slope, add=TRUE)

# Remove the mask.
fasterRename('MASK', 'MASKold')

# Operations no longer masked
slopeUnmasked <- fasterTerrain('madElev', v='slope',
outGrassName='slopeUnmasked', cores=2, grassDir=grassDir,
location='examples') # line for examples only

grays <- colorRampPalette(c('white', 'gray20'))
grays <- grays(10)

plot(slopeUnmasked, legend=TRUE)
plot(slope, col=grays, legend=FALSE, add=TRUE)

### create a persistent mask: fasterMakeMask
############################################

# create mask from raster
fasterMakeMask(madElevMan, grassDir=grassDir, grassToR=FALSE,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# What did we make? The mask is always named "MASK".
fasterLs()

# importing then exporting a raster will crop it
fasterRast(madForest2000)
forestMasked <- importFromGrass('madForest2000')

plot(madForest2000, col='gray', legend=FALSE)
plot(forestMasked, col='darkgreen', add=TRUE)

# Doing operations on a raster masks it.
slope <- fasterTerrain(madElev, v='slope', cores=2, grassDir=grassDir,
location='examples') # line for examples only

grays <- colorRampPalette(c('white', 'gray20'))
grays <- grays(10)

plot(madElev, col=grays, legend=FALSE)
plot(slope, add=TRUE)

# Remove the mask.
fasterRename('MASK', 'MASKold')

# Operations no longer masked
slopeUnmasked <- fasterTerrain('madElev', v='slope',
outGrassName='slopeUnmasked', cores=2, grassDir=grassDir,
location='examples') # line for examples only

grays <- colorRampPalette(c('white', 'gray20'))
grays <- grays(10)

plot(slopeUnmasked, legend=TRUE)
plot(slope, col=grays, legend=FALSE, add=TRUE)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
initGrass(location='default')

}
