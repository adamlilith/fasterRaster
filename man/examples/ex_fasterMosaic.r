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

# rasters
madElevAnt <- fasterData('madElevAnt')
madElevMan <- fasterData('madElevMan')
madCoast0 <- fasterData('madCoast0')

# make different color palettes for each raster so we can differentiate them
antCol <- colorRampPalette(c('lightblue', 'darkblue'))
manCol <- colorRampPalette(c('pink', 'darkred'))

antCol <- antCol(10)
manCol <- manCol(10)

plot(st_geometry(madCoast0))
plot(madElevAnt, col=antCol, legend=FALSE, add=TRUE)
plot(madElevMan, col=manCol, legend=FALSE, add=TRUE)

inRastName <- c('ant', 'man') # AntMan~!

combo <- fasterMosaic(madElevAnt, madElevMan, inRastName=inRastName, cores=2,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(combo)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
