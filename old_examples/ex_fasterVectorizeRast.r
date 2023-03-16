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

# Converting this raster to a vector can take a lot of time. To speed the
# example, we will create a MASK raster to restrict analysis to a smaller
# region. To do this, we'll create a SpatVector in R that covers the
# area of analysis.

### convert forest raster to points
###################################

# raster of forest (1 = forest, NA = other)
madForest2000 <- fasterData('madForest2000')

# start GRASS and import raster
startFaster(madForest2000,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# create MASK raster
xy <- cbind(
	x = c(740914, 746774, 746774, 740914),
	y = c(1072607, 1072607, 1076195, 1076195)
)
area <- vect(xy, 'polygon', crs=crs(madForest2000))

fasterMakeMask(area, 'maskVect', grassDir=grassDir, grassToR=FALSE,
location='examples') # line for examples only

# vectorize forest raster to points
points <- fasterAsPoints('madForest2000', grassDir=grassDir,
location='examples') # line for examples only

plot(madForest2000, ext=area, col='forestgreen')
plot(points, pch='.', col='yellow', add=TRUE)

### vectorize river raster to lines
###################################

# NB This is a contrived example, as we start with a rivers vector, then
# rasterize it to a raster. We then thin the raster to create better lines,
# and finally vectorize it to lines.
madRivers <- fasterData('madRivers')

fasterRasterize(madRivers, 'madElev', inVectName='madRivers', grassToR=FALSE,
outGrassName='riverRast',
location='examples') # line for examples only

riverPolys <- fasterAsLines('riverRast', grassDir=grassDir,
location='examples') # line for examples only

plot(riverPolys)
plot(st_geometry(madRivers), col='red', ext=area, add=TRUE)

### vectorize elevation map to polygons
#######################################

# elevation raster
madElev <- fasterData('madElev')

# vectorize forest to polygons
forestPolys <- fasterAsPolygons('madForest2000', grassDir=grassDir,
outGrassName='forestPolys',
location='examples') # line for examples only

plot(madForest2000, ext=area)
plot(forestPolys, add=TRUE)

# vectorize to polygons
elevPolys <- fasterAsPolygons('madElev', grassDir=grassDir,
outGrassName='elevPolys',
location='examples') # line for examples only

# make color ramp palette and plot
elevPolys <- elevPolys[order(elevPolys$madElev), ]
unis <- unique(elevPolys$madElev)
uniqueCols <- terrain.colors(length(unis), rev=TRUE)

rle <- rle(elevPolys$madElev)
col <- character()
for (i in seq_along(unis)) {
	col <- c(col, rep(uniqueCols[i], each=rle$length[i]))
}

plot(elevPolys, col=col, border=NA)

# Revert back to original GRASS session if needed.
# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
