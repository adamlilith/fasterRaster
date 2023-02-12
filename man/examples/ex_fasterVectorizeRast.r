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
# raster of forest (1 = forest, NA = other)
madForest2000 <- fasterData('madForest2000')

xy <- cbind(
	x = c(740914, 746774, 746774, 740914),
	y = c(1072607, 1072607, 1076195, 1076195)
)
area <- vect(xy, 'polygon', crs=crs(madForest2000))

# create MASK raster
fasterMakeMask(area, 'maskVect', grassDir=grassDir, grassToR=FALSE,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

# vectorize forest raster to points
points <- fasterAsPoints(madForest2000, grassDir=grassDir,
location='examples') # line for examples only

oldpar <- par(mfrow=c(1, 2))
plot(madForest2000, ext=area, col='forestgreen')
plot(points, pch='.')
par(oldpar)

### vectorize elevation map to polygons
# elevation raster
madElev <- fasterData('madElev')

# vectorize to polygons
polys <- fasterAsPolygons(madElev, grassDir=grassDir,
location='examples') # line for examples only

# make color ramp palette and plot
polys <- polys[order(polys$cat), ]
unis <- unique(polys$cat)
pal <- colorRampPalette(c('white', 'darkgreen'))
uniqueCols <- pal(length(unis))

rle <- rle(polys$cat)
col <- character()
for (i in seq_along(unis)) {
	col <- c(col, rep(uniqueCols[i], each=rle$length[i]))
}

oldpar <- par(mfrow=c(1, 2))
plot(polys, col=col, border=NA)
plot(madElev, ext=area)
par(oldpar)

### vectorize river raster to lines
# NB This is a contrived example, as we start with a rivers vector, then
# rasterize it to a raster. We then thin the raster to create better lines,
# and finally vectorize it to lines.
madRivers <- fasterData('madRivers')

fasterRasterize()!!!!!!!!!!!!

polys <- fasterAsPolygons(madElev, grassDir=grassDir,
location='examples') # line for examples only


}
