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

# load data
madCoast <- fasterData('madCoast0') # polygon vector
madDypsis <- fasterData('madDypsis') # point vector
madRivers <- fasterData('madRivers') # line vector
madElev <- fasterData('madElev') # raster

# get a commune
madAnt <- madCoast4[madCoast4$NAME_4 == 'Antanambe', ]

# rasterize (raster is all 1s)
antRast <- fasterRasterize(vect=madAnt, rast=madElev, grassDir=grassDir,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

grays <- paste0('gray', 20:100)

plot(madElev, col=grays, main='Portion of Eastern Madagascar')
plot(antRast, legend=FALSE, add=TRUE)
plot(st_geometry(madAnt), add=TRUE)

# rasterize polygons (raster values set to values in "OBJECTID" column)
idRast <- fasterRasterize(vect=madCoast4, rast=madElev, grassDir=grassDir,
use='field', field='OBJECTID',
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(idRast)
plot(st_geometry(madCoast4), add=TRUE)

# rasterize polygons (raster values are categorized by polygon)
catRast <- fasterRasterize(vect=madCoast4, rast=madElev, grassDir=grassDir,
use='cat',
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(catRast)
plot(st_geometry(madCoast4), add=TRUE)

# rasterize points (all cells will have a value of 5)
pointRast <- fasterRasterize(vect=madDypsis, rast=madElev, grassDir=grassDir,
use='value', value=5,
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

plot(pointRast, col='red') # very hard to see, but they are there!
plot(st_geometry(madDypsis), col='gray85', add=TRUE)

# rasterize lines (thin and thick versions)
thinLinesRast <- fasterRasterize(vect=madRivers, rast=madElev,
inVectName='madRivers', grassDir=grassDir, outGrassName='thinRast',
location='examples', restartGrass=TRUE, warn=FALSE) # line for examples only

thickLinesRast <- fasterRasterize(vect='madRivers', rast='madElev',
grassDir=grassDir, outGrassName='thickRast', thick=TRUE,
location='examples') # line for examples only

plot(c(thinLinesRast, thickLinesRast), col='blue')

# Change to your working location if not "default" (it usually is).
startFaster(location='default')

}
