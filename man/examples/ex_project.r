\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# elevation raster, climate raster, rivers vector
madElev <- fastData('madElev')
madChelsa <- fastData('madChelsa')
madRivers <- fastData('madRivers')

# For this example, we'll project the madRivers vector to WGS84 using terra.
# (The CHELSA raster stack is already in WGS84.)
madRivers <- vect(madRivers)
madRivers <- project(madRivers, madChelsa)

# working directory
wd <- forwardSlash(tempdir())

### Method 1: Project SpatRasters, SpatVectors, or sf vectors
#############################################################

# Initiate GRASS session:
faster(x = madElev, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for 

riversFromVect <- fast(madRivers)
chelsaFromRast <- fast(madChelsa)

### Method 2: Project rasters/vectors on disk
#############################################

vectFile <- system.file('extdata', 'shapes/madCoast.shp', package='fasterRaster')
coastProj <- fast(vectFile)
coastProj

rastFile <- system.file('extdata', 'madChelsa.tif', package='fasterRaster')
chelsaFromFile <- fast(rastFile, method='bilinear')
chelsaFromFile

# Compare rasters projected between GRASS "locations" and from a file:
minmax(chelsaSameRes) - minmax(chelsaFromFile)

### Method 3: Projecting between GRASS "locations"
##################################################

# Set up "to" location:
faster(x = madElev, grassDir = grassDir,
workDir = wd, location = 'exampleTo') # line only needed for examples

elev <- fast(madElev)

# set up "from" location and put a raster and vector in it
faster(x = madChelsa, grassDir = grassDir,
workDir = wd, location = 'exampleFrom') # line only needed for examples

chelsa <- fast(madChelsa)
rivers <- fast(madRivers)

# Re-activate the "exampleTo" "location":
fastRestore(location='exampleTo')

### Project vector into the "exampleTo" location:
riversProj <- project(rivers)
rivers
riversProj

### Project raster into the "exampleTo" location but do not resample:
chelsaSameRes <- project(chelsa)
chelsaSameRes

# Compare to terra:
chelsaSameResTerra <- project(madChelsa, crs(madElev))

minmax(chelsaSameRes)
minmax(chelsaSameResTerra)

chelsaSameResSpat <- rast(chelsaSameRes)
plot(chelsaSameResSpat - chelsaSameResTerra)

# Project raster into the "exampleTo" location and resample to match template. This can take a while...
chelsaNewRes <- project(chelsa, elev)
chelsaNewRes

# Compare to terra:
chelsaNewResTerra <- project(madChelsa, madElev)

minmax(chelsaNewRes)
minmax(chelsaNewResTerra)

chelsaNewResSpat <- rast(chelsaNewRes)
plot(chelsaNewResSpat - chelsaNewResTerra)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('exampleFrom')
removeSession('exampleTo')

}
