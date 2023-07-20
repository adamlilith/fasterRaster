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
grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.3' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# elevation raster, rivers vector
madElev <- fastData('madElev')
madRivers <- fastData('madRivers')

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)

### buffer a raster by a given distance
buffByDist <- buffer(elev, width = 2000) # 2000-m buffer
plot(buffByDist, legend=FALSE)
plot(madElev, add=TRUE)

### buffer a raster by a given number of cells
buffByCells <- buffer(elev, width = 20.01, unit = 'cells') # 20-cell buffer
plot(buffByCells)
plot(madElev, add=TRUE)

### buffer a vector
buffRivers <- buffer(rivers, width = 2000) # 2000-m buffer
plot(buffRivers)
plot(st_geometry(madRivers), col = 'blue', add = TRUE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
