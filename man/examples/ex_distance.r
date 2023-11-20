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
grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
grassDir <- "/usr/local/grass" # Linux

# setup
library(sf)
library(terra)

# elevation raster, rivers vector, locations of Dypsis plants
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

### case 1: GRaster by itself

# distance between NA cells and nearest non-NA cells
naDist <- distance(elev)
names(naDist) <- "NA Distance"
plot(naDist)

# distance between non-NA cells and nearest NA cells
nonNaDist <- distance(elev, fillNA = FALSE)
names(nonNaDist) <- "non-NA Distance"
plot(nonNaDist)

# distance between cells with an elevation of 3 and any other cell that != 3
distFocal3 <- distance(elev, target = 3)
names(distFocal3) <- "Distance from 3"
plot(distFocal3)

# distance between any cell and cells with a value of 3
distTo3 <- distance(elev, fillNA = FALSE, target = 3)
names(distTo3) <- "Distance to 3"
plot(distTo3)

### case 2: GRaster and GVector
distToVect <- distance(elev, rivers)

plot(distToVect)
plot(st_geometry(madRivers), add = TRUE)

### case 3: GVector vs GVector
plot(st_geometry(madRivers))
plot(st_geometry(madDypsis), add = TRUE)

distToRivers <- distance(dypsis, rivers, unit = "yd")
distToPlants <- distance(rivers, dypsis)
distToRivers
distToPlants

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
