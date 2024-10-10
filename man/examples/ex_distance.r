if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, rivers vector, locations of Dypsis plants
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# Convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

### case 1: GRaster by itself

# Distance between NA cells and nearest non-NA cells
naDist <- distance(elev)
names(naDist) <- "NA Distance"
plot(naDist)

# Distance between non-NA cells and nearest NA cells
nonNaDist <- distance(elev, fillNA = FALSE)
names(nonNaDist) <- "non-NA Distance"
plot(nonNaDist)

# Distance between cells with an elevation of 3 and any other cell that != 3
distFocal3 <- distance(elev, target = 3)
names(distFocal3) <- "Distance from 3"
plot(distFocal3)

# Distance between any cell and cells with a value of 3
distTo3 <- distance(elev, fillNA = FALSE, target = 3)
names(distTo3) <- "Distance to 3"
plot(distTo3)

### Case 2: GRaster and GVector
distToVect <- distance(elev, rivers)

plot(distToVect)
plot(rivers, add = TRUE)

### Case 3: GVector vs GVector
plot(rivers)
plot(dypsis, add = TRUE)

distToRivers <- distance(dypsis, rivers, unit = "yd")
distToPlants <- distance(rivers, dypsis)
distToRivers
distToPlants

### Case 4: GVector vs itself
distToItself <- distance(dypsis)
distToItself

}
