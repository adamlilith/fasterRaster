if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Importing a raster sets the region to match its extent and dimensions:
elev <- fast(madElev)
elev
.region()

# Importing a vector does not change the region:
dypsis <- fast(madDypsis)
.region()

# report information on current region:
.regionDim()
.regionExt()
.regionRes()

dim()
nrow()
ncol()
ndepth()
ncell()
ncell3d()
nlyr()

ext()
west()
east()
south()
north()

res()
res3d()
xres()
yres()
zres()

# Operations on rasters can change the region:
(startingRegion <- .region())
aggElev <- aggregate(elev, 2)
.region()

# reset region dimensions
dim()
.regionDim(elev, respect = "resolution")
dim()

# reset region extent
ext()
.regionExt(dypsis, respect="resolution")
ext()

# reset region resolution
res()
.regionRes(elev, respect="extent")
res()

}
