if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Plant specimens (points), elevation (raster)
madDypsis <- fastData("madDypsis")
madElev <- fastData("madElev")

# Convert a sf to a GVector, and a SpatRaster to a GRaster
dypsis <- fast(madDypsis)
elev <- fast(madElev)

# Calculate viewshed from two points:
loc <- dypsis[4:5]
vs <- viewshed(elev, loc)

plot(vs)
plot(loc, add = TRUE)

}
