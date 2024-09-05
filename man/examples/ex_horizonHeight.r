if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# convert a SpatRaster to a GRaster
elev <- fast(madElev)

# calculate horizon height in north and east directions
hhNorth <- horizonHeight(elev)
hhNorth
plot(hhNorth)

# calculate horizon height in east and north directions
hhEast <- horizonHeight(elev, northIs0 = FALSE)
hhEast
plot(hhEast)

}
