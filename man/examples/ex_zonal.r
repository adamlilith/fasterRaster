if (grassStarted()) {

# Setup
library(terra)

# Elevation SpatRaster:
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

### Calculate zonal statistics using a GRaster as zones
#######################################################

# Generate a "zones" GRaster by dividing raster into areas based on
# high/low elevation.
names(elev) # Use this name in app() formula.
fun <- "= if (madElev <200, 0, if (madElev <400, 1, 2))"
zones <- app(elev, fun = fun)

# Calculate zonal statistics using a raster as zones
zonal(elev, zones, fun = "mean")
zonal(elev, zones, fun = "*") # all statistics

# Calculate zonal statistics on multi-layered GRaster
elev2 <- c(elev, log10(elev))
zonal(elev2, zones, fun = c("mean", "sum", "sdpop"))

### Calculate zonal statistics using a GVector as zones
#######################################################

madCoast4 <- fastData("madCoast4")
coast <- fast(madCoast4)

zonal(elev, z = coast, fun = "mean")

}
