if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Calculate global statistics:
global(elev, fun = c("mean", "countNonNA", "countNA", "var", "varpop"))

}
