if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Calculate global statistics:
global(elev, fun = c("mean", "var", "varpop"))
global(elev, "quantile", probs = c(0.25, 0.5, 0.75))

global(elev, "*") # calculate all available functions

global() # vector of all functions

}
