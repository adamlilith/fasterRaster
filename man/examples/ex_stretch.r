if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

### Stretch based on user-defined range
#######################################

#  fasterRaster
fr <- stretch(elev, smin=1, smax=100)
fr

# terra
tr <- stretch(madElev, smin = 1, smax = 100)
tr

# Compare fasterRaster to terra output
fr <- rast(fr)
fr <- extend(fr, tr)
fr - tr

### Stretch values in a certain quantile range
##############################################

#  fasterRaster
fr <- stretch(elev, minq = 0.25, maxq = 0.75)
fr

# terra
tr <- stretch(madElev, minq = 0.25, maxq = 0.75)
tr

# Compare fasterRaster to terra output
fr <- rast(fr)
fr <- extend(fr, tr)
fr - tr

}
