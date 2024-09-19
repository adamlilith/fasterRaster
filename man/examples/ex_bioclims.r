if (grassStarted()) {

# Setup
library(terra)

# Load rasters with precipitation and min/max temperature
madPpt <- fastData("madPpt")
madTmin <- fastData("madTmin")
madTmax <- fastData("madTmax")

### Classic and extended BIOCLIMs from SpatRasters
bcSR <- bioclims(madPpt, madTmin, madTmax, bios = "*")
bcSR

### BIOCLIMs from GRasters
ppt <- fast(madPpt)
tmin <- fast(madTmin)
tmax <- fast(madTmax)

# Takes longer to run compared to SpatRaster version for small rasters, so
# just calculate select BIOCLIMs:
bc <- bioclims(ppt, tmin, tmax, bios = c(1, 5, 6, 12, 15), verbose = TRUE)
bc
plot(bc)

}
