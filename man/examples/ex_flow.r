if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate flow accumulation and watershed basins rasters
water <- flow(elev, return = c("accumulation", "basins", "direction"))
water
plot(water)

}
