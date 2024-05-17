if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate flow accumulation and watershed basins
water <- flow(elev, return = c("accum", "basins"))
water

elevWater <- c(elev, water)
plot(elevWater)

}
