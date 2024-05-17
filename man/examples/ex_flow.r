if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
elev <- fast(madElev)

# Calculate flow accumulation, basins, flow direction, flooded areas, and TCI
water <- flow(elev, return = "*")
water

elevWater <- c(elev, water)
plot(elevWater)

}
