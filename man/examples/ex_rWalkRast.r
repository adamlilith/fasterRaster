if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Random walker:
walk <- rRandWalk(elev)
plot(walk)

# Random walker with self-avoidance:
walkAvoid <- rRandWalk(elev, steps = 1000, avoid = TRUE, seed = 1)
plot(walkAvoid)

# 10 random walkers:
walk10 <- rRandWalk(elev, n = 10)
plot(walk10)

# 10 random walkers starting in same place:
walkSame10 <- rRandWalk(elev, n = 10, sameStart = TRUE)
plot(walkSame10)


}
