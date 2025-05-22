if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

### Create a raster with values drawn from a uniform distribution:
unif <- rUnifRast(elev)
plot(unif)

### Create a raster with values drawn from a normal distribution:
norms <- rNormRast(elev, n = 2, mu = c(5, 10), sigma = c(2, 1))
plot(norms)
hist(norms, bins = 100)

# Create a raster with random, seemingly normally-distributed values:
rand <- rSpatialDepRast(elev, dist = 1000)
plot(rand)

# Values appear normal on first inspection:
hist(rand)

# ... but actually are patterned:
hist(rand, bins = 100)

# Create a fractal raster:
fractal <- fractalRast(elev, n = 2, dimension = c(2.1, 2.8))
plot(fractal)
hist(fractal)

### Random walker rasters

# One random walker
walk <- rRandWalkRast(elev)
plot(walk)

# Random walker with self-avoidance:
walkAvoid <- rRandWalkRast(elev, steps = 1000, avoid = TRUE, seed = 1)
plot(walkAvoid)

# 10 random walkers:
walk10 <- rRandWalkRast(elev, n = 10)
plot(walk10)

# 10 random walkers starting in same place:
walkSame10 <- rRandWalkRast(elev, n = 10, sameStart = TRUE)
plot(walkSame10)


}
