if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Create a raster with values drawn from a uniform distribution:
unif <- runifRast(elev)
plot(unif)

unifs <- runifRast(elev, 2, -1, 1)
plot(unifs)

# Create a raster with values drawn from a normal distribution:
norms <- rnormRast(elev, n = 2, mu = c(5, 10), sigma = c(2, 1))
plot(norms)

ns <- rast(norms)
hist(ns[[1]], breaks = 100)

# Create a raster with random, seemingly normally-distributed values:
rand <- spDepRast(elev)
plot(rand)

# Values appear normal on first inspection:
r <- rast(rand)
hist(r)

# ... but actually are patterned:
hist(r, breaks = 100)

# Create a raster with random, normally-distributed values with
# spatial dependence. This can take a few minutes.
spatDep <- spDepRast(elev, dist = 3000)
plot(spatDep)

# Create a fractal raster:
fractal <- fractalRast(elev, n = 2, dimension = c(2.1, 2.8))
plot(fractal)

}
