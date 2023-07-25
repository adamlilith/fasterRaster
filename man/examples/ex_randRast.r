\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.3.app/Contents/Resources" # Mac
grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
grassDir <- "/usr/local/grass" # Linux

# setup
library(sf)
library(terra)

# elevation raster
madElev <- fastData("madElev")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

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

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
