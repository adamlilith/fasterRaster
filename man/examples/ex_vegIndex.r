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

# Setup
library(terra)

# Elevation raster, rivers vector
madLANDSAT <- fastData("madLANDSAT")

# Start GRASS session for examples only:
faster(x = madLANDSAT, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert a SpatRaster to a GRaster:
landsat <- fast(madLANDSAT)

# Normalized Difference Vegetation Index and Enhanced Vegetation Index:
indices <- c("ndvi", "evi")
vi <- vegIndex(landsat, index = indices, r = 1, b = 3, nir = 4, bits = 8)

plot(vi)

# All indices using R and NIR:
rnir <- vegIndex(landsat, index = c("rnir"), r = 1, nir = 4, bits = 8)

# Note: Some values are highly skewed, likely due to cloud cover and other
# anomalies that should be corrected.
plot(rnir)

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
