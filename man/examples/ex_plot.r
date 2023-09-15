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

# Example data
madRivers <- fastData("madRivers")
madChelsa <- fastData("madChelsa")

# Start GRASS session for examples only:
faster(x = madChelsa, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert SpatRaster to GRaster and SpatVector to GVector
chelsa <- fast(madChelsa)
rivers <- fast(madRivers)

# Plot:
plot(chelsa[[1L]])
plot(rivers, add = TRUE)

# Histogram:
hist(chelsa)

# Plot in RGB... This isn't a multispectral raster, but we'll use the layers
# as if it was.
chelsaRescale <- chelsa
maxs <- global(chelsaRescale, "max")
chelsaRescale[[1]] <- 255 * chelsaRescale[[1]] / maxs[1, 1]
chelsaRescale[[2]] <- 255 * chelsaRescale[[2]] / maxs[2, 1]
chelsaRescale[[3]] <- 255 * chelsaRescale[[3]] / maxs[3, 1]

plotRGB(chelsa, r = 1, g = 2, b = 3, stretch = "lin")

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
