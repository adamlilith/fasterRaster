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

# Example data: Land cover raster
madCover <- fastData("madCover")

# Start GRASS session for examples only
faster(x = madCover, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert categorical SpatRaster to categorical GRaster
cover <- fast(madCover)

# The GRaster inherited the SpatRaster's categories:
cover # Note display of "min"/"max" categories and number of categories
is.factor(cover) # Is the raster categorical?
ncat(cover) # Number of categories
levels(cover) # Categories

# Assign new levels. Note we can use ranges of values.
value <- c("11:30", "40:110", "120:150", "160:180", "190", "200:220", "230")
label <- c("Cropland", "Forest", "Grass/shrubland", "Flooded", "Artifical",
     "Bare/Water/Ice", "NA")

cats <- data.frame(value = value, label = label)
levels(cover) <- cats



# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
