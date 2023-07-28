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
library(terra)

# example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Send spatial objects to GRASS:
elev <- fast(madElev)
rivers <- fast(madRivers)

# Extend raster by number of rows/columns:
extended1 <- extend(elev, 10, fill = 900)
extended2 <- extend(elev, c(10, 20), fill = 900)
extended3 <- extend(elev, c(10, 80, 0, 100), fill = 900)
dim(elev)
dim(extended1)
dim(extended2)
dim(extended3)

plot(extended3)

# Note that when exporting a raster (including plotting it), NA rows and
# columns are removed.
extended4 <- extend(elev, 100, fill=1) # default fill is NA
extended4terra <- rast(extended4)

dim(extended4)
dim(extended4terra)

plot(extended4)

# Extend the raster by another object with a wider extent. We will crop the
# raster so that it is smaller than the object we use to extend it,
# then extend it.
elevCrop <- crop(elev, rivers)
uncrop <- extend(elevCrop, elev, fill = 900)
plot(uncrop)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
