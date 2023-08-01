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

# elevation raster, rivers vector, locations of Dypsis plants
madElev <- fastData("madElev")

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert SpatRaster to a GRaster:
elev <- fast(madElev)

# Trim NA rows/columns:
trimmedElev <- trim(elev)
dim(elev)
dim(trimmedElev)

# Trim a "stack" of rasters. We will artificially add NA rows and columns to
# one raster to demonstrate how the trim() function removes only rows/columns
# that are NA for *all* rasters.
elevNAs <- elev

fun <- " = if(col() > ncols() - 200, null(), madElev)"
elevNAs <- app(elevNAs, fun)

# Notice raster is "narrower" because we added NA columns
plot(elevNAs)

elevs <- c(elev, elevNAs)
trimmedElevs <- trim(elevs)
trimmedElevNAs <- trim(elevNAs)

dim(elevs)
dim(trimmedElevNAs)
dim(trimmedElevs)


# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession("examples")

}
