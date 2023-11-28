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

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

### GRasters
############

# Example data
madElev <- fastData("madElev") # elevation raster
madForest2000 <- fastData("madForest2000") # forest raster
madForest2014 <- fastData("madForest2000") # forest raster

# Convert SpatRasters to GRasters
elev <- fast(madElev)
forest2000 <- fast(madForest2000)
forest2014 <- fast(madForest2014)

### Re-assigning values of a GRaster

constant <- elev
constant[] <- 14
constant

names(constant) <- "constant"

### Adding and re-assigning layers of a GRaster

rasts <- c(elev, constant, forest2000)

# Combine with another layer:
add(rasts) <- forest2014

# Combine two layers:
elevs <- c(2 * elev, elev / 10)
names(elevs) <- c("elev_x2", "elev_div10")
elevs

# Combine with previous layers:
rasts <- c(rasts, elevs)
rasts

### Subsetting GRaster layers

# Subset:
rasts[[2:3]]
rasts[[c("madForest2000", "elev_x2")]]
rasts$madForest2000

# Get every other layer:
rasts[[c(FALSE, TRUE)]]

### Replacing layers of a GRaster

# Replace a layer
logElev <- log(elev)
names(logElev) <- "logElev"
rasts$madForest2000 <- logElev
rasts

# Replace a layer:
rasts[[3]] <- forest2000
rasts

### GVectors
############

# example data
madDypsis <- fastData("madDypsis") # vector of points

# Convert SpatVector to GVector
dypsis <- fast(madDypsis)

### Retrieving GVector columns

dypsis$species # Returns the column

dypsis[[c("year", "species")]] # Returns a GRaster with these columns
dypsis[ , c("year", "species")] # Same as above

### Subsetting GVector geometries

# Subset first three geometries
dypsis[1:3]
dypsis[1:3, "species"]

# Get geometries by data table condition
dypsis[dypsis$species == "Dypsis betsimisarakae"]

### (Re)assigning GVector column values

# New column
dypsis$pi <- pi

# Re-assign values
dypsis$pi <- "pie"

# Re-assign specific values
dypsis$institutionCode[dypsis$institutionCode == "MO"] <-
   "Missouri Botanical Garden"

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
