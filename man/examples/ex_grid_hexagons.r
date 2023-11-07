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
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madCoast0 <- fastData("madCoast0")

# Start GRASS session for examples only:
faster(x = madCoast0, grassDir = grassDir,
workDir = tempdir(), location = "examples") # line only needed for examples

# Convert sf to a GVector:
coast <- fast(madCoast0)

### grid
########

# grid specified by number of cells in x-dimension
g1 <- grid(coast, nx = 10)
plot(coast, col = "cornflowerblue")
plot(g1, add = TRUE)

# grid specified by number of cells in x- and y-dimension
g2 <- grid(coast, nx = 10, ny = 5)
plot(coast, col = "cornflowerblue")
plot(g2, add = TRUE)

# grid specified by size of cells in both dimensions
g3 <- grid(coast, nx = 1250, ny = 2000, use = "size")
plot(coast, col = "cornflowerblue")
plot(g3, add = TRUE)

### hexagons
############

hexes <- hexagons(coast, 20)
plot(coast)
plot(hexes, add = TRUE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
restoreSession(opts.)
removeSession("examples")

}
