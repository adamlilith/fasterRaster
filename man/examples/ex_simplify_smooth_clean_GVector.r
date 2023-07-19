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
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# example data
madCoast0 <- fastData('madCoast0')
madRivers <- fastData('madRivers')

# start GRASS session for examples only
faster(x = madRivers, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

### simplify geometry (remove nodes)
####################################

rivers <- fast(madRivers)

vr <- simplifyGeom(rivers, tolerance = 2000)
dp <- simplifyGeom(rivers, tolerance = 2000, method = 'dp')
dpr <- simplifyGeom(rivers, tolerance = 2000, method = 'dpr', prop=0.5)
rw <- simplifyGeom(rivers, tolerance = 2000, method = 'rw')

plot(st_geometry(madRivers), col='gray', lwd=3)
plot(vr, col='blue', add=TRUE)
plot(dp, col='red', add=TRUE)
plot(dpr, col='chartreuse', add=TRUE)
plot(rw, col='orange', add=TRUE)

legend('topright',
   legend = c(
	  'Original',
      'Vertex reduction',
      'Douglas-Peucker',
      'Douglas-Peucker reduction',
      'Reumann-Witkam'
	),
	col = c('gray', 'blue', 'red', 'chartreuse', 'orange'),
	lwd = c(3, 1, 1, 1, 1)
)

### smooth geometry
###################

hermite <- smoothGeom(rivers, dist = 2000, angle = 3)
chaiken <- smoothGeom(rivers, method = 'Chaiken', dist = 2000)

plot(st_geometry(madRivers), col='gray', lwd=2)
plot(hermite, col='blue', add=TRUE)
plot(chaiken, col='red', add=TRUE)

legend('topright',
   legend = c(
	  'Original',
      'Hermite',
      'Chaiken'
	),
	col = c('gray', 'blue', 'red'),
	lwd = c(2, 1, 1, 1, 1)
)

### clean geometry
##################

noDangs <- cleanGeom(rivers, method = 'removeDangles', tolerance = 2000)

plot(st_geometry(madRivers), col='blue')
plot(noDangs, col='red', add=TRUE)

legend('topright',
   legend = c(
	  'Original',
      'No dangles'
	),
	col = c('blue', 'red')
)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
removeSession('examples')

}
