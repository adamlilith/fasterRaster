\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(sf)
library(terra)

# elevation raster, rivers vector
madElev <- fastData('madElev')
madRivers <- fastData('madRivers')
madCoast4 <- fastData('madCoast4')
madAnt <- madCoast4[madCoast4$NAME_4 == 'Antanambe', ]

# start GRASS session for examples only
wd <- forwardSlash(tempdir())

faster(crs = madElev, grassDir = grassDir,
workDir = wd, location = 'examples') # line only needed for examples

# convert a SpatRaster to a GRaster, and sf to a GVector
elev <- fast(madElev)
rivers <- fast(madRivers)
man <- fast(madAnt)

### crop raster by vector
rastByVect <- crop(elev, man)

### crop raster by raster
# We'll cheat by making the raster smaller in R, then crop the
# fasterRaster by this.
madMan <- madCoast4[madCoast4$NAME_4 == 'Manompana', ]
templateRast <- crop(madElev, madMan)

template <- fast(templateRast)
rastByRast <- crop(elev, template)

### crop vector by vector
vectByVect <- crop(rivers, man)

### crop vector by raster
vectByRast <- crop(rivers, template)


# IMPORTANT #3: Revert back to original GRASS session if needed.
sessionRestore(opts.)
removeSession('examples')

}
