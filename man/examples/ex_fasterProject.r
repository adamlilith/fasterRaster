\dontrun{

library(terra)

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

library(sf)
library(terra)

### project a raster using fasterProjectRast()
madChelsa <- fasterData('madChelsa') # unprojected
madElev <- fasterData('madElev') # projected

# coordinate reference systems are different!
crs(madChelsa)
crs(madElev)

madChelsaProj <- fasterProjectRast(rast=madChelsa,
template=madElev, grassDir=grassDir)

crs(madChelsaProj)

### project a vector using fasterProjectVect()
# This is a contrived example, as we will project a vector to a different
# coordinate reference system outside 
madCoast0 <- fasterData('madCoast0')
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84'
madCoast0wgs84 <- st_transform(madCoast0, wgs84)

madCoast0proj <- fasterProjectVect(vect=madCoast0wgs84,
template=madElev, grassDir=grassDir)

# Note that we already created a GRASS session with the call to
# fasterProjectRast(), so we could have used that session's CRS and
# thereby # save some time importing the madElev raster to serve
# as a template:

madCoast0proj <- fasterProjectVect(vect=madCoast0wgs84,
template=NULL, grassDir=grassDir)



}
