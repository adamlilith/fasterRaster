
### vector data
###############

library(sf)

# For vector data, we can use data(*) or fastData(*):
data(madCoast0) # same as next line
madCoast0 <- fastData("madCoast0") # same as previous
madCoast0

madCoast4 <- fastData("madCoast4")
madCoast4

madRivers <- fastData("madRivers")
madRivers

madDypsis <- fastData("madDypsis")
madDypsis

### raster data
###############

library(terra)

# For raster data, we can get the file directly or using fastData(*):
rastFile <- system.file("extdata/madElev.tif", package="fasterRaster")
madElev <- terra::rast(rastFile)

madElev <- fastData("madElev") # same as previous two lines
madElev
plot(madElev)

madForest2000 <- fastData("madForest2000")
madForest2000
plot(madForest2000)

madForest2014 <- fastData("madForest2014")
madForest2014
plot(madForest2014)

madChelsa <- fastData("madChelsa")
madChelsa
plot(madChelsa)

madLANDSAT <- fastData("madLANDSAT")
madLANDSAT
plotRGB(madLANDSAT, 4, 1, 2, stretch = "lin")

madCover <- fastData("madCover")
madCover
madCover <- droplevels(madCover)
levels(madCover) # levels in the raster
nlevels(madCover) # number of categories
catNames(madCover) # names of categories table

plot(madCover)
