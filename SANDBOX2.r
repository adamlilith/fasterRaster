# SANDBOX

rm(list = ls())
drive <- "C:/"
# drive <- "E:/"

.libPaths(paste0(drive, "/ecology/Drive/R/libraries"))
setwd(paste0(drive, "/ecology/Drive/R/fasterRaster"))

# devtools::document()
devtools::load_all()

library(terra)
library(sf)
library(rgrass)
library(data.table)

grassDir <- "C:/Program Files/GRASS GIS 8.3"

madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")
madCoast4 <- fastData("madCoast4")

session <- faster(x = madCoast0, grassDir = grassDir)

mc <- fast(madCoast4)
elev <- fast(madElev)

info <- execGRASS(
	cmd = "v.rast.stats",
	raster = sources(elev),
	map = sources(mc),
	column_prefix = "works",
	method = "average"
	# flags = c("p"),
)


