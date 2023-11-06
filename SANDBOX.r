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

grassDir <- "C:/Program Files/GRASS GIS 8.3"

madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")
madCoast0 <- fastData("madCoast0")
madCoast4 <- fastData("madCoast4")

session <- faster(x = madCoast4, grassDir = grassDir, workDir = tempdir(), location = 'examples', overwrite = TRUE, warn = FALSE)

mcFile <- "C:/!Scratch/madCoast4.gpkg"
# madCoast4 <- vect(madCoast4)

execGRASS("v.in.ogr", input = mcFile, output = "coast")
execGRASS("v.category", input = "coast", option = "print")

# save cat table
newcats <- data.frame(newcat = c(rep(1, 9), 2:10))

tf <- tempfile(fileext = ".csv")
tft <- paste0(tf, "t")
utils::write.csv(newcats, tf, row.names = FALSE)
keyTableType <- '"Integer"'
write(keyTableType, tft)

execGRASS("db.in.ogr", input = tf, output = "newcat_table", flags = "overwrite")
execGRASS("v.db.connect", map = "coast", flags = "d")
execGRASS("v.info", map = "coast", flags = "e")

execGRASS("v.db.connect", map = "coast", table = "newcat_table", key = "newcat", flags = "o")
execGRASS("v.info", map = "coast", flags = "e")

# execGRASS("v.edit", input = "coast", output = "nocats", option = "delcat")
execGRASS("v.reclass", input = "coast", output = "reclassed", layer = "newcat_table", column = "newcat", type = "centroid", flags = "overwrite")
execGRASS("v.category", input = "reclassed", option = "print")

execGRASS("v.info", map = "reclassed", flags = "e")

