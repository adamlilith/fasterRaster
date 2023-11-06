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

session <- faster(x = madCoast4, grassDir = grassDir)


mdFile <- "C:/Ecology/Drive/R/fasterRaster/data/madCoast4.rda"
load(mdFile)
mad <- vect(madCoast4)
table <- as.data.frame(mad)
mad[, 1:ncol(mad)] <- NULL
# mad$cat <- 1:2
mad$frCAT <- 1:length(mad)


# mad$frKEY <- rstring(dim(mad)[1L])
# frKEY <- mad$frKEY

madFile <- "C:/!Scratch/madCoast4.gpkg"
writeVector(mad, madFile, overwrite = TRUE)

#### re-assigning cats!!!
execGRASS(
	cmd = "v.in.ogr",
	input = madFile,
	output = "md",
	flags = c("overwrite")
)

# print cats
oldcats <- rgrass::execGRASS("v.category", input = "md", option = "print", intern = TRUE)
oldcats

newcats <- data.frame(oldcat = oldcats, newcat = seq_along(oldcats))
tf <- tempfile(fileext = ".csv")
tft <- paste0(tf, "t")
utils::write.csv(newcats, tf, row.names = FALSE)
tableType <- '"Integer"'
write(tableType, tft)

rgrass::execGRASS("db.in.ogr", input = tf, output = "newcats", flags = "overwrite")

rgrass::execGRASS("v.db.join", map = "md", column = "cat", other_table = "newcats", other_column = "oldcat")

.vAsDataTable("md")

rgrass::execGRASS("v.reclass", input = sources(rivers), output = "mdnew", column = "newcat")

.vAsDataTable("mdnew")

