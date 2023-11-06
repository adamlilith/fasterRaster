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


mdFile <- "C:/!Scratch/madCoast4.gpkg"
mad <- vect(mdFile)

table <- as.data.frame(mad)
mad[ , 1:ncol(mad)] <- NULL

# mad$frKEY <- rstring(dim(mad)[1L])
# frKEY <- mad$frKEY

writeVector(mad, mdFile, overwrite = TRUE)

# import into GRASS
execGRASS(
	cmd = "v.in.ogr",
	input = mdFile,
	output = "md",
	flags = c("overwrite")
)

# print cats
oldcats <- rgrass::execGRASS("v.category", input = "md", option = "print", intern = TRUE)
oldcats

#####
# execGRASS("v.db.droptable", map = "md", flags = "f")

newtable <- data.frame(cat = rep(1L, length(oldcats)), newcat = rep(3, length(oldcats)), oldcat = oldcats, key = rstring(length(oldcats)))
tf <- tempfile(fileext = ".csv")
tft <- paste0(tf, "t")
utils::write.csv(newtable, tf, row.names = FALSE)
tableType <- '"Integer","Integer","Integer","String"'
write(tableType, tft)

execGRASS("db.in.ogr", input = tf, output = "newtable", flags = "overwrite")

rgrass::execGRASS("v.category", input = "md", option = "print")

# execGRASS("v.db.connect", map = "md", table = "newtable", layer = "2", flags = "o")

rgrass::execGRASS("v.category", input = "md", option = "print", layer = "1")
# rgrass::execGRASS("v.category", input = "md", option = "print", layer = "newtable")

execGRASS("v.db.join", map = "md", column = "cat", other_table = "newtable", other_column = "oldcat")

.vAsDataTable("md")


rgrass::execGRASS("v.reclass", input = "md", output = "mdx", column = "newcat", type = "centroid", flags = "overwrite")
rgrass::execGRASS("v.category", input = "md", option = "print")
.vAsDataTable("md")




rgrass::execGRASS("v.category", input = "md", output = "mdx", option = "del")

rgrass::execGRASS("v.category", input = "mdx", option = "print")

rgrass::execGRASS("v.category", input = "mdx", output = "mdplus", option = "add", cat = 3, flags = "overwrite")

rgrass::execGRASS("v.edit", map = "md", tool = "catdel", type = "centroid", cats = "1")
rgrass::execGRASS("v.category", input = "md", option = "print")


#### works for aggregating!!!
newcats <- data.frame(oldcat = oldcats, newcat = rep(1, length(oldcats)))
tf <- tempfile(fileext = ".csv")
tft <- paste0(tf, "t")
utils::write.csv(newcats, tf, row.names = FALSE)
tableType <- '"Integer"'
write(tableType, tft)

rgrass::execGRASS("db.in.ogr", input = tf, output = "newcats", flags = "overwrite")

rgrass::execGRASS("v.db.join", map = sources(rivers), column = "cat", other_table = "newcats", other_column = "oldcat")

.vAsDataTable(rivers)

rgrass::execGRASS("v.reclass", input = sources(rivers), output = "mdnew", column = "newcat")

.vAsDataTable("mdnew")


#### re-assigning cats!!!
# import into GRASS
execGRASS(
    cmd = "v.in.ogr",
    input = mdFile,
    output = "md",
    flags = c("overwrite")
)

newcats <- data.frame(oldcat = oldcats, newcat = rep(1, length(oldcats)))
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

