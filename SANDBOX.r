# SANDBOX

rm(list = ls())
drive <- "C:/"
# drive <- "E:/"

.libPaths(paste0(drive, "/ecology/Drive/R/libraries"))
setwd(paste0(drive, "/ecology/Drive/R/fasterRaster"))

# devtools::document()
devtools::load_all()

grassDir <- "C:/Program Files/GRASS GIS 8.3"

madElev <- fastData("madElev")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")
madCoast0 <- fastData("madCoast0")
madCoast4 <- fastData("madCoast4")

session <- faster(grassDir = grassDir, x = madRivers, workDir = tempdir(), location = 'examples', overwrite = TRUE, warn = FALSE)

y <- fast(madCoast4)


# print categories
.vCats(y)

# see atts table
.vAsDataTable(y)

# remove atts table
.vRemoveTable(y)

# # see atts table
# .vAsDataTable(y) # error bc removed

# print categories
.vCats(y)

# attach new table with random column
.vAttachTable(y)

# tb <- data.frame(yes = letters[ngeom(y, "GRASS")])
# .vAttachTable(y, table = tb) # already table linked

# # see atts table
# .vAsDataTable(y)

# .vAddColumn(y, name = "MINE", type = "int", nchar = 100)

# # see atts table
# .vAsDataTable(y)

# db columns
.vNames(y)

# remove column
.vDropColumn(y, 2)

# see atts table
.vAsDataTable(y)

# # add values to a column
# args <- list(
# 	cmd = "v.db.update",
# 	map = sources(y),
# 	column = "MINE",
# 	value = "12",
# 	where = "cat = 17"
# )

# do.call(rgrass::execGRASS, args = args)

# # see atts table
# .vAsDataTable(y)

y <- .vMakeKey(y)
.vAsDataTable(y)

sql <- paste0("UPDATE ", src, " SET cat = 7 WHERE key = 'yq8Bf7uYgTpC';")
rgrass::execGRASS("db.execute", sql = sql)

