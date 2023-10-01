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

# # remove atts table
# .vRemoveTable(y)

# # see atts table
# .vAsDataTable(y) # error bc removed

# # print categories
# .vCats(y)

# # attach new table with random column
# .vAttachTable(y)

# tb <- data.frame(yes = letters[ngeom(y, "GRASS")])
# .vAttachTable(y, table = tb) # already table linked

# # see atts table
# .vAsDataTable(y)

# .vAddColumn(y, name = "MINE", type = "int", nchar = 100)

# # see atts table
# .vAsDataTable(y)

# db columns
.vNames(y)

# # remove column
# .vDropColumn(y, 2)

# # see atts table
# .vAsDataTable(y)

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

.vMakeKey(y)
.vKeys(y)
.vAsDataTable(y)

# sql <- matrix(NA_character_, nrow = 2, ncol = 1)
# sql[1, 1] <- "cat 1"
# sql[2, 1] <- "WHERE key = 'wfwfO7A5cjTS'"

keys <- .vKeys(y)
nGeoms <- ngeom(y, "GRASS")
sql <- matrix(NA_character_, nrow = 2 * nGeoms, ncol = 1)
for (i in seq_len(nGeoms)) {
	sql[2 * i - 1L, 1L] <- paste0("cat ", 1)
	sql[2 * i, 1L] <- paste0("WHERE key = '", keys[i], "'")
}

tf <- tempfile(fileext = ".sql")
write(sql, tf)

src <- .makeSourceName("recat", "vector")
args <- list(
	cmd = "v.reclass",
	input = sources(y),
	output = src,
	rules = tf,
	flags = c("quiet", "overwrite")
)
do.call(rgrass::execGRASS, args = args)
out <- .makeGVector(src)

# dt <- data.frame(key = "character")
# .vAttachTable(out, dt)
# .vMakeKey(out)

.vKeys(out)
.vCats(out)
.vAsDataTable(out)




