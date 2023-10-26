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




#### STOP HERE FOR AGGREGATING^^^


newcats <- as.integer(oldcats) + 2

execGRASS("v.category", input = "md", output = "mdnew", option = "change", old = oldcats, new = newcats)

# # delete categories
# rgrass::execGRASS("v.category", input = "md", output = "md2", option = "del", cat = -1)
# rgrass::execGRASS("v.category", input = "md2", option = "print")

cats <- paste(rep(2, length(oldcats)), collapse = ",")
# cats <- rep(1, 11)
rgrass::execGRASS("v.category", input = "md", option = "report")
execGRASS("v.edit", map = "md", tool = "catadd", cat = cats, type = "centroid", layer = "2")
rgrass::execGRASS("v.category", input = "md", option = "print", layer = "2")


rules <- paste(
    "cat 1\n",
    paste0("where frKEY = ", frKEY[1], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[2], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[3], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[4], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[5], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[6], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[7], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[8], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[9], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[10], "\n"),
	"cat 2\n",
	paste0("where frKEY = ", frKEY[11], "\n")
)





# remove all cats
rgrass::execGRASS("v.category", input = "md", output = "mdnocat", option = "del", cat = -1)

# print cats
rgrass::execGRASS("v.category", input = "mdnocat", option = "print")

# remake cats
rgrass::execGRASS("v.category", input = "mdnocat", output = "mdnewcat", option = "add", step = 1)

# print cats
rgrass::execGRASS("v.category", input = "mdnewcat", option = "print")

###
atts <- .vAsDataTable("md")
frKEY <- atts$frKEY

rules <- paste(
    "cat 1\n",
    "where frKEY = 'yFqTo5KNnLBB','BTxBCwV7izAl','uRPZfPwzUzgb'\n"
    # "cat 2\n",
    # "where frKEY != 'yFqTo5KNnLBB','BTxBCwV7izAl','uRPZfPwzUzgb'\n"
)

write(rules, "C:/!Scratch/rules.txt")

execGRASS("v.reclass", input = "md", output = "reclass", rules = "C:/!Scratch/rules.txt", flags = "overwrite")

.vAsDataTable("reclass")

att <- data.frame(dummy = 1L)
.vAttachTable("reclass", att)




















###
execGRASS("v.db.addcolumn", map = "mdnewcat", columns = "cat2 INT")

execGRASS("v.to.db", map = "mdnewcat", type = "point", option = "cat", columns = "cat2", flags = "overwrite")








### assign new cats
# save cat table
cats <- c(rep(1, 7), 8:42)
cats <- data.frame(recat = cats)
tf <- tempfile(fileext = ".csv")
tft <- paste0(tf, "t")
utils::write.csv(cats, tf, row.names = FALSE)
tableType <- '"Integer"'
write(tableType, tft)

# import into GRASS
attsSrc <- .makeSourceName("db_in_ogr", "table")
args <- list(
	cmd = "db.in.ogr",
	input = tf,
	output = attsSrc,
	key = "recat",
	flags = c("quiet", "overwrite")
)
do.call(rgrass::execGRASS, args = args)



args <- list(
	cmd = "v.db.connect",
	map = "mdnewcat",
	table = attsSrc,
	key = "recat",
	layer = "2",
	flags = c("quiet", "overwrite", "o")
)
do.call(rgrass::execGRASS, args = args)

rgrass::execGRASS("v.category", input = "mdnewcat", option = "print", layer = "2")

rgrass::execGRASS("v.category", input = "mdnewcat", option = "print")














args <- list(
	cmd = "v.db.addtable",
	map = "mdnewcat",
	columns = columns,
	flags = "quiet",
	intern = TRUE
)

info <- do.call(rgrass::execGRASS, args = args)










# db?
tf <- tempfile(fileext = ".csv")
rgrass::execGRASS(
	"v.db.select",
	map = "md",
	intern = TRUE,
	separator = "comma",
	null_value = "NA",
	file = tf,
	flags = "overwrite"
)
out <- data.table::fread(tf)
out

.vAddColumn("md", "newCat", "character")
atts <- .vAsDataTable("md")
frKEY <- atts$frKEY

for (i in 1:42) {

	v <- ifelse(i %% 2 == 0, 1, 2)

	rgrass::execGRASS("v.db.update", map = "md", column = "newCat", value = as.character(v), where = paste0("frKEY = '", frKEY[i], "'"))

}

execGRASS("v.reclass", input = "md", output = "mdNew", column = "newCat", flags = "overwrite")

.vAsDataTable("mdNew")

execGRASS('v.info', map = "mdNew")


### select cat 2
execGRASS(
	cmd = "v.extract",
	input = "mdNew",
	cats = "2",
	output = "mdNew2",
	new = -1, # NB not -1???
	# new = 1, # NB not -1???
	flags = c("overwrite")
)




# has db?
tf <- tempfile(fileext = ".csv")
rgrass::execGRASS(
    "v.db.select",
    map = "mdNew2",
    intern = TRUE,
    separator = "comma",
    null_value = "NA",
    file = tf,
    flags = "overwrite"
)
out <- data.table::fread(tf)
out

execGRASS("v.info", map = "mdNew2", flags = "t")












### reclass

atts <- .vAsDataTable("md")
frKEY <- atts$frKEY

rules <- paste(
"cat 1\n",
"where frKEY = 'EKsBRahI3VIO'\n",
"cat 1\n",
"where frKEY = 'uswgp9kngSjc'\n",
"cat 2\n",
"where frKEY = 'bktHf8A4uJVE'\n"
)

write(rules, "C:/!Scratch/rules.txt")

execGRASS("v.reclass", input = "md", output = "reclass", rules = 'C:/!Scratch/rules.txt', flags = "overwrite")

.vAsDataTable("reclass")

att <- data.frame(dummy = 1L)
.vAttachTable("reclass", att)


