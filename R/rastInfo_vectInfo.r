
#' Information on rasters and vectors in "GRASS"
#'
#' @param x A `GRaster`, `GVector`, or `sources`.
#'
#' @returns Extent, dimensions, resolution, bottom/top, etc.
#'
#' @noRd
.rastInfo <- function(x) {

	src <- if (!inherits(x, "character")) {
		sources(x)
	} else {
		x
	}

	### more than one raster
	#######################
	if (length(src) > 1L) {
		
		for (i in seq_along(src)) {
		
			this <- .rastInfo(src[i])
			out <- if (i == 1L) {
				this
			} else {
    			omnibus::appendLists(out, this)
			}
		
		}
	
	} else {
	### a single source
	##################

		rasters <- .ls(c("rasters", "rasters3d"))
		type <- names(rasters[rasters == src])

		### 2D raster
		if (type == "raster") {

			suppressMessages(
				niceInfo <- rgrass::execGRASS(
					"r.info",
					flags = .quiet(),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				extentInfo <- rgrass::execGRASS(
					"r.info",
					flags = c("g", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				rangeInfo <- rgrass::execGRASS(
					"r.info",
					flags = c("r", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
			
			topology <- "2D"

			ztop <- NA_real_
			zbottom <- NA_real_
			
		### 3D raster
		} else if (type == "raster3d") {

			suppressMessages(
				niceInfo <- rgrass::execGRASS(
					"r3.info",
					flags = c(.quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				extentInfo <- rgrass::execGRASS(
					"r3.info",
					flags = c("g", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
		
			suppressMessages(
				rangeInfo <- rgrass::execGRASS(
					"r3.info",
					flags = c("r", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			topology <- "3D"
		
			ztop <- extentInfo[grepl(extentInfo, pattern = "top=")]
   			zbottom <- extentInfo[grepl(extentInfo, pattern = "bottom=")]
			
			ztop <- sub(ztop, pattern="top=", replacement="")
			zbottom <- sub(zbottom, pattern="bottom=", replacement="")
			
			ztop <- as.numeric(ztop)
			zbottom <- as.numeric(zbottom)
			
		}

		# projection
		index <- grepl(niceInfo, pattern = " \\|        Projection: ")
		projection <- niceInfo[index]
		projection <- sub(projection, pattern = " \\|        Projection: ", replacement = "")
		projection <- substr(projection, 1L, nchar(projection) - 1L)
		projection <- trimws(projection)
		
		# extent
		west <- extentInfo[grepl(extentInfo, pattern = "west=")]
		east <- extentInfo[grepl(extentInfo, pattern = "east=")]
		south <- extentInfo[grepl(extentInfo, pattern = "south=")]
		north <- extentInfo[grepl(extentInfo, pattern = "north=")]

		west <- sub(west, pattern="west=", replacement="")
		east <- sub(east, pattern="east=", replacement="")
		south <- sub(south, pattern="south=", replacement="")
		north <- sub(north, pattern="north=", replacement="")
		
		west <- as.numeric(west)
		east <- as.numeric(east)
		south <- as.numeric(south)
		north <- as.numeric(north)

		# dimensions
		rows <- extentInfo[grepl(extentInfo, pattern="rows=")]
		cols <- extentInfo[grepl(extentInfo, pattern="cols=")]
		depths <- extentInfo[grepl(extentInfo, pattern="depths=")]

		rows <- sub(rows, pattern="rows=", replacement="")
		cols <- sub(cols, pattern="cols=", replacement="")
		depths <- sub(depths, pattern="depths=", replacement="")
		
		rows <- as.integer(rows)
		cols <- as.integer(cols)
		depths <- as.integer(depths)
		if (length(depths) == 0L) depths <- NA_integer_
		
		# resolution
		ewres <- extentInfo[grepl(extentInfo, pattern="ewres=")]
		nsres <- extentInfo[grepl(extentInfo, pattern="nsres=")]
		tbres <- extentInfo[grepl(extentInfo, pattern="tbres=")]

		ewres <- sub(ewres, pattern="ewres=", replacement="")
		nsres <- sub(nsres, pattern="nsres=", replacement="")
		tbres <- sub(tbres, pattern="tbres=", replacement="")
		
		ewres <- as.numeric(ewres)
		nsres <- as.numeric(nsres)
		tbres <- as.numeric(tbres)
		if (length(tbres) == 0L) tbres <- NA_real_

		# data type
		grassDataType <- extentInfo[grepl(extentInfo, pattern="datatype=")]
		grassDataType <- sub(grassDataType, pattern="datatype=", replacement="")
		grassDataType <- gsub(grassDataType, pattern="\"", replacement="")

		# ### Any categories? Note that "r.info" incorrectly reports number of categories, so we will use r.info just to determine if there are any categories.
		# anyCats <- niceInfo[grepl(niceInfo, pattern = "Number of Categories:")]
		# start <- regexpr(anyCats, pattern = "Number of Categories: ")
    	# start <- start[1L] + nchar("Number of Categories: ")
		# anyCats <- substr(anyCats, 63, nchar(anyCats))
		# anyCats <- gsub(anyCats, pattern = "\\|", replacement = "")
		# anyCats <- as.integer(anyCats)

		# if (anyCats == 0L) {
		# 	numCategories <- 0L
		# } else {
		
		# 	catinfo <- rgrass::execGRASS(
		# 		"r.category",
		# 		map = src,
		# 		flags = .quiet(),
		# 		separator = "pipe",
		# 		intern = TRUE
		# 	)

		# 	numCategories <- length(catinfo)
		# 	numCategories <- as.integer(numCategories)
		
		# }

		# mininum/maximum value
		minVal <- rangeInfo[grepl(rangeInfo, pattern="min=")]
		maxVal <- rangeInfo[grepl(rangeInfo, pattern="max=")]
		
		minVal <- sub(minVal, pattern="min=", replacement="")
		maxVal <- sub(maxVal, pattern="max=", replacement="")
		
		minVal <- if (minVal == "NULL") { NA_real_ } else { as.numeric(minVal) }
		maxVal <- if (maxVal == "NULL") { NA_real_ } else { as.numeric(maxVal) }

		out <- list(
			sources = src,
			type = type,
			topology = topology,
			projection = projection,
			
			west = west,
			east = east,
			south = south,
			north = north,
			
			ztop = ztop,
			zbottom = zbottom,
		
			rows = rows,
			cols = cols,
			depths = depths,
		
			ewres = ewres,
			nsres = nsres,
			tbres = tbres,
			
			grassDataType = grassDataType,
			
			# numCategories = numCategories,
			minVal = minVal,
			maxVal = maxVal
			
		)
		
	} # if just one raster

	class(out) <- "rastInfo"
	out

}

#' @aliases print
#' @rdname show
#' @export
print.rastInfo <- function(x, ...) {

	cat("Source(s)     :", x$sources, "\n")
	cat("Type(s)       :", x$type, "\n")
	cat("Topology      :", x$topology, "\n")
	cat("Projection    :", x$projection, "\n")
	cat("Resolution    :", x$ewres, x$nsres, x$tbres, "(x, y, z)\n")
	cat("Dimensions    :", x$rows, x$cols, x$depths, "(rows, cols, depths)\n")
	cat("Extent (WESN) :", paste(x$west, x$east, x$south, x$north, collapse = ", "), "\n")
	cat("Z extent (BT) :", paste(x$zbottom, x$ztop, collapse = ", "), "\n")
	cat("Datatype      :", x$grassDataType, "\n")
	cat("Min/max       :", x$minVal, x$maxVal, "\n")

}

#' @noRd
show.rastInfo <- function(x) print.rastInfo(x)

#' @noRd
summary.rastInfo <- function(x) print.rastInfo(x)

.vectInfo <- function(x, integer = TRUE) {

	src <- if (inherits(x, "GVector")) {
		sources(x)
	} else {
		x
	}

	### extent/topology
	suppressMessages(
		extentInfo <- rgrass::execGRASS(
			"v.info",
			flags = c("g", .quiet()),
			map = src,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)
	
	### geometry
	suppressMessages(
		geomInfo <- rgrass::execGRASS(
			"v.info",
			map = src,
			flags = c("t", .quiet()),
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)

	# extent
	west <- extentInfo[grepl(extentInfo, pattern = "west=")]
	east <- extentInfo[grepl(extentInfo, pattern = "east=")]
	south <- extentInfo[grepl(extentInfo, pattern = "south=")]
	north <- extentInfo[grepl(extentInfo, pattern = "north=")]

	west <- sub(west, pattern = "west=", replacement = "")
	east <- sub(east, pattern = "east=", replacement = "")
	south <- sub(south, pattern = "south=", replacement = "")
	north <- sub(north, pattern = "north=", replacement = "")

	west <- as.numeric(west)
	east <- as.numeric(east)
	south <- as.numeric(south)
	north <- as.numeric(north)

	# topology (2/3D)
	topology <- geomInfo[grepl(geomInfo, pattern = "map3d=")]
	topology <- sub(topology, pattern = "map3d=", replacement = "")
	topology <- if (topology == "0") {
		"2D"
	} else if (topology == "1") {
		"3D"
	}

	# top/bottom
	if (topology == "2D") {
		ztop <- zbottom <- NA_real_
	} else {
		zbottom <- extentInfo[grepl(extentInfo, pattern = "bottom=")]
		ztop <- extentInfo[grepl(extentInfo, pattern = "top=")]

		zbottom <- sub(zbottom, pattern = "bottom=", replacement = "")
		ztop <- sub(ztop, pattern = "top=", replacement = "")

		zbottom <- as.numeric(zbottom)
		ztop <- as.numeric(ztop)
	}
 
	# geometry (points/lines/polygons)
	points <- geomInfo[grepl(geomInfo, pattern="points=")]
	lines <- geomInfo[grepl(geomInfo, pattern="lines=")]
	boundaries <- geomInfo[grepl(geomInfo, pattern="boundaries=")]
	centroids <- geomInfo[grepl(geomInfo, pattern="centroids=")]
	areas <- geomInfo[grepl(geomInfo, pattern="areas=")]

	points <- sub(points, pattern="points=", replacement="")
	lines <- sub(lines, pattern="lines=", replacement="")
	boundaries <- sub(boundaries, pattern="boundaries=", replacement="")
	centroids <- sub(centroids, pattern="centroids=", replacement="")
	areas <- sub(areas, pattern="areas=", replacement="")

	points <- as.integer(points)
	lines <- as.integer(lines)
	boundaries <- as.integer(boundaries)
	centroids <- as.integer(centroids)
	areas <- as.integer(areas)

	if (points > 0 & lines == 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		geometry <- "points"
		# nGeometries <- points
	} else if (points == 0 & lines > 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		geometry <- "lines"
		# nGeometries <- lines
	} else if (points == 0 & lines == 0 & boundaries > 0 & centroids > 0 & areas > 0) {
		geometry <- "polygons"
		# nGeometries <- centroids
	} else {
		geometry <- NA_character_
		# nGeometries <- NA_integer_
	}
	
	# nGeometries <- as.integer(nGeometries)
	cats <- .vCats(src, integer = integer)
	nGeometries <- length(unique(cats))
	catsValid <- !any(grepl(cats, pattern = "[/]")) & !anyNA(cats)

	# ### fields
	# suppressWarnings(
	# 	fieldinfo <- rgrass::execGRASS(
	# 		"v.info",
	# 		flags = c("c", .quiet()),
	# 		map = src,
	# 		intern = TRUE,
	# 		Sys_show.output.on.console = FALSE,
	# 		echoCmd = FALSE
	# 	)
	# )
	
	# fieldData <- rgrass::execGRASS(
	# 	"v.db.connect",
	# 	map = src,
	# 	flags = c("g", .quiet()),
	# 	intern = TRUE
	# )

	suppressMessages(
		attribMetaInfo <- rgrass::execGRASS(
			"v.info",
			flags = "e",
			map = src,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)

	# hasNoFields <- any(grepl(fieldData, pattern="is not connected to a database")) | any(grepl(fieldinfo, pattern="ERROR: Database connection not defined for layer"))
	
	# if (hasNoFields) {

	# 	nFields <- as.integer(0)
	# 	dbLayer <- NA_character_
	# 	fields <- NA_character_
	# 	classes <- NA_character_

	# # vector has a data frame
	# } else {

	# 	# fields and types
	# 	fieldinfo <- fieldinfo[!grepl(fieldinfo, pattern ="Displaying column types/names for database connection of layer")]
		
	# 	fieldinfo <- strsplit(fieldinfo, "\\|")
	# 	fields <- classes <- rep(NA, length(fieldinfo))
		
	# 	for (i in seq_along(fieldinfo)) {
		
	# 		type <- fieldinfo[[i]][1L]
	# 		type <- if (type == "CHARACTER") {
	# 			"character"
	# 		} else if (type == "INTEGER") {
	# 			"integer"
	# 		} else if (type == "DOUBLE PRECISION") {
	# 			"numeric"
	# 		} else {
	# 			NA_character_
	# 		}
		
	# 		classes[i] <- type
	# 		fields[i] <- fieldinfo[[i]][2L]
	# 	}

	# 	cat <- which(fields == "cat")
	# 	fields <- fields[-cat]
	# 	classes <- classes[-cat]

	# 	nFields <- length(fields)
	# 	nFields <- as.integer(nFields)

  	# 	dbLayer <- attribMetaInfo[grepl(attribMetaInfo, pattern = "attribute_layer_name")]
	# 	dbLayer <- if (length(dbLayer) == 0) {
	# 		NA_character_
	# 	} else {
	# 		gsub(dbLayer, pattern="attribute_layer_name=", replacement="")
	# 	}

	# }

  	projection <- attribMetaInfo[grepl(attribMetaInfo, pattern = "projection=")]
 	projection <- sub(projection, pattern = "projection=", replacement = "")
		
	out <- list(
		sources = src,
		type = "vector",
		projection = projection,
		topology = topology,
		geometry = geometry,
		
		cats = cats,
		catsValid = catsValid,
		nGeometries = nGeometries,
		
		west = west,
		east = east,
		south = south,
		north = north,
	
		zbottom = zbottom,
		ztop = ztop
		
		# nFields = nFields,
		# dbLayer = dbLayer,
		# fields = fields,
		# classes = classes
	)
	
	class(out) <- "vectInfo"
	out

}

#' @aliases print
#' @rdname show
#' @export
print.vectInfo <- function(x, ...) {

	cats <- x$cats
	if (length(cats) > 6L) {
		cats <- paste0(paste(head(x$cats), collapse = ", "), ", ...\n")
	} else {
		cats <- paste0(paste(head(x$cats), collapse = ", "), "\n")
	}

	cat("Source        :", x$sources, "\n")
	cat("Geometry      :", x$geometry, "\n")
	cat("Projection    :", x$projection, "\n")
	cat("Topology      :", x$topology, "\n")
	cat("Extent (WESN) :", paste(x$west, x$east, x$south, x$north, collapse = ", "), "\n")
	cat("Z extent (BT) :", paste(x$zbottom, x$ztop, collapse = ", "), "\n")
	cat("Geometries    :", x$nGeometries, "\n")
	cat("Cats          :", cats)
	cat("Cats valid    :", x$catsValid, "\n")

}

#' @export
summary.vectInfo <- function(x) print(x)

#' @export
show.vectInfo <- function(x) print(x)
