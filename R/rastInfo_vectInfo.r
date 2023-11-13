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
    			appendLists(out, this)
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
				niceinfo <- rgrass::execGRASS(
					"r.info",
					flags = c(.quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				extentinfo <- rgrass::execGRASS(
					"r.info",
					flags = c("g", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				rangeinfo <- rgrass::execGRASS(
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
				niceinfo <- rgrass::execGRASS(
					"r3.info",
					flags = c(.quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				extentinfo <- rgrass::execGRASS(
					"r3.info",
					flags = c("g", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
		
			suppressMessages(
				rangeinfo <- rgrass::execGRASS(
					"r3.info",
					flags = c("r", .quiet()),
					map = src,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			topology <- "3D"
		
			ztop <- extentinfo[grepl(extentinfo, pattern = "top=")]
   			zbottom <- extentinfo[grepl(extentinfo, pattern = "bottom=")]
			
			ztop <- sub(ztop, pattern="top=", replacement="")
			zbottom <- sub(zbottom, pattern="bottom=", replacement="")
			
			ztop <- as.numeric(ztop)
			zbottom <- as.numeric(zbottom)
			
		}

		# projection
		index <- grepl(niceinfo, pattern = " \\|        Projection: ")
		projection <- niceinfo[index]
		projection <- sub(projection, pattern = " \\|        Projection: ", replacement = "")
		projection <- substr(projection, 1L, nchar(projection) - 1L)
		projection <- trimws(projection)
		
		# extent
		west <- extentinfo[grepl(extentinfo, pattern = "west=")]
		east <- extentinfo[grepl(extentinfo, pattern = "east=")]
		south <- extentinfo[grepl(extentinfo, pattern = "south=")]
		north <- extentinfo[grepl(extentinfo, pattern = "north=")]

		west <- sub(west, pattern="west=", replacement="")
		east <- sub(east, pattern="east=", replacement="")
		south <- sub(south, pattern="south=", replacement="")
		north <- sub(north, pattern="north=", replacement="")
		
		west <- as.numeric(west)
		east <- as.numeric(east)
		south <- as.numeric(south)
		north <- as.numeric(north)

		# dimensions
		rows <- extentinfo[grepl(extentinfo, pattern="rows=")]
		cols <- extentinfo[grepl(extentinfo, pattern="cols=")]
		depths <- extentinfo[grepl(extentinfo, pattern="depths=")]

		rows <- sub(rows, pattern="rows=", replacement="")
		cols <- sub(cols, pattern="cols=", replacement="")
		depths <- sub(depths, pattern="depths=", replacement="")
		
		rows <- as.integer(rows)
		cols <- as.integer(cols)
		depths <- as.integer(depths)
		if (length(depths) == 0L) depths <- NA_integer_
		
		# resolution
		ewres <- extentinfo[grepl(extentinfo, pattern="ewres=")]
		nsres <- extentinfo[grepl(extentinfo, pattern="nsres=")]
		tbres <- extentinfo[grepl(extentinfo, pattern="tbres=")]

		ewres <- sub(ewres, pattern="ewres=", replacement="")
		nsres <- sub(nsres, pattern="nsres=", replacement="")
		tbres <- sub(tbres, pattern="tbres=", replacement="")
		
		ewres <- as.numeric(ewres)
		nsres <- as.numeric(nsres)
		tbres <- as.numeric(tbres)
		if (length(tbres) == 0L) tbres <- NA_real_

		# data type
		grassDataType <- extentinfo[grepl(extentinfo, pattern="datatype=")]
		grassDataType <- sub(grassDataType, pattern="datatype=", replacement="")
		grassDataType <- gsub(grassDataType, pattern="\"", replacement="")

		# ### Any categories? Note that "r.info" incorrectly reports number of categories, so we will use r.info just to determine if there are any categories.
		# anyCats <- niceinfo[grepl(niceinfo, pattern = "Number of Categories:")]
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
		minVal <- rangeinfo[grepl(rangeinfo, pattern="min=")]
		maxVal <- rangeinfo[grepl(rangeinfo, pattern="max=")]
		
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
	
	out

}

.vectInfo <- function(x) {

	src <- if (inherits(x, "GVector")) {
		sources(x)
	} else {
		x
	}

	### extent/topology
	suppressMessages(
		extentinfo <- rgrass::execGRASS(
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
		geominfo <- rgrass::execGRASS(
			"v.info",
			flags = c("t", .quiet()),
			map = src,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)

	# extent
	west <- extentinfo[grepl(extentinfo, pattern = "west=")]
	east <- extentinfo[grepl(extentinfo, pattern = "east=")]
	south <- extentinfo[grepl(extentinfo, pattern = "south=")]
	north <- extentinfo[grepl(extentinfo, pattern = "north=")]

	west <- sub(west, pattern = "west=", replacement = "")
	east <- sub(east, pattern = "east=", replacement = "")
	south <- sub(south, pattern = "south=", replacement = "")
	north <- sub(north, pattern = "north=", replacement = "")

	west <- as.numeric(west)
	east <- as.numeric(east)
	south <- as.numeric(south)
	north <- as.numeric(north)

	# topology (2/3D)
	topology <- geominfo[grepl(geominfo, pattern = "map3d=")]
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
		zbottom <- extentinfo[grepl(extentinfo, pattern = "bottom=")]
		ztop <- extentinfo[grepl(extentinfo, pattern = "top=")]

		zbottom <- sub(zbottom, pattern = "bottom=", replacement = "")
		ztop <- sub(ztop, pattern = "top=", replacement = "")

		zbottom <- as.numeric(zbottom)
		ztop <- as.numeric(ztop)
	}
 
	# geometry (points/lines/polygons)
	points <- geominfo[grepl(geominfo, pattern="points=")]
	lines <- geominfo[grepl(geominfo, pattern="lines=")]
	boundaries <- geominfo[grepl(geominfo, pattern="boundaries=")]
	centroids <- geominfo[grepl(geominfo, pattern="centroids=")]
	areas <- geominfo[grepl(geominfo, pattern="areas=")]

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
		nGeometries <- points
	} else if (points == 0 & lines > 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		geometry <- "lines"
		nGeometries <- lines
	} else if (points == 0 & lines == 0 & boundaries > 0 & centroids > 0 & areas > 0) {
		geometry <- "polygons"
		nGeometries <- centroids
	} else {
		geometry <- NA_character_
		nGeometries <- NA_integer_
	}
	
	nGeometries <- as.integer(nGeometries)

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
	
	out

}
