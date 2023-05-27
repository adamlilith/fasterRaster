#' Information on rasters and vectors in 'GRASS'
#'
#' @param x A `GRaster`, `GVector`, or `gnames`.
#'
#' @returns Extent, dimensions, resolution, bottom/top, etc.
#'
#' @noRd
.rastInfo <- function(x) {

	gn <- if (!inherits(x, 'character')) {
		gnames(x)
	} else {
		x
	}

	### more than one raster
	#######################
	if (length(gn) > 1L) {
		
		for (i in seq_along(gn)) {
		
			this <- .rastInfo(gn[i])
			out <- if (exists('out', inherits=FALSE)) {
				appendLists(out, this)
			} else {
				this
			}
		
		}
	
	} else {
	### a single gnames
	##################

		rasters <- .ls(c('rasters', 'rasters3d'))
		type <- names(rasters[rasters == gn])

		### 2D raster
		if (type == 'raster') {

			suppressMessages(
				info <- rgrass::execGRASS(
					'r.info',
					flags = c('g', 'quiet'),
					map = gn,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				range <- rgrass::execGRASS(
					'r.info',
					flags = c('r', 'quiet'),
					map = gn,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
			
			topology <- '2D'

			ztop <- NA_real_
			zbottom <- NA_real_
			
			nCats <- info[grepl(info, pattern='ncats=')]
			nCats <- sub(nCats, pattern='ncats=', replacement='')
			nCats <- as.integer(nCats)

		### 3D raster
		} else if (type == 'raster3d') {

			suppressMessages(
				info <- rgrass::execGRASS(
					'r3.info',
					flags = c('g', 'quiet'),
					map = gn,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
		
			suppressMessages(
				range <- rgrass::execGRASS(
					'r3.info',
					flags = c('r', 'quiet'),
					map = gn,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			topology <- '3D'
		
			ztop <- info[grepl(info, pattern='top=')]
			zbottom <- info[grepl(info, pattern='bottom=')]
			
			ztop <- sub(ztop, pattern='top=', replacement='')
			zbottom <- sub(zbottom, pattern='bottom=', replacement='')
			
			ztop <- as.numeric(ztop)
			zbottom <- as.numeric(zbottom)
			
			nCats <- 0L
			
		}
		
		# extent
		west <- info[grepl(info, pattern='west=')]
		east <- info[grepl(info, pattern='east=')]
		south <- info[grepl(info, pattern='south=')]
		north <- info[grepl(info, pattern='north=')]

		west <- sub(west, pattern='west=', replacement='')
		east <- sub(east, pattern='east=', replacement='')
		south <- sub(south, pattern='south=', replacement='')
		north <- sub(north, pattern='north=', replacement='')
		
		west <- as.numeric(west)
		east <- as.numeric(east)
		south <- as.numeric(south)
		north <- as.numeric(north)

		# dimensions
		rows <- info[grepl(info, pattern='rows=')]
		cols <- info[grepl(info, pattern='cols=')]
		depths <- info[grepl(info, pattern='depths=')]

		rows <- sub(rows, pattern='rows=', replacement='')
		cols <- sub(cols, pattern='cols=', replacement='')
		depths <- sub(depths, pattern='depths=', replacement='')
		
		rows <- as.integer(rows)
		cols <- as.integer(cols)
		depths <- as.integer(depths)
		if (length(depths) == 0L) depths <- NA_integer_
		
		# resolution
		ewres <- info[grepl(info, pattern='ewres=')]
		nsres <- info[grepl(info, pattern='nsres=')]
		tbres <- info[grepl(info, pattern='tbres=')]

		ewres <- sub(ewres, pattern='ewres=', replacement='')
		nsres <- sub(nsres, pattern='nsres=', replacement='')
		tbres <- sub(tbres, pattern='tbres=', replacement='')
		
		ewres <- as.numeric(ewres)
		nsres <- as.numeric(nsres)
		tbres <- as.numeric(tbres)
		if (length(tbres) == 0L) tbres <- NA_real_

		# data type
		grassDataType <- info[grepl(info, pattern='datatype=')]
		grassDataType <- sub(grassDataType, pattern='datatype=', replacement='')
		grassDataType <- gsub(grassDataType, pattern='"', replacement='')
		
		if (grassDataType == 'CELL') {
			terraDataType <- 'INT4S'
			gdalDataType <- 'Int32'
		} else if (grassDataType == 'FCELL') {
			terraDataType <- 'FLT4S'
			gdalDataType <- 'Float32'
		} else if (grassDataType == 'DCELL') {
			terraDataType <- 'FLT8S'
			gdalDataType <- 'Float64'
		}
		
		# number of categories
		if (length(nCats) == 0L) nCats <- 0L

		# range of values
		if (nCats == 0) {

			minVal <- range[grepl(range, pattern='min=')]
			maxVal <- range[grepl(range, pattern='max=')]
			
			minVal <- sub(minVal, pattern='min=', replacement='')
			maxVal <- sub(maxVal, pattern='max=', replacement='')
			
			minVal <- if (minVal == 'NULL') { NA_real_ } else { as.numeric(minVal) }
			maxVal <- if (maxVal == 'NULL') { NA_real_ } else { as.numeric(maxVal) }

		} else {
			minVal <- NA_real_
			maxVal <- NA_real_
		}

		out <- list(
			gnames = gn,
			type = type,
			topology = topology,
			
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
			terraDataType = terraDataType,
			gdalDataType = gdalDataType,
			
			nCats = nCats,
			minVal = minVal,
			maxVal = maxVal
			
		)
		
	} # if just one raster
	
	out

}

.vectInfo <- function(x) {

	gn <- if (inherits(x, 'GVector')) {
		gnames(x)
	} else {
		x
	}

	### extent
	suppressMessages(
		info1 <- rgrass::execGRASS(
			'v.info',
			flags = 'g',
			map = gn,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)
	
	### geometry and topology
	suppressMessages(
		info2 <- rgrass::execGRASS(
			'v.info',
			flags = 't',
			map = gn,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)
	
	# geometry (points/lines/polygons)
	points <- info2[grepl(info2, pattern='points=')]
	lines <- info2[grepl(info2, pattern='lines=')]
	boundaries <- info2[grepl(info2, pattern='boundaries=')]
	centroids <- info2[grepl(info2, pattern='centroids=')]
	areas <- info2[grepl(info2, pattern='areas=')]

	points <- sub(points, pattern='points=', replacement='')
	lines <- sub(lines, pattern='lines=', replacement='')
	boundaries <- sub(boundaries, pattern='boundaries=', replacement='')
	centroids <- sub(centroids, pattern='centroids=', replacement='')
	areas <- sub(areas, pattern='areas=', replacement='')

	points <- as.numeric(points)
	lines <- as.numeric(lines)
	boundaries <- as.numeric(boundaries)
	centroids <- as.numeric(centroids)
	areas <- as.numeric(areas)

	if (points > 0 & lines == 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		geometry <- 'points'
		nGeometries <- points
	} else if (points == 0 & lines > 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		geometry <- 'lines'
		nGeometries <- lines
	} else if (points == 0 & lines == 0 & boundaries > 0 & centroids > 0 & areas > 0) {
		geometry <- 'polygons'
		nGeometries <- boundaries
	} else {
		geometry <- NA_character_
		nGeometries <- NA_integer_
	}
	
	nGeometries <- as.integer(nGeometries)

	# extent
	west <- info1[grepl(info1, pattern='west=')]
	east <- info1[grepl(info1, pattern='east=')]
	south <- info1[grepl(info1, pattern='south=')]
	north <- info1[grepl(info1, pattern='north=')]

	west <- sub(west, pattern='west=', replacement='')
	east <- sub(east, pattern='east=', replacement='')
	south <- sub(south, pattern='south=', replacement='')
	north <- sub(north, pattern='north=', replacement='')
	
	west <- as.numeric(west)
	east <- as.numeric(east)
	south <- as.numeric(south)
	north <- as.numeric(north)

	# topology (2/3D)
	topology <- info2[grepl(info2, pattern='map3d=')]
	topology <- sub(topology, pattern='map3d=', replacement='')
	topology <- if (topology == '0') { '2D'} else if (topology == '1') { '3D' }

	# top/bottom
	if (topology == '2D') {
		ztop <- zbottom <- NA_real_
	} else {
		
		zbottom <- info1[grepl(info1, pattern='bottom=')]
		ztop <- info1[grepl(info1, pattern='top=')]
		
		zbottom <- sub(zbottom, pattern='bottom=', replacement='')
		ztop <- sub(ztop, pattern='top=', replacement='')

		zbottom <- as.numeric(zbottom)
		ztop <- as.numeric(ztop)
		
	}

	### fields
	suppressWarnings(
		info3 <- rgrass::execGRASS(
			'v.info',
			flags = 'c',
			map = gn,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)
	
	fieldData <- rgrass::execGRASS('v.db.connect', map=gn, flags='g', intern=TRUE)
	hasNoFields <- any(grepl(fieldData, pattern='is not connected to a database')) | any(grepl(info3, pattern='ERROR: Database connection not defined for layer'))
	
	if (hasNoFields) {

		nFields <- as.integer(0)
		layerName <- NA_character_
		fields <- NA_character_
		classes <- NA_character_

	# vector has a data frame
	} else {

		suppressWarnings(
			info3 <- rgrass::execGRASS(
				'v.info',
				flags = 'c',
				map = gn,
				intern = TRUE,
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE
			)
		)
		
		suppressMessages(
			info4 <- rgrass::execGRASS(
				'v.info',
				flags = 'e',
				map = gn,
				intern = TRUE,
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE
			)
		)
		
		# fields and types
		info3 <- info3[!grepl(info3, pattern ='Displaying column types/names for database connection of layer')]
		info3 <- strsplit(info3, '\\|')
		fields <- classes <- rep(NA, length(info3))
		for (i in seq_along(info3)) {
		
			type <- info3[[i]][1L]
			type <- if (type == 'CHARACTER') {
				'character'
			} else if (type == 'INTEGER') {
				'integer'
			} else if (type == 'DOUBLE PRECISION') {
				'numeric'
			} else {
				NA_character_
			}
		
			classes[i] <- type
			fields[i] <- info3[[i]][2L]
		}
		
		nFields <- length(fields)
		nFields <- as.integer(nFields)

		layerName <- info4[grepl(info4, pattern = 'attribute_layer_name')]
		layerName <- if (length(layerName) == 0) {
			NA_character_
		} else {
			gsub(layerName, pattern='attribute_layer_name=', replacement='')
		}

	}
		
	out <- list(
		gnames = gn,
		type = 'vector',
		topology = topology,
		geometry = geometry,
		
		nGeometries = nGeometries,
		
		west = west,
		east = east,
		south = south,
		north = north,
	
		zbottom = zbottom,
		ztop = ztop,
		
		nFields = nFields,
		layerName = layerName,
		fields = fields,
		classes = classes
	)
	
	out

}
