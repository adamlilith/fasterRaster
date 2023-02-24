#' Information on rasters and vectors in 'GRASS'
#'
#' Private. Reports extent, dimensions, resolution, bottom/top, etc.
#'
#' @keywords internal

.rastInfo <- function(gname) {

	### more than one raster
	#######################
	if (length(gname) > 1L) {
		
		for (i in seq_along(gname)) {
		
			this <- .rastInfo(gname[i])
			out <- if (exists('out', inherits=FALSE)) {
				appendLists(out, this)
			} else {
				this
			}
		
		}
	
	} else {
	### a single gname
	##################

		suppressMessages(
			info1 <- rgrass::execGRASS(
				'r.info',
				flags = c('g', 'quiet'),
				map = gname,
				intern = TRUE,
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE
			)
		)

		suppressMessages(
			info2 <- rgrass::execGRASS(
				'r.univar',
				flags = c('r'),
				map = gname,
				intern = TRUE,
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE
			)
		)

		suppressMessages(
			info3 <- rgrass::execGRASS(
				'r.info',
				flags = c('s', 'e'),
				map = gname,
				intern = TRUE,
				Sys_show.output.on.console = FALSE,
				echoCmd = FALSE
			)
		)

		# topology
		topology <- info3[grepl(info3, pattern='vdatum=')]
		topology <- sub(topology, pattern='vdatum=\"', replacement='')
		topology <- sub(topology, pattern='\"', replacement='')
		topology <- if (topology == 'none') { '2D' } else { '3D' }

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

		# dimensions
		rows <- info1[grepl(info1, pattern='rows=')]
		cols <- info1[grepl(info1, pattern='cols=')]

		rows <- sub(rows, pattern='rows=', replacement='')
		cols <- sub(cols, pattern='cols=', replacement='')
		
		rows <- as.integer(rows)
		cols <- as.integer(cols)
		
		# resolution
		ewres <- info1[grepl(info1, pattern='ewres=')]
		nsres <- info1[grepl(info1, pattern='nsres=')]

		ewres <- sub(ewres, pattern='ewres=', replacement='')
		nsres <- sub(nsres, pattern='nsres=', replacement='')
		
		ewres <- as.numeric(ewres)
		nsres <- as.numeric(nsres)

		# data type
		grassDataType <- info1[grepl(info1, pattern='datatype=')]
		grassDataType <- sub(grassDataType, pattern='datatype=', replacement='')
		
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
		numCategories <- info1[grepl(info1, pattern='ncats=')]
		numCategories <- sub(numCategories, pattern='ncats=', replacement='')
		numCategories <- as.integer(numCategories)

		# range of values
		if (numCategories == 0) {

			minVal <- info2[grepl(info2, pattern='minimum: ')]
			maxVal <- info2[grepl(info2, pattern='maximum: ')]
			
			minVal <- sub(minVal, pattern='minimum: ', replacement='')
			maxVal <- sub(maxVal, pattern='maximum: ', replacement='')
			
			minVal <- as.numeric(minVal)
			maxVal <- as.numeric(maxVal)

		} else {
			minVal <- NA
			maxVal <- NA
		}

		out <- list(
			gname = gname,
			type = 'raster',
			topology = topology,
			
			west = west,
			east = east,
			south = south,
			north = north,
		
			rows = rows,
			cols = cols,
		
			ewres = ewres,
			nsres = nsres,
			
			grassDataType = grassDataType,
			terraDataType = terraDataType,
			gdalDataType = gdalDataType,
			
			numCategories = numCategories,
			minVal = minVal,
			maxVal = maxVal
			
		)
		
	} # if just one raster
	
	out

}

.vectInfo <- function(gname) {

	# extent
	suppressMessages(
		info1 <- rgrass::execGRASS(
			'v.info',
			flags = 'g',
			map = gname,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)
	
	# geometry and topology
	suppressMessages(
		info2 <- rgrass::execGRASS(
			'v.info',
			flags = 't',
			map = gname,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)
	
	# fields
	suppressMessages(
		info3 <- rgrass::execGRASS(
			'v.info',
			flags = 'c',
			map = gname,
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

	geometry <- if (points > 0 & lines == 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		'points'
	} else if (points == 0 & lines > 0 & boundaries == 0 & centroids == 0 & areas == 0) {
		'lines'
	} else if (points == 0 & lines == 0 & boundaries > 0 & centroids > 0 & areas > 0) {
		'polygons'
	} else {
		NA
	}

	# topology (2/3D)
	topology <- info2[grepl(info2, pattern='map3d=')]
	topology <- sub(topology, pattern='map3d=', replacement='')
	topology <- if (topology == '0') { '2D'} else if (topology == '1') { '3D' }

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

	# top/bottom
	bottom <- info1[grepl(info1, pattern='bottom=')]
	top <- info1[grepl(info1, pattern='top=')]
	
	bottom <- sub(bottom, pattern='bottom=', replacement='')
	top <- sub(bottom, pattern='top=', replacement='')

	bottom <- as.numeric(bottom)
	top <- as.numeric(top)

	# fields and types
	info3 <- info3[!grepl(info3, pattern ='INTEGER|cat')]
	info3 <- info3[!grepl(info3, pattern ='Displaying column types/names for database connection of layer')]
	info3 <- strsplit(info3, '\\|')
	fields <- fieldClasses <- rep(NA, length(info3))
	for (i in seq_along(info3)) {
	
		type <- info3[[i]][1L]
		type <- if (type == 'CHARACTER') {
			'character'
		} else if (type == 'INTEGER') {
			'integer'
		} else if (type == 'DOUBLE PRECISION') {
			'numeric'
		}
	
		fieldClasses[i] <- type
		fields[i] <- info3[[i]][2L]
	}
	
	out <- list(
		gname = gname,
		type = 'vector',
		topology = topology,
		geometry = geometry,
		
		west = west,
		east = east,
		south = south,
		north = north,
	
		bottom = bottom,
		top = top,
		
		fields = fields,
		fieldClasses = fieldClasses
	)
	
	out

}
