#' Export a raster or vector to 'GRASS'
#'
#' @description To use a raster or vector in with most **fasterRaster** functions, you need to 1) initiate a **GRASS** session with [startfast()], then either 2a) export one or more rasters or vectors to the session, or 2b) use [readRast()] or [readVect()] to load a raster or vector from disk directly into **GRASS**.  This function allows you to export a raster or vector to a **GRASS** session. **GRASS** supports loading from disk a vareity of [raster](https://grass.osgeo.org/grass82/manuals/r.in.gdal.html) and [vector](https://grass.osgeo.org/grass82/manuals/v.in.ogr.html) formats.
#'
#' @param ... One or more of:
#' * A `SpatRaster` or `stars` raster. Rasters can have one or more layers. They will retain their 'layerdness' in most **fasterRaster** functions.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character vector with the path and filename of one or more rasters or vector to be loaded directly into **GRASS**. The function will attempyt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#' Mixing data types is allowed (i.e., you can export rasters and vectors to **GRASS** with one call of this function).
#' 
#' @param gname The name of the raster or vector in the **GRASS** session. Depending on how you intend to use **fasterRaster** functions, you may or may not want to use this argument:
#' * For most cases: The names of rasters an vectors in **GRASS** is assigned automatically, and you do not need to use this argument (i.e., leave it as `NULL`).
#' * For cases where you intend to use the same `GRASS` project folder in `GRASS` and do not want random-looking names for rasters and vectors: Use this argument to assign a human-friendly name to the raster or vector.
#' 
#' @param rastOrVect Either `NULL` (default) or character (`raster` or `vector`). If `...` is a raster or vector, this does not need to be specified. However, if `...` is one or more filenames, then the function will try to ascertain the data type from the file name(s), but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used. If `...` has more than one file name, you can specify one per file name, or just a single value (`raster` or `vector`), in which case it will be assumed that all are of that type.
#'
#' @return A **fasterRaster** `GRaster` or `GVector` object. Also exports a raster or vector to the active **GRASS** session.
#'
#' @example man/ex_fast.r
#'
#' @export

# if (!isGeneric('fast')) { setGeneric('fast', function(x, ...) standardGeneric('fast')) }
setGeneric('fast', function(x, rastOrVect = NULL, format = NULL) standardGeneric('fast'))

### SpatRaster in R
###################
setMethod(
	'fast',
	signature(x = 'SpatRaster'),
	function(x) {

	### function globals
	####################
	
	flags <- c('quiet', 'overwrite')

	### import raster(s)
	####################

	gname <- .makeGname(x)
	rname <- names(x)
	nLayers <- terra::nlyr(x)
	nLayers <- as.integer(nLayers)
	
	# NB writing first raster in a stack actually writes all of them
	rgrass::write_RAST(x[[1L]], vname=gname[1L], flags=flags, verbose=FALSE)
	
	# if multi-layer raster, rasters are imported using the first name plus a '.#' where # is a number, so they need renamed
	warning('ABS: If raster was part of a stack then subset, all layers are still imported into GRASS.')
	# if (nLayers > 1L) {
	
		baseName <- gname[1L]
	
		for (i in 1L:nLayers) {
		
			from <- paste0(baseName, '.', i)
			to <- gname[i]
			.rename(from=from, to=to, rastOrVect='raster')
		
		}
		
	# }

	info <- .rastInfo(gname)
	
	out <- GRaster(
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = terra::crs(x),
		gname = gname,
		rname = rname,
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		topology = info[['topology']][1L],
		datatypeGRASS = info[['grassDataType']],
		dimensions = c(info[['rows']][1L], info[['cols']][1L], nLayers),
		resolution = c(info[['ewres']][1L], info[['nsres']][1L]),
		numCategories = info[['numCategories']],
		minVal = info[['minVal']],
		maxVal = info[['maxVal']]
	)

	out
	
	} # EOF
)

### stars in R
##############
setMethod(
	'fast',
	signature(x = 'stars'),
	function(x) {

	x <- terra::rast(x)
	out <- fast(x)
	out

	} # EOF
)

### load from file (raster or vector)
#####################################
setMethod(
	'fast',
	signature(x = 'character'),
	function(x, rastOrVect = NULL) {

	### function globals
	####################
	
	flags <- c('quiet', 'overwrite')

	### raster or vector?
	#####################
	if (is.null(rastOrVect)) {
		
		### attempt to get type from extension
		nc <- nchar(x)
		
		# 3-letter extensions
		rastExtensions <- c('.asc', '.grd', '.img', '.mem', '.tif')
		vectExtensions <- c('.shp')

		ext <- substr(x, nc - 3, nc)
		
		if (ext %in% rastExtensions) {
			rastOrVect <- 'raster'
		} else if (ext %in% vectExtensions) {
			rastOrVect <- 'vector'
		}
		
		# 4-letter extensions
		rastExtensions <- c('.saga')
		vectExtensions <- c('.gpkg')

		ext <- substr(x, nc - 4, nc)
		
		if (ext %in% rastExtensions) {
			rastOrVect <- 'raster'
		} else if (ext %in% vectExtensions) {
			rastOrVect <- 'vector'
		}
		
		if (is.null(rastOrVect)) stop('Cannot determine data if raster or vector from file name. Please use <rastOrVect>.')
	
	} else {
	### user supplied rastOrVect
		rastOrVect <- .pmatch(rastOrVect, c('raster', 'vector'))
	}

	### raster from disk
	if (rastOrVect == 'raster') {
	
		rast <- terra::rast(x)
		nLayers <- terra::nlyr(rast)
		nLayers <- as.integer(nLayers)
		rname <- names(rast)
		gname <- .makeGname(rast, rastOrVect = 'raster')
		rgrass::execGRASS('r.in.gdal', input=x, output=gname[1L], flags=flags, intern=TRUE)
			
		# if multi-layer raster, rasters are imported using the first name plus a '.#' where # is a number, so they need renamed
		if (nLayers > 1L) {
		
			baseName <- gname[1L]
		
			for (i in 1L:nLayers) {
			
				from <- paste0(baseName, '.', i)
				to <- gname[i]
				.rename(from=from, to=to, rastOrVect='raster')
			
			}
			
		}

		info <- .rastInfo(gname)
		
		out <- GRaster(
			location = getFastOptions('location'),
			mapset = getFastOptions('mapset'),
			crs = terra::crs(rast),
			gname = gname,
			rname = rname,
			extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
			topology = info[['topology']][1L],
			datatypeGRASS = info[['grassDataType']],
			dimensions = c(info[['rows']][1L], info[['cols']][1L], nLayers),
			resolution = c(info[['ewres']][1L], info[['nsres']][1L]),
			numCategories = info[['numCategories']],
			minVal = info[['minVal']],
			maxVal = info[['maxVal']]
		)

	### load vector from disk
	} else if (rastOrVect == 'vector') {
	
		vect <- terra::vect(x)
		gname <- .makeGname(x, rastOrVect = 'vector')
		rgrass::execGRASS('v.in.ogr', input=x, output=gname, flags=flags, intern=TRUE)
	
		info <- .vectInfo(gname)
		
		out <- GVector(
			location = getFastOptions('location'),
			mapset = getFastOptions('mapset'),
			crs = terra::crs(vect),
			gname = gname,
			extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
			topology = info[['topology']][1L],
			geometry = info[['geometry']][1L],
			bottom = info[['bottom']],
			top = info[['top']],
			fields = info[['fields']],
			fieldClasses = info[['fieldClasses']]
		)

	}

	out

	} # EOF
)

### vector in R
###############
setMethod(
	'fast',
	signature(x = 'SpatVector'),
	function(x) {

	### function globals
	####################
	
	flags <- c('quiet', 'overwrite')

	### import raster(s)
	####################

	gname <- .makeGname(x)
	
	# NB writing first raster in a stack actually writes all of them
	rgrass::write_VECT(x, vname=gname, flags=flags, ignore.stderr=TRUE)
	
	info <- .vectInfo(gname)
	
	out <- GVector(
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = terra::crs(x),
		gname = gname,
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		topology = info[['topology']][1L],
		geometry = info[['geometry']][1L],
		bottom = info[['bottom']],
		top = info[['top']],
		fields = info[['fields']],
		fieldClasses = info[['fieldClasses']]
	)

	out
	
	} # EOF
)

