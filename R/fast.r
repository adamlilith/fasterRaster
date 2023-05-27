#' @title Create a GRaster or GVector
#'
#' @description
#' To use most **fasterRaster** functions, you need to
#' 1. Initiate a **GRASS** session with [faster()];
#' 2. Then use `fast()` to either:
#'      * Convert a `SpatRaster`, `SpatVector`, `sf` object, or `stars` raster to a `GRaster` or `GVector`,
#'      * Load a raster or vector from a file directly into the `GRaster` or `GVector` format. **GRASS** supports loading from disk a variety of raster (see the **GRASS** manual page for `r.in.gdal`) and vector(see the **GRASS** manual page for `v.in.ogr`) formats, though not all of them will work with this function.
#'
#' @param x Any one of:
#' * A `SpatRaster` or `stars` raster. Rasters can have one or more layers. They will retain their "layerdness" in most **fasterRaster** functions.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string with the path and filename of a raster or vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param rastOrVect Either `NULL` (default) or character (`'raster'` or `'vector'`). If `x` is a raster or vector already in **R**, this does not need to be specified. However, if `x` is a filename, then the function will try to ascertain the data type from the filenames, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' @param method Character or `NULL` (rasters only): If `x` does not have the same coordinate reference system as the currently active **GRASS** [session][tutorial_sessions], then it will be projected when it is imported. You may need to specify which method is used to conduction the transformation. Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data)
#' * `'near'`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.
#' * `'bilinear'`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `'bicubic'`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `'lanczos'`: Lanczos interpolation (uses weighted values from 25 cells).
#'
#' @param trim Logical (rasters only): When importing rasters from disk that "wrap around" (i.e., whole-world rasters or rasters that have edges that actually circle around to meet on the globe), `trim` should be `FALSE` to avoid removing rows and columns from the "edge" of the map.
#'
#' @param warn Logical: If `TRUE`, display a warning when projecting the vector or raster.
#'
#' @details When projecting a raster, the "fallback" methods in `r.import` are actually used, even though the `method` argument takes the strings for non-fallback methods. See the manual page for the `r.import` **GRASS** module.
#' 
#' @seealso [rgrass::read_RAST()] and [rgrass::read_VECT()], plus **GRASS** modules `r.in.gdal`, `r.import`, and `v.in.ogr`.
#'
#' @return A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_fastStart.r
#'
#' @aliases fast
#' @rdname fast
#' @export
#' @exportMethod fast
methods::setMethod(
	'fast',
	signature(x = 'character'),
	function(
		x,
		rastOrVect = NULL,
		method = NULL,
		trim = TRUE,
		warn = TRUE
	) {

	### raster or vector?
	#####################
	if (is.null(rastOrVect)) {

		### attempt to get type from extension
		nc <- nchar(x)

		# 3-letter extensions
		rastExtensions <- c('.asc', '.grd', '.img', '.mem', '.tif')
		vectExtensions <- c('.shp')

		ext <- substr(x, nc - 3, nc)
		ext <- tolower(ext)

		if (ext %in% rastExtensions) {
			rastOrVect <- 'raster'
		} else if (ext %in% vectExtensions) {
			rastOrVect <- 'vector'
		}

		# 4-letter extensions
		rastExtensions <- c('.saga')
		vectExtensions <- c('.gpkg')

		ext <- substr(x, nc - 4, nc)
		ext <- tolower(ext)

		if (ext %in% rastExtensions) {
			rastOrVect <- 'raster'
		} else if (ext %in% vectExtensions) {
			rastOrVect <- 'vector'
		}

		if (is.null(rastOrVect)) stop('Cannot determine data if raster or vector from file name. Please use argument ', sQuote('rastOrVect'), '.')

	} else {
	### user supplied rastOrVect
		rastOrVect <- .pmatch(rastOrVect, c('raster', 'vector'))
	}

	### raster from disk
	####################
	
	if (rastOrVect == 'raster') {

		rast <- terra::rast(x)

		### project raster (if needed)
		if (terra::crs(rast) != crs()) {
		
			if (warn) warning('Raster has a different coordinate reference system than this GRASS ', dQuote('location'), '\n  Raster will be projected to the current location\'s coordinate reference system.')
		
			# put focal raster in its a location with the same CRS
			toLocation <- location()
			toMapset <- mapset()
			toWorkDir <- getFastOptions('workDir')

			fromLoc <- paste0('location_', rstring(1))
			fromWorkDir <- tempdir()
			faster(crs = rast, location=fromLoc, mapset='PERMANENT', workDir=fromWorkDir)
			inRast <- fast(x, rastOrVect='raster')

			# project raster to starting location
			fastRestore(location=toLocation, mapset=toMapset, workDir=toWorkDir)
			out <- project(inRast, method=method, trim=trim)
		
			fastRemove(location=fromLoc, workDir=fromWorkDir)
		
		### if projecting
		} else {
		
		# # ### autodetect method for resampling
		# # if (is.null(method)) {
		
			# # categories <- terra::cats(rast)
			
			# # hasCats <- FALSE
			# # for (i in seq_along(categories)) {
				# # if (!is.null(categories[[i]])) hasCats <- TRUE
			# # }

			# # method <- if (hasCats) {
				# # 'nearest'
			# # } else {
				# # 'bilinear'
			# # }
		
		# # }
		
		# # method <- .pmatch(method, c('nearest', 'bilinear_f', 'bicubic_f', 'lanczos_f'))

		# # ### resample/project
		# # flags <- c('quiet', 'overwrite')
		# # if (!trim) flags <- c(flags, 'n')
	
		# # args <- list(
			# # cmd = 'r.import',
			# # input = x,
			# # output = gn[1L],
			# # resample = method,
			# # memory = getFastOptions('memory'),
			# # # extent = 'input',
			# # extent = 'region',
			# # # resolution = 'estimated',
			# # resolution = 'region',
			# # flags = flags,
			# # intern = TRUE
		# # )
	
		# # do.call(rgrass::execGRASS, args=args)

			nLayers <- terra::nlyr(rast)
			nLayers <- as.integer(nLayers)
			names <- names(rast)
			gn <- .makeGname(rast, rastOrVect = 'raster')

			region(rast)
			rgrass::execGRASS('r.in.gdal', input=x, output=gn[1L], flags=c('quiet'), intern=TRUE)

			# if multi-layer raster, rasters are imported using the first name plus a '.#' where # is a number, so they need renamed
			if (nLayers > 1L) {

				baseName <- gn[1L]

				for (i in 1L:nLayers) {

					from <- paste0(baseName, '.', i)
					to <- gn[i]
					.rename(from=from, to=to, rastOrVect='raster')

				}

			}

			for (i in 1L:nLayers) {
				if (i == 1L) {
					out <- makeGRaster(gn[1L], names=names[1L])
				} else {
					out <- c(out, makeGRaster(gn[i], names=names[i]))
				}
			}
			
		} # if not projecting

	### vector from disk (and project on the fly if needed)
	#######################################################
	
	} else if (rastOrVect == 'vector') {

		vect <- terra::vect(x)
		gn <- .makeGname(x, rastOrVect = 'vector')
		rgrass::execGRASS('v.in.ogr', input=x, output=gn, flags=c('quiet', 'overwrite'), intern=TRUE)

		out <- makeGVector(gn)

	}

	out

	} # EOF
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	'fast',
	signature(x = 'SpatRaster'),
	function(x) {

	### set region
	##############

	region(x)

	### import raster
	#################

	gn <- .makeGname(x)
	names <- names(x)
	nLayers <- terra::nlyr(x)
	nLayers <- as.integer(nLayers)

	# NB writing first raster in a stack actually writes all of them
	rastToGrass(x[[1L]], gn=gn[1L], flags=c('quiet', 'overwrite'))

	# if multi-layer raster, rasters are imported using the first name plus a '.#' where # is a number, so they need renamed
	warning('ABS: If raster was part of a stack then subset, all layers are still imported into GRASS.')
	
	if (nLayers > 1L) {

		baseName <- gn[1L]

		for (i in 1L:nLayers) {

			from <- paste0(baseName, '.', i)
			to <- gn[i]
			.rename(from=from, to=to, rastOrVect='raster')

		}

		# groupName <- .makeGname(NULL, rastOrVect = 'raster')
		# rgrass::execGRASS('i.group', group=groupName, input=gn, flags='quiet', intern=TRUE)

	}

	for (i in 1L:nLayers) {
		out <- if (i == 1L)	{
			makeGRaster(gn[i], names(x)[i])
		} else {
			c(out, makeGRaster(gn[i], names(x)[i]))
		}
	}
	
	out
	
	} # EOF
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	'fast',
	signature(x = 'stars'),
	function(x) {

	x <- terra::rast(x)
	out <- fast(x)
	out

	} # EOF
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	'fast',
	signature(x = 'SpatVector'),
	function(x) {
		out <- .fastVector(x)
		out
	}
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	'fast',
	signature(x = 'sf'),
	function(x) {
	
		x <- terra::vect(x)
		out <- .fastVector(x)
		out
	}
)

.fastVector <- function(x) {

	gn <- .makeGname(names, 'vector')
	vectToGrass(x, gn=gn)
	makeGVector(gn)
	
} # EOF
