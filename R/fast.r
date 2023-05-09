#' @title Create a GRaster or GVector
#'
#' @description
#' To use most **fasterRaster** functions, you need to
#' 1. Initiate a **GRASS** session with [fastStart()];
#' 2. Then use `fast()` to either:
#'      * Convert a `SpatRaster`, `SpatVector`, `sf` object, or `stars` raster to a `GRaster` or `GVector`,
#'      * Load a raster or vector from a file directly into the `GRaster` or `GVector` format. **GRASS** supports loading from disk a variety of raster (see the **GRASS** manual page for `r.in.gdal`) and vector(see the **GRASS** manual page for `v.in.ogr`) formats, though not all of them will work with this function.
#'
#' @param x Any one of:
#' * A `SpatRaster` or `stars` raster. Rasters can have one or more layers. They will retain their "layerdness" in most **fasterRaster** functions.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string with the path and filename of a raster or vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param rastOrVect Either `NULL` (default) or character (`raster` or `vector`). If `x` is a raster or vector already in **R**, this does not need to be specified. However, if `x` is a filename, then the function will try to ascertain the data type from the filenames, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' @seealso [rgrass::read_RAST()] and [rgrass::read_VECT()], plus **GRASS** modules `r.in.gdal` and `v.in.ogr`
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
	function(x, rastOrVect = NULL) {

	### function globals
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
	if (rastOrVect == 'raster') {

		rast <- terra::rast(x)
		nLayers <- terra::nlyr(rast)
		nLayers <- as.integer(nLayers)
		names <- names(rast)
		gn <- .makeGname(rast, rastOrVect = 'raster')

		regionShape(rast)
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

		out <- makeGRaster(gn, names=names)

	### load vector from disk
	} else if (rastOrVect == 'vector') {

		vect <- terra::vect(x)
		gn <- .makeGname(x, rastOrVect = 'vector')
		rgrass::execGRASS('v.in.ogr', input=x, output=gn, flags=flags, intern=TRUE)

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

	regionShape(x)

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

		groupName <- .makeGname(NULL, rastOrVect = 'raster')
		rgrass::execGRASS('i.group', group=groupName, input=gn, flags='quiet', intern=TRUE)

	}

	makeGRaster(gn, names(x))

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
