#' @title Classes for 'fasterRaster' locations, rasters, and vectors
#'
#' @describeIn GSession
#'
#' @importFrom methods new
#' @importFrom methods show
#' @exportClass GRaster
GRaster <- setClass(
	'GRaster',
	contains = 'GSpatial',
	slots = list(
		datatypeGRASS = 'character',
		dimensions = 'integer',
		resolution = 'numeric',
		nLayers = 'integer',
		names = 'character',
		nCats = 'integer',
		minVal = 'numeric',
		maxVal = 'numeric'
	),
	prototype = prototype(
		datatypeGRASS = NA_character_,
		dimensions = c(NA_integer_, NA_integer_, NA_integer_),
		resolution = c(NA_real_, NA_real_, NA_real_),
		nLayers = NA_integer_,
		names = NA_character_,
		nCats = NA_integer_,
		minVal = NA_real_,
		maxVal = NA_real_
	)
)

setValidity('GRaster',
	function(object) {
		if (!all(object@datatypeGRASS %in% c('CELL', 'FCELL', 'DCELL'))) {
			paste0('@datatypeGRASS can only be NA, ', sQuote('CELL'), ', ', sQuote('FCELL'), ', or ', sQuote('DCELL'), '.')
		} else if (any(object@dimensions[1L:2L] <= 0L)) {
			'First two values in @dimensions must be positive integers.'
		} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
			'Third value in @dimensions must be NA or a positive integer.'
		} else if (object@resolution[1L] <= 0) {
			'First @resolution must be a positive real value.'
		} else if (object@resolution[2L] <= 0) {
			'Second @resolution must be a positive real value.'
		} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
			'Third value in @resolution must be NA or a positive real value.'
		} else if (object@nLayers < 1) {
			'@nLayers must be a positive integer.'
		} else if (object@nLayers != length(object@gnames)) {
			'@nLayers is different from the number of @gnames.'
		} else if (object@nLayers != length(object@names)) {
			'@names must be @nLayers in length.'
		} else if (object@nLayers != length(object@nCats)) {
			'@nCats must be @nLayers in length.'
		} else if (object@nLayers != length(object@minVal)) {
			'@minVal must be @nLayers in length.'
		} else if (object@nLayers != length(object@maxVal)) {
			'@maxVal must be @nLayers in length.'
		} else {
			TRUE
		}
	} # EOF
)

#' Create a GRaster
#'
#' @description Create a `GRaster` from a raster existing in the current **GRASS** session.
#'
#' @param gn Character: The name of the raster in **GRASS**.
#' @param names Character: Name of the raster.
#'
#' @returns A `GRaster`.
#'
#' @seealso [makeGVector()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @rdname makeGRaster
#' @export
makeGRaster <- function(gn, names = 'raster') {

	info <- .rastInfo(gn)
	new(
		'GRaster',
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = crs(),
		topology = info[['topology']][1L],
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		zextent = c(info[['zbottom']], info[['ztop']]),
		nLayers = 1L,
		dimensions = c(info[['rows']][1L], info[['cols']][1L], info[['depths']][1L]),
		resolution = c(info[['ewres']][1L], info[['nsres']][1L], info[['tbres']][1L]),
		gnames = gn,
		names = names,
		datatypeGRASS = info[['grassDataType']],
		nCats = info[['nCats']],
		minVal = info[['minVal']],
		maxVal = info[['maxVal']]
	)
}

# show
methods::setMethod(
	f='show',
	signature='GRaster',
	definition = function(object) {

		details <- getFastOptions('details')

		digs <- min(5, getOption('digits'))
		resol <- round(object@resolution, digs)
		if (length(resol) == 2L) resol <- c(resol, NA_real_)
		
		digs <- min(1, getOption('digits'))
		extent <- round(object@extent, digs)

		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input

		# pad everything by same amount so display of data for each later appears in neat columns
		minVal <- round(object@minVal, digs)
		maxVal <- round(object@maxVal, digs)		
		
		minValLength <- nchar(minVal)
		maxValLength <- nchar(maxVal)
		
		if (anyNA(minValLength)) minValLength[is.na(minValLength)] <- 2L
		if (anyNA(maxValLength)) maxValLength[is.na(maxValLength)] <- 2L
		
		nc <- pmax(
			rep(3, object@nLayers),
			nchar(object@names),
			nchar(object@datatypeGRASS),
			nchar(object@nCats),
			minValLength,
			maxValLength
		)
		if (details) nc <- pmax(nc, nchar(object@gnames))

		gnames <- names <- datatype <- nCats <- minValChar <- maxValChar <- rep(NA, object@nLayers)
		for (i in seq_len(object@nLayers)) {
			fmt <- paste0('%', nc[i], 's')
			gnames[i] <- sprintf(fmt, object@gnames[i])
			names[i] <- sprintf(fmt, object@names[i])
			datatype[i] <- sprintf(fmt, object@datatypeGRASS[i])
			nCats[i] <- sprintf(fmt, object@nCats[i])
			minValChar[i] <- sprintf(fmt, minVal[i])
			maxValChar[i] <- sprintf(fmt, maxVal[i])
		}
		
		gnames <- paste(gnames, collapse=' ')
		names <- paste(names, collapse=' ')
		datatype <- paste(datatype, collapse=' ')
		minValChar <- paste(minValChar, collapse=' ')
		maxValChar <- paste(maxValChar, collapse=' ')

		cat('class       : GRaster\n')
		if (details) {
			cat('location    :', object@location, '\n')
			cat('mapset      :', object@mapset, '\n')
		}
		cat('topology    :', object@topology, '\n')
		cat('coord ref.  :', crs, '\n')
		cat('dimensions  :', paste(c(object@dimensions, object@nLayers), collapse=', '), '(nrow, ncol, ndepth, nlyr)\n')
		cat('resolution  :', paste(resol, collapse=', '), '(x, y, z)\n')
		cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
		if (details | object@topology == '3D') {
			cat('z extent    :', paste(object@zextent, collapse=', '), '(bottom, top)\n')
		}
		if (details) cat('gnames(s)   :', gnames, '\n')
		cat('name(s)     :', names, '\n')
		cat('datatype*   :', datatype, '\n')
		if (details | any(ncat(object) > 0L)) cat('num. categ. :', nCats, '\n')
		cat('min. value  :', minValChar, '\n')
		cat('max. value  :', maxValChar, '\n')
		cat('* GRASS datatype\n')
	}
)

# print
methods::setMethod(f='print', signature='GRaster',
	definition = function(x) {
		show(x)
	}
)
