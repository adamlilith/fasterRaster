#' @title Classes for 'fasterRaster' locations, rasters, and vectors
#'
#' @describeIn GSession
#'
#' @importFrom methods new
#' @exportClass GVector

#' @exportClass GMetaTable
#' @exportClass GEmptyMetaTable
#' @exportClass GFullMetaTable
GMetaTable <- setClass('GMetaTable')
GEmptyMetaTable <- setClass('GEmptyMetaTable', contains = 'GMetaTable')
GFullMetaTable <- setClass(
	'GFullMetaTable',
	contains = 'GMetaTable',
	slots = list(
		layerName = 'character',
		fields = 'character',
		classes = 'character'
	)
)

setValidity('GFullMetaTable',
	function(object) {
		if (length(object@fields) != length(object@classes)) {
			'Number of @fields must be the same as the number of @classes'
		} else if (is.na(object@layerName)) {
			'@layerName cannot be NA.'
		} else {
			TRUE
		}
	} # EOF
)


#' @exportClass GVector
GVector <- methods::setClass(
	'GVector',
	contains = 'GSpatial',
	slots = list(
		nGeometries = 'integer',
		geometry = 'character',
		nFields = 'integer',
		df = 'GMetaTable'
	),
	prototype = prototype(
		geometry = NA_character_,
		nGeometries = NA_integer_,
		nFields = NA_integer_,
		df = GEmptyMetaTable()
	)
)


setValidity('GVector',
	function(object) {
		if (!all(object@geometry %in% c(NA_character_, 'points', 'lines', 'polygons'))) {
			paste0('@geometry can only be NA, ', sQuote('points'), ', ', sQuote('lines'), ', or ', sQuote('polygons'), '.')
		# } else if (!inherits(object@df, 'GEmptyMetaTable') && object@nFields != length(object@df@fields)) {
			# '@fields does not have @nFields values.'
		# } else if (!inherits(object@df, 'GEmptyMetaTable') && object@nFields != length(object@df@classes)) {
			# '@classes does not have @nFields values.'
		} else {
			TRUE
		}
	} # EOF
)

#' Create a GVector
#'
#' @description Create a `GVector` from a vector existing in the current **GRASS** session.
#'
#' @param gn Character: The name of the vector in **GRASS**.
#'
#' @returns A `GVector`.
#'
#' @seealso [makeGRaster()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @rdname makeGVector
#' @export
makeGVector <- function(gn) {

	info <- .vectInfo(gn)
	
	df <- if (is.na(info[['fields']][1L])) {
		GEmptyMetaTable()
	} else {
		GFullMetaTable(
			layerName = info[['layerName']],
			fields = info[['fields']],
			classes = info[['classes']]
		)
	}
	
	new(
		'GVector',
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = crs(),
		topology = info[['topology']][1L],
		gnames = gn,
		geometry = info[['geometry']][1L],
		nGeometries = info[['nGeometries']],
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		zextent = c(info[['zbottom']], info[['ztop']]),
		nFields = info[['nFields']],
		df = df
	)
	
}

#' @importFrom methods show
#' @aliases show
#' @exportMethod show
methods::setMethod(f='show', signature='GVector',
	definition = function(object) {

	details <- getFastOptions('details')

	digs <- min(3, getOption('digits'))
	extent <- round(object@extent, digs)
	zextent <- round(object@zextent, digs)

	# concatenate fields across vectors for display
	# making list, each element becomes a line in the display
	# each element contains field n from first vector, field n from second vector, etc.
	# will then print each element on one line
	if (object@nFields > 0L) {

		fields <- object@df@fields
		classes <- object@df@classes
		maxFieldsToShow <- min(object@nFields, 10L)
		
		if (object@nFields > 0L) {
		
			fields <- fields[1L:maxFieldsToShow]
			classes <- classes[1L:maxFieldsToShow]
			
			classes <- gsub(classes, pattern='integer', replacement='<int>')
			classes <- gsub(classes, pattern='numeric', replacement='<num>')
			classes <- gsub(classes, pattern='character', replacement='<chr>')
			
		}
		
		ncFields <- nchar(fields)
		ncFieldClasses <- nchar(classes)
		nc <- pmax(ncFields, ncFieldClasses)
		
		for (i in seq_along(fields)) {
		
			fmt	<- paste0('%', nc[i], 's')
			fields[i] <- sprintf(fmt, fields[i])
			classes[i] <- sprintf(fmt, classes[i])
		}
		
		if (object@nFields > maxFieldsToShow) {
			classes <- c(classes, paste('(and', object@nFields - maxFieldsToShow, 'more)'))
		}
		
	}
	
	# CRS
	crs <- object@crs
	crs <- sf::st_crs(crs)
	crsSimple <- crs$input

	cat('class       : GVector\n')
	if (details) {
		cat('location    :', object@location, '\n')
		cat('mapset      :', object@mapset, '\n')
		cat('gname       :', object@gnames, '\n')
		if (details & object@nFields > 0) cat('layer name  :', object@df@layerName, '\n')
	}
	cat('geometry    :', object@geometry, '\n')
	cat('dimensions  :', paste0(object@nGeometries, ', ', object@nFields), '(geometries, columns)\n')
	cat('topology    :', object@topology, '\n')
	cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
	if (details | object@topology == '3D') cat('z extent    :', paste(zextent, collapse=', '), '(bottom, top)\n')
	cat('coord ref.  :', crsSimple, '\n')

	if (object@nFields > 0L) {
		cat('names       :', fields, '\n')
		cat('type        :', classes, '\n')
	}
	
	} # EOF

)

# print
methods::setMethod(f='print', signature='GVector',
	definition = function(x) {
		show(x)
	}
)
