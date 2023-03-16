#' @title Arithmetic operations on 'GRasters'
#'
#' @name arithmetic
#' 
#' @description You can do normal arithmetic operations on `GRaster`s using normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division).
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_Graster_arithmetic.r
#'
#' @export

.genericArith <- function(x, gname, ex) {

	# x 		GRaster
	# gname		gname of the raster being operated on
	# ex 		expression for r.mapcalc

	rgrass::execGRASS('r.mapcalc', expression = ex, flags = c('quiet', 'overwrite'), intern = TRUE)

	info <- .rastInfo(gname)
	GRaster(
		location = getFastOptions('location'),
		mapset = getFastOptions('mapset'),
		crs = terra::crs(x),
		gname = gname,
		name = names(x),
		topology = info[['topology']][1L],
		extent = c(info[['west']][1L], info[['east']][1L], info[['south']][1L], info[['north']][1L]),
		ztop = info[['ztop']],
		zbottom = info[['zbottom']],
		datatypeGRASS = info[['grassDataType']],
		dimensions = c(info[['rows']][1L], info[['cols']][1L], info[['depths']][1L], nlyr(x)),
		resolution = c(info[['ewres']][1L], info[['nsres']][1L], info[['tbres']][1L]),
		numCategories = info[['numCategories']],
		minVal = info[['minVal']],
		maxVal = info[['maxVal']]
	)

}

setMethod('Arith', signature(e1 = 'GRaster', e2 = 'logical'),
    function(e1, e2) {
	
		.restore(e1)
		oper <- as.vector(.Generic)[1L]
		e2 <- as.integer(e2)
		gname <- .makeGname(NULL, 'rast')
		ex <- paste(gname, '=', .gname(e1), oper, e2)
		.genericArith(x = x, gname = gname, ex = ex)
		
	}
)

setMethod('Arith', signature(e1 = 'logical', e2 = 'GRaster'),
    function(e1, e2) {
	
		.restore(e2)
		oper <- as.vector(.Generic)[1L]
		e1 <- as.integer(e1)
		gname <- .makeGname(NULL, 'rast')
		ex <- paste(gname, '=', e1, oper, .gname(e2))
		.genericArith(x = x, gname = gname, ex = ex)
		
	}
)

setMethod('Arith', signature(e1 = 'GRaster', e2 = 'GRaster'),
    function(e1, e2) {
	
		comparable(e1, e2, fail = TRUE)
		.restore(e1)
		oper <- as.vector(.Generic)[1L]
		gname <- .makeGname(NULL, 'rast')
		ex <- if (oper == '%/%') {
			paste(gname, '=', 'floor(', .gname(e1), '/', .gname(e2), ')')
		} else if (oper == '^') {
			paste(gname, '= exp(', .gname(e1), ', ', .gname(e2), ')')
		} else {
			paste(gname, '=', .gname(e1), oper, .gname(e2))
		}
		
		.genericArith(x = e1, gname = gname, ex = ex)
		
	}
)

