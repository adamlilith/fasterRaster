#' Comparison operations on GRasters
#'
#' @description You can do comparative operations on `GRaster`s using normal operators in **R**: `<`, `<=`, `==`, `!=`, `>=`, and `>`.
#' 
#' @param e1,e2 `GRaster`, logical, or numeric
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @rdname Comparison
#' @noRd

# raster raster
methods::setMethod('Ops', signature(e1 = 'GRaster', e2 = 'GRaster'),
    function(e1, e2) {
	
		comparable(e1, e2)
		.restore(e1)

		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- paste(gn, '= ', gnames(e1), ' ', oper, ' ', gnames(e2))
		.genericArith(x = e1, gn = gn, ex = ex)
		
	}
)

# logical raster
methods::setMethod('Ops', signature(e1 = 'logical', e2 = 'GRaster'),
    function(e1, e2) {
	
		.restore(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- 'null()'
		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- paste(gn, '= ', e1, ' ', oper, ' ', gnames(e2))
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)

# raster logical
methods::setMethod('Ops', signature(e1 = 'GRaster', e2 = 'logical'),
    function(e1, e2) {
	
		.restore(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- 'null()'
		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- paste(gn, '= ', gnames(e1), ' ', oper, ' ', e2)
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)

# numeric raster
methods::setMethod('Ops', signature(e1 = 'numeric', e2 = 'GRaster'),
    function(e1, e2) {
	
		.restore(e2)

		if (is.na(e1)) e1 <- 'null()'
		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- paste(gn, '= ', e1, ' ', oper, ' ', gnames(e2))
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)

# raster numeric
methods::setMethod('Ops', signature(e1 = 'GRaster', e2 = 'numeric'),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- 'null()'
		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- paste(gn, '= ', gnames(e1), ' ', oper, ' ', e2)
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)
