#' Arithmetic operations on GRasters
#'
#' @description You can do arithmetic operations on `GRaster`s using normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), and `%/%` (integer division).
#' 
#' @param x `GRaster` to be operated on. Used to transfer properties.
#' @param gn `gnames` of the `GRaster` being operated on
#' @param ex expression for `r.mapcalc`
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @rdname Arithmetic
#' @noRd
.genericArith <- function(x, gn, ex) {

	rgrass::execGRASS('r.mapcalc', expression = ex, flags = c('quiet', 'overwrite'), intern = TRUE)
	makeGRaster(gn, names(x))
	
}

# raster logical
methods::setMethod('Arith', signature(e1 = 'GRaster', e2 = 'logical'),
    function(e1, e2) {
	
		.restore(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- 'null()'
		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste(gn, '= floor(', gnames(e1), '/', e2, ')')
		} else if (oper == '%%') {
			paste0(gn, ' = ', gnames(e1), '%', e2)
		} else {
			paste0(gn, ' = ', gnames(e1), oper, e2)
		}
		.genericArith(x = e1, gn = gn, ex = ex)
		
	}
)

# logical raster
methods::setMethod('Arith', signature(e1 = 'logical', e2 = 'GRaster'),
    function(e1, e2) {
	
		.restore(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- 'null()'
		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste0(gn, ' = floor(', e1, '/', gnames(e2), ')')
		} else if (oper == '%%') {
			paste0(gn, ' = ', e1, '%', gnames(e2))
		} else if (oper == '^') {
			paste0(gn, ' = exp(', e1, ', ', gnames(e2), ')')
		} else {
			paste0(gn, ' = ', e1, oper, gnames(e2))
		}
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)

# raster numeric
methods::setMethod('Arith', signature(e1 = 'GRaster', e2 = 'numeric'),
    function(e1, e2) {
	
		.restore(e1)

		gn <- .makeGname(NULL, 'rast')
		if (is.na(e2)) e2 <- 'null()'

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste0(gn, ' = floor(', gnames(e1), '/', e2, ')')
		} else if (oper == '%%') {
			paste0(gn, ' = ', gnames(e1), '%', e2)
		} else if (oper == '^') {
			paste0(gn, ' = exp(double(', gnames(e1), '), ', e2, ')')
		} else {
			paste0(gn, ' = double(', gnames(e1), ')', oper, e2)
		}
		.genericArith(x = e1, gn = gn, ex = ex)
		
	}
)

# raster integer
methods::setMethod('Arith', signature(e1 = 'GRaster', e2 = 'integer'),
    function(e1, e2) {
	
		.restore(e1)

		gn <- .makeGname(NULL, 'rast')
		if (is.na(e2)) e2 <- 'null()'

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste0(gn, ' = floor(', gnames(e1), '/', e2, ')')
		} else if (oper == '%%') {
			paste0(gn, ' =', gnames(e1), '%', e2)
		} else if (oper == '^') {
			paste0(gn, ' = exp(double(', gnames(e1), '), ', e2, ')')
		} else {
			paste0(gn, ' = double(', gnames(e1), ')', oper, e2)
		}
		.genericArith(x = e1, gn = gn, ex = ex)
		
	}
)

# numeric raster
methods::setMethod('Arith', signature(e1 = 'numeric', e2 = 'GRaster'),
    function(e1, e2) {
	
		.restore(e2)

		gn <- .makeGname(NULL, 'rast')
		if (is.na(e1)) e1 <- 'null()'

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste0(gn, ' = floor(', e1, '/ double(', gnames(e2), '))')
		} else if (oper == '%%') {
			paste0(gn, ' = ', e1, '%', gnames(e2))
		} else if (oper == '^') {
			paste0(gn, ' = exp(', e1, ', double(', gnames(e2), '))')
		} else {
			paste0(gn, ' = ', e1, oper, 'double(', gnames(e2), ')')
		}
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)

# integer raster
methods::setMethod('Arith', signature(e1 = 'integer', e2 = 'GRaster'),
    function(e1, e2) {
	
		.restore(e2)

		gn <- .makeGname(NULL, 'rast')
		if (is.na(e1)) e1 <- 'null()'

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste0(gn, ' = floor(', e1, '/ double(', gnames(e2), '))')
		} else if (oper == '%%') {
			paste0(gn, ' = ', e1, '%', gnames(e2))
		} else if (oper == '^') {
			paste0(gn, ' = exp(double(', e1, '), ', gnames(e2), ')')
		} else {
			paste0(gnames, ' = ', e1, oper, 'double(', gnames(e2), ')')
		}
		.genericArith(x = e2, gn = gn, ex = ex)
		
	}
)

# # missing raster
# methods::setMethod('Arith', signature(e1 = 'missing', e2 = 'GRaster'),
    # function(e1, e2) {
	
		# .restore(e2)

		# gn <- .makeGname(NULL, 'rast')

		# oper <- as.vector(.Generic)[1L]
		# print(oper)
		# ex <- if (oper == '-') {
			# paste0(gn, ' = -1 * ', gnames(e2))
		# } else {
			# paste0(gn, ' = ', gnames(e2))
		# }
		# .genericArith(x = e2, gn = gn, ex = ex)
		
	# }
# )

# raster raster
methods::setMethod('Arith', signature(e1 = 'GRaster', e2 = 'GRaster'),
    function(e1, e2) {
	
		comparable(e1, e2, fail = TRUE)
		.restore(e1)

		gn <- .makeGname(NULL, 'rast')

		oper <- as.vector(.Generic)[1L]
		ex <- if (oper == '%/%') {
			paste0(gn, ' = floor(double(', gnames(e1), ') / double(', gnames(e2), '))')
		} else if (oper == '%%') {
			paste0(gn, ' = ', gnames(e1), '%', gnames(e2))
		} else if (oper == '^') {
			paste0(gn, ' = exp(double(', gnames(e1), '), double(', gnames(e2), '))')
		} else {
			paste0(gn, '= double(', gnames(e1), ')', oper, 'double(', gnames(e2), ')')
		}
		
		.genericArith(x = e1, gn = gn, ex = ex)
		
	}
)
