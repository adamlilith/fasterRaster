#' @title Mathematical operations on single GRasters
#'
#' @description You can apply mathematical operators to `GRaster`s. These include:\cr
#'
#' * Absolute value: `abs()`
#' * Trigonometric functions (assumes values are in radians): `cos()`, `sin()`, `tan()`, `acos()`, `asin()`, `atan()`, `atan2()`
#' * Exponential and logarithmic functions: `exp()`, `log()` (natural log), `log1p()`, `log2()`, `log10()`
#' * Power functions: `sqrt()`, `x^y`
#' * Rounding: `round()`, `floor()`, `ceiling()`, `trunc()`
#'
#' @param x,y `GRaster`s
#' @param base Numeric: Base of the logarithm.
#' @param digits Numeric: Number of digits to round to. If negative, then rounding is to the nearest power of 10. For example, if `digits = -2`, then the `GRaster` values are rounded to the nearest 100.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @aliases abs
#' @rdname math
#' @exportMethod abs
setMethod(
	'abs',
	signature(x = 'GRaster'),
	function(x) .genericFx('abs', x)
)

#' @aliases sin
#' @rdname math
#' @export
#' @exportMethod sin
setMethod(
	'sin',
	signature(x = 'GRaster'),
	function(x) .genericTrig('sin', x)
)

#' @aliases cos
#' @rdname math
#' @export
#' @exportMethod cos
setMethod(
	'cos',
	signature(x = 'GRaster'),
	function(x) .genericTrig('cos', x)
)

#' @aliases tan
#' @rdname math
#' @export
#' @exportMethod tan
setMethod(
	'tan',
	signature(x = 'GRaster'),
	function(x) .genericTrig('tan', x)
)

#' @aliases asin
#' @rdname math
#' @export
#' @exportMethod asin
setMethod(
	'asin',
	signature(x = 'GRaster'),
	function(x) .genericArcTrig('asin', x)
)

#' @aliases acos
#' @rdname math
#' @export
#' @exportMethod acos
setMethod(
	'acos',
	signature(x = 'GRaster'),
	function(x) .genericArcTrig('acos', x)
)

#' @aliases atan
#' @rdname math
#' @export
#' @exportMethod atan
setMethod(
	'atan',
	signature(x = 'GRaster'),
	function(x) .genericArcTrig('atan', x)
)

#' @aliases atan2
#' @rdname math
#' @export
#' @exportMethod atan2
setMethod(
	'atan2',
	signature(y = 'GRaster', x = 'GRaster'),
	function(y, x) {
	
		comparable(y, x)
		.restore(x)
		
		ny <- nlyr(y)
		nx <- nlyr(x)
		
		if (ny == 1L & nx > 1L) {
			y <- y[[rep(1L, nx)]]
		} else if (ny > 1L & nx == 1L) {
			x <- x[[rep(1L, ny)]]
		} else if (ny != nx) {
			stop('Rasters must have the same number of layers, or at least one raster must have one layer.')
		}
		
		n <- max(ny, nx)
		
		for (i in seq_len(n)) {
			
			name <- paste0(names(y)[i], '_', names(x)[i])
			gn <- .makeGname(name, 'rast')
			ex <- paste0(gn, ' = atan(double(', gnames(x)[i], ') , double(', gnames(y)[i], '))  * (', pi, ' / 180)')
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				this <- .genericArith(name = name, gn = gn, ex = ex)
				out <- c(out, this)
			}
		}
		out
		
	} # EOF
)

#' @aliases exp
#' @rdname math
#' @export
#' @exportMethod exp
setMethod(
	'exp',
	signature = 'GRaster',
	function(x) .genericFx('exp', x)
)

#' @aliases log1p
#' @rdname math
#' @export
#' @exportMethod log1p
setMethod(
	'log1p',
	signature = 'GRaster',
	function(x) {

		.restore(x)
		for (i in 1L:nlyr(x)) {

			name <- names(x)[i]
			gn <- .makeGname(name, 'rast')
			ex <- paste0(gn, ' = log(', gnames(x)[i], ' + 1)')
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				this <- .genericArith(name = name, gn = gn, ex = ex)
				out <- c(out, this)
			}
		}
		out
		
	} # EOF
)

#' @aliases log
#' @rdname math
#' @export
#' @exportMethod log
setMethod(
	'log',
	signature = 'GRaster',
	function(x, base = exp(1)) .genericFx2('log', x, base)
)

#' @aliases log2
#' @rdname math
#' @export
#' @exportMethod log2
setMethod(
	'log2',
	signature = 'GRaster',
	function(x) .genericFx2('log', x, 2)
)

#' @aliases log10
#' @rdname math
#' @exportMethod log10
setMethod(
	'log10',
	signature = 'GRaster',
	function(x) .genericFx2('log', x, 10)
)

#' @aliases sqrt
#' @rdname math
#' @exportMethod sqrt
setMethod(
	'sqrt',
	signature = 'GRaster',
	function(x) .genericFx('sqrt', x)
)

#' @aliases round
#' @rdname math
#' @export
#' @exportMethod round
setMethod(
	'round',
	signature = 'GRaster',
	function(x, digits = 0) {

		roundto <- if (digits == 0) {
			1
		} else if (digits > 0) {
			as.numeric(paste0('0.', paste(rep(0, digits - 1), collapse=''), '1'))
		} else if (digits < 0) {
			as.numeric(paste0('1', paste(rep(0, abs(digits)), collapse='')))
		}

		.genericFx2('round', x, roundto)
		
	} # EOF
)

#' @aliases floor
#' @rdname math
#' @export
#' @exportMethod floor
setMethod(
	'floor',
	signature = 'GRaster',
	function(x) .genericFx('floor', x)
)

#' @aliases ceiling
#' @rdname math
#' @export
#' @exportMethod ceiling
setMethod(
	'ceiling',
	signature = 'GRaster',
	function(x) .genericFx('ceil', x)
)

#' @aliases trunc
#' @rdname math
#' @export
#' @exportMethod trunc
setMethod(
	'trunc',
	signature = 'GRaster',
	function(x) .genericFx('int', x)
)

#' Generic trigonometry function
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @noRd
.genericTrig <- function(fx, x) {

	.restore(x)
	for (i in 1L:nlyr(x)) {
	
		name <- names(x)[i]

		gn <- .makeGname(name, 'rast')
		ex <- paste0(gn, ' = ', fx, '(', gnames(x)[i], ' * 180 / ', pi, ')')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'), intern=TRUE)
		this <- makeGRaster(gn, name)
		if (i == 1L) {
			out <- this
		} else {
			out <- c(out, this)
		}
		
	}
	out

}

#' Generic "arc"-trigonometry function
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @noRd
.genericArcTrig <- function(fx, x) {

	.restore(x)
	for (i in 1L:nlyr(x)) {
	
		name <- names(x)[i]
		gn <- .makeGname(name, 'rast')
		ex <- paste0(gn, ' = ', fx, '(', gnames(x)[i], ') * ', pi, ' / 180')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'), intern=TRUE)
		this <- makeGRaster(gn, name)
		if (i == 1L) {
			out <- this
		} else {
			out <- c(out, this)
		}
	}
	out

}

#' Generic function with one input (the GRaster)
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @noRd
.genericFx <- function(fx, x) {

	.restore(x)
	for (i in 1L:nlyr(x)) {
	
		name <- names(x)[i]
		gn <- .makeGname(name, 'rast')
		ex <- paste0(gn, ' = ', fx, '(double(', gnames(x)[i], '))')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'), intern=TRUE)
		this <- makeGRaster(gn, name)
		if (i == 1L) {
			out <- this
		} else {
			out <- c(out, this)
		}
	}
	out

}

#' Generic function with two inputs (GRaster and a numeric)
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @param y		A numeric.
#' @noRd
.genericFx2 <- function(fx, x, y) {

	.restore(x)
	for (i in 1L:nlyr(x)) {
	
		name <- names(x)[i]
		gn <- .makeGname(name, 'rast')
		ex <- paste0(gn, ' = ', fx, '(double(', gnames(x)[i], '), ', y, ')')
		if (i == 1L) {
			out <- .genericArith(name = fx, gn = gn, ex = ex)
		} else {
			this <- .genericArith(name = fx, gn = gn, ex = ex)
			out <- c(out, this)
		}
	}
	out

}
