#' @title Mathematical operations on GRasters
#'
#' @description You can apply mathematical operators to `GRaster`s. These include:\cr
#'
#' * Absolute value: `abs()`
#' * Trigonometric functions (assumes values are in radians): `cos()`, `sin()`, `tan()`, `acos()`, `asin()`, `atan()`, `atan2()`
#' * Exponential and logarithmic functions: `exp()`, `log()`, `log1p()` `log2()`, `log10()`
#' * Power functions: `sqrt()`, `x^y`
#' * Rounding: `round()`, `floor()`, `ceiling()`, `trunc()`
#'
#' @param x,y `GRaster`s
#' @param base Numeric: Base of the logarithm.
#' @param digits Numeric: Number of digits to round to. If negative, then rounding is to the nearest power of 10. For example, if `digits = -2`, then the `GRaster` values are rounded to the nearest 100.
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @aliases abs
#' @rdname math
#' @exportMethod abs
setMethod(
	'abs',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = abs(', gnames(x), ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases sin
#' @rdname math
#' @export
#' @exportMethod sin
setMethod(
	'sin',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = sin(double(', gnames(x), ') * 180 / ', pi, ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases cos
#' @rdname math
#' @export
#' @exportMethod cos
setMethod(
	'cos',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = cos(double(', gnames(x), ') * 180 / ', pi, ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases tan
#' @rdname math
#' @export
#' @exportMethod tan
setMethod(
	'tan',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = tan(double(', gnames(x), ') * 180 / ', pi, ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases asin
#' @rdname math
#' @export
#' @exportMethod asin
setMethod(
	'asin',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = asin(double(', gnames(x), ')) * (', pi, ' / 180)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases acos
#' @rdname math
#' @export
#' @exportMethod acos
setMethod(
	'acos',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = acos(double(', gnames(x), ')) * (', pi, ' / 180)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases atan
#' @rdname math
#' @export
#' @exportMethod atan
setMethod(
	'atan',
	signature(x = 'GRaster'),
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = atan(double(', gnames(x), ')) * (', pi, ' / 180)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
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
		gn <- .makeGname(NULL, 'rast')
		# ex <- paste0(gn, ' = atan(double(', gnames(y), ') / double(', gnames(x), '))  * (', pi, ' / 180)')
		ex <- paste0(gn, ' = atan(double(', gnames(x), ') , double(', gnames(y), '))  * (', pi, ' / 180)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases exp
#' @rdname math
#' @export
#' @exportMethod exp
setMethod(
	'exp',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = exp(', gnames(x), ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
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
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = log(', gnames(x), ' + 1)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases log
#' @rdname math
#' @export
#' @exportMethod log
setMethod(
	'log',
	signature = 'GRaster',
	function(x, base = exp(1)) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = log(', gnames(x), ', ', base, ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases log2
#' @rdname math
#' @export
#' @exportMethod log2
setMethod(
	'log2',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = log(', gnames(x), ', 2)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases log10
#' @rdname math
#' @export
#' @exportMethod log10
setMethod(
	'log10',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = log(', gnames(x), ', 10)')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases sqrt
#' @rdname math
#' @export
#' @exportMethod sqrt
setMethod(
	'sqrt',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = sqrt(', gnames(x), ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases round
#' @rdname math
#' @export
#' @exportMethod round
setMethod(
	'round',
	signature = 'GRaster',
	function(x, digits = 0) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')

		roundto <- if (digits == 0) {
			1
		} else if (digits > 0) {
			as.numeric(paste0('0.', paste(rep(0, digits - 1), collapse=''), '1'))
		} else if (digits < 0) {
			as.numeric(paste0('1', paste(rep(0, abs(digits)), collapse='')))
		}

		ex <- paste0(gn, ' = round(', gnames(x), ', ', roundto, ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases floor
#' @rdname math
#' @export
#' @exportMethod floor
setMethod(
	'floor',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = floor(', gnames(x), ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases ceiling
#' @rdname math
#' @export
#' @exportMethod ceiling
setMethod(
	'ceiling',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = ceil(', gnames(x), ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)

#' @aliases trunc
#' @rdname math
#' @export
#' @exportMethod trunc
setMethod(
	'trunc',
	signature = 'GRaster',
	function(x) {
		.restore(x)
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = int(', gnames(x), ')')
		.genericArith(x = x, gn = gn, ex = ex)
	} # EOF
)
