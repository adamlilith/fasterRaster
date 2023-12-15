#' @title Mathematical operations on each layer of a GRasters
#'
#' @description You can apply mathematical functions to each layer of a `GRaster`. These include:\cr
#'
#' * `NA`s:
#'      * `is.na()`
#'      * `not.na()`
#' * Absolute value: `abs()`
#' * Trigonometric functions (assumes values are in radians):
#'      * `cos()`
#'      * `sin()`
#'      * `tan()`
#'      * `acos()`
#'      * `asin()`
#'      * `atan()`
#'      * `atan2()`
#' * Exponential and logarithmic functions:
#'      * `exp()`
#'      * `log()` (natural log)
#'      * `ln()` (also natural log)
#'      * `log1p()` (same as `log(x + 1)`)
#'      * `log2()` (log, base 2)
#'      * `log10()` (log, base 10)
#' * Power functions:
#'      * `sqrt()`
#'      * `x^y`
#' * Rounding:
#'      * `round()`
#'      * `floor()` (round down)
#'      * `ceiling()` (round up)
#'      * `trunc()` (remove decimal portion)
#'
#' @param x,y `GRaster`s.
#'
#' @param falseNA Logical (function `not.na()`): If `FALSE` (default), non-`NA` cells will be converted to 1, and `NA` cells to 0. If `TRUE`, non-`NA` cells will be converted to  and `NA` cells will stay as `NA`.
#'
#' @param base Numeric: Base of the logarithm.
#'
#' @param digits Numeric: Number of digits to round to. If negative, then rounding is to the nearest positive power of 10. For example, if `digits = -2`, then the `GRaster` values are rounded to the nearest 100.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @aliases is.na
#' @rdname math
#' @exportMethod is.na
setMethod(
	"is.na",
	signature(x = "GRaster"),
	function(x) {
	
	.locationRestore(x)
	.region(x)

	srcs <- .makeSourceName("isNA", "raster", nlyr(x))
	for (i in seq_len(nlyr(x))) {
	
		ex <- paste0(srcs[i], " = int(if(isnull(", sources(x)[i], "), 1, 0))")
		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	
	}
	.makeGRaster(srcs, names(x))

	} # EOF
)

#' @aliases not.na
#' @rdname math
#' @exportMethod not.na
methods::setMethod(
	f = "not.na",
	signature(x = "GRaster"),
	function(x, falseNA = FALSE) {
	
	.locationRestore(x)
	.region(x)

	srcs <- .makeSourceName("notNA", "raster", nlyr(x))
	for (i in seq_len(nlyr(x))) {
	
		ex <- if (falseNA) {
			paste0(srcs[i], " = int(if(isnull(", sources(x)[i], "), 1, null()))")
		} else {
   			paste0(srcs[i], " = int(if(isnull(", sources(x)[i], "), 1, 0))")
		}
		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	
	}
	.makeGRaster(srcs, names(x))

	}
)

#' @aliases abs
#' @rdname math
#' @exportMethod abs
setMethod(
	"abs",
	signature(x = "GRaster"),
	function(x) .genericRastFx("abs", x)
)

#' @aliases sin
#' @rdname math
#' @export
#' @exportMethod sin
setMethod(
	"sin",
	signature(x = "GRaster"),
	function(x) .genericTrig("sin", x)
)

#' @aliases cos
#' @rdname math
#' @export
#' @exportMethod cos
setMethod(
	"cos",
	signature(x = "GRaster"),
	function(x) .genericTrig("cos", x)
)

#' @aliases tan
#' @rdname math
#' @export
#' @exportMethod tan
setMethod(
	"tan",
	signature(x = "GRaster"),
	function(x) .genericTrig("tan", x)
)

#' @aliases asin
#' @rdname math
#' @export
#' @exportMethod asin
setMethod(
	"asin",
	signature(x = "GRaster"),
	function(x) .genericArcTrig("asin", x)
)

#' @aliases acos
#' @rdname math
#' @export
#' @exportMethod acos
setMethod(
	"acos",
	signature(x = "GRaster"),
	function(x) .genericArcTrig("acos", x)
)

#' @aliases atan
#' @rdname math
#' @export
#' @exportMethod atan
setMethod(
	"atan",
	signature(x = "GRaster"),
	function(x) .genericArcTrig("atan", x)
)

#' @aliases atan2
#' @rdname math
#' @export
#' @exportMethod atan2
setMethod(
	"atan2",
	signature(y = "GRaster", x = "GRaster"),
	function(y, x) {
	
		compareGeom(y, x)
		.locationRestore(x)
  		.region(x)
		
		ny <- nlyr(y)
		nx <- nlyr(x)
		
		if (ny == 1L & nx > 1L) {
			y <- y[[rep(1L, nx)]]
		} else if (ny > 1L & nx == 1L) {
			x <- x[[rep(1L, ny)]]
		} else if (ny != nx) {
			stop("Rasters must have the same number of layers, or at least one raster must have one layer.")
		}
		
		n <- max(ny, nx)
  		
		prec <- faster("rasterPrecision")

		for (i in seq_len(n)) {
			
			name <- paste0(names(y)[i], "_", names(x)[i])
			src <- .makeSourceName(name, "rast")
			ex <- paste0(src, " = atan(", prec, "(", sources(x)[i], ") , double(", sources(y)[i], "))  * (", pi, " / 180)")
			this <- .genericArithRast(name = name, src = src, ex = ex)
			if (i == 1L) {
				out <- this
			} else {
				out <- c(out, this)
			}
		}
		out
		
	} # EOF
)

#' @aliases exp
#' @rdname math
#' @exportMethod exp
setMethod(
	"exp",
	signature = "GRaster",
	function(x) .genericRastFx("exp", x)
)

#' @aliases log1p
#' @rdname math
#' @exportMethod log1p
setMethod(
	"log1p",
	signature = "GRaster",
	function(x) {

		.locationRestore(x)
		.region(x)

		srcs <- .makeSourceName(names(x), "rast", nlyr(x))
		for (i in seq_len(nlyr(x))) {

			ex <- paste0(srcs[i], " = log(", sources(x)[i], " + 1)")
			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)

		}
		.makeGRaster(srcs, names(x))
		
	} # EOF
)

#' @aliases log
#' @rdname math
#' @export
#' @exportMethod log
setMethod(
	"log",
	signature = "GRaster",
	function(x, base = exp(1)) .genericFx2("log", x, base)
)

#' @aliases log ln
#' @rdname math
#' @exportMethod ln
setMethod(
	"ln",
	signature = "GRaster",
 	function(x) .genericFx2("log", x, y = exp(1))
)

#' @aliases log2
#' @rdname math
#' @export
#' @exportMethod log2
setMethod(
	"log2",
	signature = "GRaster",
	function(x) .genericFx2("log", x, 2)
)

#' @aliases log10
#' @rdname math
#' @exportMethod log10
setMethod(
	"log10",
	signature = "GRaster",
	function(x) .genericFx2("log", x, 10)
)

#' @aliases sqrt
#' @rdname math
#' @exportMethod sqrt
setMethod(
	"sqrt",
	signature = "GRaster",
	function(x) .genericRastFx("sqrt", x)
)

#' @aliases round
#' @rdname math
#' @export
#' @exportMethod round
setMethod(
	"round",
	signature = "GRaster",
	function(x, digits = 0) {

		roundto <- if (digits == 0) {
			1
		} else if (digits > 0) {
			as.numeric(paste0("0.", paste(rep(0, digits - 1), collapse=""), "1"))
		} else if (digits < 0) {
			as.numeric(paste0("1", paste(rep(0, abs(digits)), collapse="")))
		}

		.genericFx2("round", x, roundto)
		
	} # EOF
)

#' @aliases floor
#' @rdname math
#' @export
#' @exportMethod floor
setMethod(
	"floor",
	signature = "GRaster",
	function(x) .genericRastFx("floor", x)
)

#' @aliases ceiling
#' @rdname math
#' @export
#' @exportMethod ceiling
setMethod(
	"ceiling",
	signature = "GRaster",
	function(x) .genericRastFx("ceil", x)
)

#' @aliases trunc
#' @rdname math
#' @export
#' @exportMethod trunc
setMethod(
	"trunc",
	signature = "GRaster",
	function(x) .genericRastFx("int", x)
)

#' Generic trigonometry function
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @noRd
.genericTrig <- function(fx, x) {

	.locationRestore(x)
	.region(x)
	
	precision <- faster("rasterPrecision")

	srcs <- .makeSourceName("r.mapcalc", "rast", nlyr(x))
	for (i in 1L:nlyr(x)) {
	
  		ex <- paste0(srcs[i], " = ", fx, "(", precision, "(", sources(x)[i], ") * 180 / ", pi, ")")
		rgrass::execGRASS(
			"r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
		
	}
	.makeGRaster(srcs, names(x))

}

#' Generic "arc"-trigonometry function
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @noRd
.genericArcTrig <- function(fx, x) {

	.locationRestore(x)
	.region(x)

 	precision <- faster("rasterPrecision")

	srcs <- .makeSourceName(names(x), "rast", nlyr(x))
	for (i in 1L:nlyr(x)) {
	
		ex <- paste0(srcs[i], " = ", fx, "(", precision, "(", sources(x)[i], ") * ", pi, " / 180)")
		rgrass::execGRASS(
			"r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	}

	.makeGRaster(srcs, names(x))

}

#' Generic function with one input (the GRaster)
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @noRd
.genericRastFx <- function(fx, x) {

	.locationRestore(x)
	.region(x)
	
	nLayers <- nlyr(x)
	srcs <- .makeSourceName(names(x), "raster", nLayers)
	
	prec <- faster("rasterPrecision")

	for (i in seq_len(nLayers)) {
	
		ex <- paste0(srcs[i], " = ", fx, "(", prec, "(", sources(x)[i], "))")

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
		
	}
	.makeGRaster(srcs, names(x))

}

#' Generic function with two inputs (GRaster and a numeric)
#' @param fx	Character: Name of the function in **GRASS** module `r.series`.
#' @param x		A `GRaster`.
#' @param y		A numeric.
#' @noRd
.genericFx2 <- function(fx, x, y) {

	.locationRestore(x)
	.region(x)

 	prec <- faster("rasterPrecision")

	nLayers <- nlyr(x)
	srcs <- .makeSourceName(names(x), "raster", nLayers)
	
	for (i in seq_len(nLayers)) {
	
		ex <- paste0(srcs[i], " = ", fx, "(", prec, "(", sources(x)[i], "), ", y, ")")

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	}
	.makeGRaster(srcs, fx)

}
