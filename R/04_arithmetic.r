#' Arithmetic operations on GRasters
#'
#' @description **`GRaster`s**: You can do arithmetic operations on `GRaster`s and using normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), and `%/%` (integer division). 
#'
#' Note that for `GRaster`s, the precision of the result will be determined by the `rasterPrecision` option, which can be set using [faster()]. The default is `"double"`, which is precise to about the 15th to 17th decimal place. This be reduced to the 6th to 9th decimal place by setting this to `"float"`. This reduces accuracy but also reduces memory requirements.
#'
#' **`GVector`s**: You can also do arithmetic operations on `GVector`s:\cr\cr
#' `+` operator: Same as [union()]\cr
#' `-` operator: Same as [erase()]\cr
#' `*` operator: Same as [intersect()]\cr
#' `/` operator: Same as [xor()]\cr
#' 
#' @param e1,e2 `GRaster`s, `numeric`s, `integer`s, or `logical`s.
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.locationRestore(e1)
  		.region(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"
		prec <- faster("rasterPrecision")
		
		for (i in 1L:nlyr(e1)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(src, "= floor(", prec, "(", sources(e1)[i], ") /", e2, ")")
			} else if (oper == "%%") {
				paste0(src, " = ", prec, "(", sources(e1)[i], ") %", e2)
			} else {
				paste0(src, " = ", prec, "(", sources(e1)[i], ")  ", oper, e2)
			}

			name <- "layer"
			if (i == 1L) {
				out <- .genericArithRast(name = name, src = src, ex = ex)
			} else {
				out <- c(out, .genericArithRast(name = name, src = src, ex = ex))
			}
		
		} # next layer
		
		out
		
	} # EOF
)

# logical raster
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.locationRestore(e2)
  		.region(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
  		prec <- faster("rasterPrecision")
		
		for (i in 1L:nlyr(e2)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(src, "= floor(", e1, "/ ", prec, "(", sources(e2)[i], "))")
			} else if (oper == "%%") {
				paste0(src, " = ", e1, "% ", prec, "(", sources(e2)[i], ")")
			} else {
				paste0(src, " = ", e1, oper, " ", prec, "(", sources(e2)[i], ")")
			}

			name <- "layer"
			if (i == 1L) {
				out <- .genericArithRast(name = name, src = src, ex = ex)
			} else {
				out <- c(out, .genericArithRast(name = name, src = src, ex = ex))
			}
		
		} # next layer
		
		out

	} # EOF
)

# raster numeric
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.locationRestore(e1)
		.region(e1)

		if (is.na(e2)) e2 <- "null()"
  		prec <- faster("rasterPrecision")
		
		for (i in 1L:nlyr(e1)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(src, "= floor(", prec, "(", sources(e1)[i], ") /", e2, ")")
			} else if (oper == "%%") {
				paste0(src, " = ", prec, "(", sources(e1)[i], ") %", e2)
			} else {
				paste0(src, " = ", prec, "(", sources(e1)[i], ")  ", oper, " ", e2)
			}

			name <- "layer"
			if (i == 1L) {
				out <- .genericArithRast(name = name, src = src, ex = ex)
			} else {
				out <- c(out, .genericArithRast(name = name, src = src, ex = ex))
			}
		
		} # next layer
		
		out
		
	} # EOF
)

# raster integer
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "integer"),
    function(e1, e2) {
	
		.locationRestore(e1)
  		.region(e1)

		if (is.na(e2)) e2 <- "null()"
  		prec <- faster("rasterPrecision")
		
		for (i in 1L:nlyr(e1)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(src, "= floor(", prec, "(", sources(e1)[i], ") /", e2, ")")
			} else if (oper == "%%") {
				paste0(src, " = ", prec, "(", sources(e1)[i], ") %", e2)
			} else {
				paste0(src, " = ", prec, "(", sources(e1)[i], ")  ", oper, e2)
			}

			name <- "layer"
			if (i == 1L) {
				out <- .genericArithRast(name = name, src = src, ex = ex)
			} else {
				out <- c(out, .genericArithRast(name = name, src = src, ex = ex))
			}
		
		} # next layer
		
		out
		
	} # EOF
)

# numeric raster
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.locationRestore(e2)
  		.region(e2)

		if (is.na(e1)) e1 <- "null()"
		oper <- as.vector(.Generic)[1L]
  		prec <- faster("rasterPrecision")
		
		for (i in 1L:nlyr(e2)) {
		
			src <- .makeSourceName("math", "rast")
			ex <- if (oper == "%/%") {
				paste(src, "= floor(", e1, "/ ", prec, "(", sources(e2)[i], "))")
			} else if (oper == "%%") {
				paste0(src, " = ", e1, " % ", prec, "(", sources(e2)[i], ")")
			} else {
				paste0(src, " = ", e1, oper, " ", prec, "(", sources(e2)[i], ")")
			}

			name <- "layer"
			if (i == 1L) {
				out <- .genericArithRast(name = name, src = src, ex = ex)
			} else {
				out <- c(out, .genericArithRast(name = name, src = src, ex = ex))
			}
		
		} # next layer
		
		out
		
	}
)

# integer raster
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "integer", e2 = "GRaster"),
    function(e1, e2) {
	
		.locationRestore(e2)
		.region(e2)

		if (is.na(e1)) e1 <- "null()"
  		prec <- faster("rasterPrecision")
		
		for (i in 1L:nlyr(e2)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(src, "= floor(", e1, " / ", prec, "(", sources(e2)[i], "))")
			} else if (oper == "%%") {
				paste0(src, " = ", e1, "% ", prec, "(", sources(e2)[i], ")")
			} else {
				paste0(src, " = ", e1, oper, " ", prec, "(", sources(e2)[i], ")")
			}

   			name <- "layer"
			if (i == 1L) {
				out <- .genericArithRast(name = name, src = src, ex = ex)
			} else {
				out <- c(out, .genericArithRast(name = name, src = src, ex = ex))
			}
		
		} # next layer
		
		out
		
	}
)

# # missing raster
# methods::setMethod(
# 	f = "Arith",
# 	signature(e1 = "missing", e2 = "GRaster"),
#     function(e1, e2) {

# 	.locationRestore(e2)
# 	.region(e2)

# 	src <- .makeSourceName("math", "rast")

# 	oper <- as.vector(.Generic)[1L]
# 	print(oper)
# 	if (oper == "-") {
		
# 		ex <- paste0(src, " = -1 * ", sources(e2))
# 		out <- .genericArithRast(x = e2, src = src, ex = ex)

# 	} else {
# 		out <- e2
# 	}

# 	out
		
# 	} # EOF
# )

# raster raster
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "GRaster"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.locationRestore(e1)
		.region(e1)

		oper <- as.vector(.Generic)[1L]
  		prec <- faster("rasterPrecision")
		
		if (nlyr(e1) == nlyr(e2)) {

			for (i in 1L:nlyr(e1)) {

				name <- "layer"
				src <- .makeSourceName(name, "rast")

				ex <- if (oper == "%/%") {
					paste0(src, " = floor(", prec, "(", sources(e1)[i], ") / ", prec, "(", sources(e2)[i], "))")
				} else if (oper == "%%") {
					paste0(src, " = ", prec, "(", sources(e1)[i], ") % ", prec, "(", sources(e2)[i], ")")
				} else if (oper == "^") {
					paste0(src, " = exp(", prec, "(", sources(e1)[i], "), ", prec, "(", sources(e2)[i], "))")
				} else {
					paste0(src, "= ", prec, "(", sources(e1)[i], ")  ", oper, " ", prec, "(", sources(e2)[i], ")")
				}
				
				if (i == 1L) {
					out <- .genericArithRast(name = name, src = src, ex = ex)
				} else {
					this <- .genericArithRast(name = name, src = src, ex = ex)
					out <- c(out, this)
				}
				
			}
			
		} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {

			for (i in 1L:nlyr(e2)) {
			
				name <- "layer"
				src <- .makeSourceName(name, "rast")

				ex <- if (oper == "%/%") {
					paste0(src, " = floor(", prec, "(", sources(e1), ") / ", prec, "(", sources(e2)[i], "))")
				} else if (oper == "%%") {
					paste0(src, " = ", prec, "(", sources(e1), ") % ", prec, "(", sources(e2)[i], ")")
				} else if (oper == "^") {
					paste0(src, " = exp(", prec, "(", sources(e1), "), ", prec, "(", sources(e2)[i], "))")
				} else {
					paste0(src, "= ", prec, "(", sources(e1), ")  ", oper, " ", prec, "(", sources(e2)[i], ")")
				}
				
				if (i == 1L) {
					out <- .genericArithRast(name = name, src = src, ex = ex)
				} else {
					this <- .genericArithRast(name = name, src = src, ex = ex)
					out <- c(out, this)
				}
				
			} # next layer
			
		} else if (nlyr(e1) > 1L & nlyr(e2) == 1L) {

			for (i in 1L:nlyr(e1)) {
			
				name <- paste0(names(e1)[i], "_", names(e2))
				src <- .makeSourceName(name, "rast")

				ex <- if (oper == "%/%") {
					paste0(src, " = floor(", prec, "(", sources(e1)[i], ") / ", prec, "(", sources(e2), "))")
				} else if (oper == "%%") {
					paste0(src, " = ", prec, "(", sources(e1)[i], ") % ", prec, "(", sources(e2), ")")
				} else if (oper == "^") {
					paste0(src, " = exp(", prec, "(", sources(e1)[i], "), ", prec, "(", sources(e2), "))")
				} else {
					paste0(src, "= ", prec, "(", sources(e1)[i], ") ", oper, " ", prec, "(", sources(e2), ")")
				}
				
				if (i == 1L) {
					out <- .genericArithRast(name = name, src = src, ex = ex)
				} else {
					this <- .genericArithRast(name = name, src = src, ex = ex)
					out <- c(out, this)
				}
				
			} # next layer
		
		} else {
			stop("Both rasters must have the same number of layers, or at least one raster must have a single layer.")
		}
		out
			
	} # EOF
)

# vector vector
#' @aliases Arith
#' @rdname Arithmetic
#' @exportMethod Arith
methods::setMethod(
	f = "Arith",
	signature(e1 = "GVector", e2 = "GVector"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.locationRestore(e1)

		oper <- as.vector(.Generic)[1L]

		if (oper == "+") {
			out <- union(e1, e2)
		} else if (oper == "-") {
			out <- erase(e1, e2)
		} else if (oper == "*") {
			out <- intersect(e1, e2)
		} else if (oper == "/") {
			out <- xor(e1, e2)
		} else {
			stop("Only the +, -, *, and / operators are defined for GVectors.")
		}
		
		out
			
	} # EOF
)

#' @noRd
.genericArithRast <- function(name, src, ex) {

	args <- list(
		cmd = "r.mapcalc",
		expression = ex,
		flags = c(.quiet(), "overwrite")
	)
	do.call(rgrass::execGRASS, args = args)
	.makeGRaster(src, name)
	
}


