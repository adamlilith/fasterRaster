#' Arithmetic operations on GRasters
#'
#' @description **`GRaster`s**: You can do arithmetic operations on `GRaster`s and using normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), and `%/%` (integer division). 
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
#' @example man/examples/ex_GRaster_arithmetic_single_layer.r
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
		
		for (i in 1L:nlyr(e1)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			prec <- .getPrec(e1[[i]], oper)
			
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
		
		for (i in 1L:nlyr(e2)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			prec <- .getPrec(e2[[i]], oper)

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

		if (is.na(e2)) {
			e2 <- "null()"
		} else {
			e2 <- formatC(e2, format = "f", drop0trailing = TRUE)
		}
		
		for (i in 1L:nlyr(e1)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			prec <- .getPrec(e1[[i]], oper)

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

		if (is.na(e2)) {
			e2 <- "null()"
		} else {
			e2 <- formatC(e2, format = "f", drop0trailing = TRUE)
		}
		
		for (i in 1L:nlyr(e1)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			prec <- .getPrec(e1[[i]], oper)
			
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

		if (is.na(e1)) {
			e1 <- "null()"
		} else {
			e1 <- formatC(e1, format = "f", drop0trailing = TRUE)
		}
		oper <- as.vector(.Generic)[1L]
		
		for (i in 1L:nlyr(e2)) {
		
			src <- .makeSourceName("math", "rast")
			prec <- .getPrec(e2[[i]], oper)
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

		if (is.na(e1)) {
			e1 <- "null()"
		} else {
			e1 <- formatC(e1, format = "f", drop0trailing = TRUE)
		}
		
		for (i in 1L:nlyr(e2)) {
		
			src <- .makeSourceName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			prec <- .getPrec(e2[[i]], oper)

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
		
		if (nlyr(e1) == nlyr(e2)) {

			for (i in 1L:nlyr(e1)) {

				name <- "layer"
				src <- .makeSourceName("arithmetic", "rast")

				prec1 <- .getPrec(e1[[i]], oper)
				prec2 <- .getPrec(e2[[i]], oper)

				ex <- if (oper == "%/%") {
					paste0(src, " = floor(", prec1, "(", sources(e1)[i], ") / ", prec2, "(", sources(e2)[i], "))")
				} else if (oper == "%%") {
					paste0(src, " = ", prec1, "(", sources(e1)[i], ") % ", prec2, "(", sources(e2)[i], ")")
				} else if (oper == "^") {
					paste0(src, " = exp(", prec1, "(", sources(e1)[i], "), ", prec2, "(", sources(e2)[i], "))")
				} else {
					paste0(src, "= ", prec1, "(", sources(e1)[i], ")  ", oper, " ", prec2, "(", sources(e2)[i], ")")
				}
				
				if (i == 1L) {
					out <- .genericArithRast(name = name, src = src, ex = ex)
				} else {
					this <- .genericArithRast(name = name, src = src, ex = ex)
					out <- c(out, this)
				}
				
			}
			
		} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {

			prec1 <- .getPrec(e1, oper)
			for (i in 1L:nlyr(e2)) {
			
				name <- "layer"
				src <- .makeSourceName("arithmetic", "rast")

				prec2 <- .getPrec(e2[[i]], oper)

				ex <- if (oper == "%/%") {
					paste0(src, " = floor(", prec1, "(", sources(e1), ") / ", prec2, "(", sources(e2)[i], "))")
				} else if (oper == "%%") {
					paste0(src, " = ", prec1, "(", sources(e1), ") % ", prec2, "(", sources(e2)[i], ")")
				} else if (oper == "^") {
					paste0(src, " = exp(", prec1, "(", sources(e1), "), ", prec2, "(", sources(e2)[i], "))")
				} else {
					paste0(src, "= ", prec1, "(", sources(e1), ")  ", oper, " ", prec2, "(", sources(e2)[i], ")")
				}
				
				if (i == 1L) {
					out <- .genericArithRast(name = name, src = src, ex = ex)
				} else {
					this <- .genericArithRast(name = name, src = src, ex = ex)
					out <- c(out, this)
				}
				
			} # next layer
			
		} else if (nlyr(e1) > 1L & nlyr(e2) == 1L) {

			prec2 <- .getPrec(e2, oper)
			for (i in 1L:nlyr(e1)) {
			
				name <- paste0(names(e1)[i], "_", names(e2))
				src <- .makeSourceName("arithmetic", "rast")

				prec1 <- .getPrec(e1[[i]], oper)

				ex <- if (oper == "%/%") {
					paste0(src, " = floor(", prec1, "(", sources(e1)[i], ") / ", prec2, "(", sources(e2), "))")
				} else if (oper == "%%") {
					paste0(src, " = ", prec1, "(", sources(e1)[i], ") % ", prec2, "(", sources(e2), ")")
				} else if (oper == "^") {
					paste0(src, " = exp(", prec1, "(", sources(e1)[i], "), ", prec2, "(", sources(e2), "))")
				} else {
					paste0(src, "= ", prec1, "(", sources(e1)[i], ") ", oper, " ", prec2, "(", sources(e2), ")")
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

	rgrass::execGRASS(cmd = "r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
	.makeGRaster(src, name)
	
}

#' Get function for precision of `GRaster`
#'
#' @param x A `GRaster`.
#' @param oper Character or `NULL`: Operator
#'
#' @returns Either "" (empty) or "double".
#' @noRd
.getPrec <- function(x, oper) {

	if ((is.null(oper) || oper %in% c("/", "%/%")) & any(is.cell(x))) {
		"double"
	} else {
		""
	}

}
