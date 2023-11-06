#' Arithmetic operations on GRasters
#'
#' @description **`GRaster`s**: You can do arithmetic operations on `GRaster`s and using normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), and `%/%` (integer division). 
#'
#' Note that for `GRaster`s, the precision of the result will be determined by the `rasterPrecision` option, which can be set using [setFastOptions()]. The default is `"float"`, which is precise to about the 7th decimal place. This be increased to about the 15th decimal place by setting this to `"double"`, though it can substantially increase the size of the raster output in memory and when saved to disk.
#'
#' **`GVector`s**: You can also do arithmetic operations on `GVector`s using the `+` and `-` operators. The `+ operator is the same as [union()] and the `-` operator the same as [not()].
#' 
#' @param name Character: Name of the new `GRaster`.
#' @param src `sources` of the `GRaster` being operated on
#' @param ex expression for `r.mapcalc`
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @rdname Arithmetic
#' @noRd
.genericArithRast <- function(name, src, ex) {

	args <- list(
		cmd = "r.mapcalc",
		expression = ex,
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)
	do.call(rgrass::execGRASS, args = args)
	.makeGRaster(src, name)
	
}

# raster math
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.restore(e1)
  		region(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"
		prec <- getFastOptions("rasterPrecision")
		
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
methods::setMethod(
	f = "Arith",
	signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)
  		region(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
  		prec <- getFastOptions("rasterPrecision")
		
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
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.restore(e1)
		region(e1)

		if (is.na(e2)) e2 <- "null()"
  		prec <- getFastOptions("rasterPrecision")
		
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
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "integer"),
    function(e1, e2) {
	
		.restore(e1)
  		region(e1)

		if (is.na(e2)) e2 <- "null()"
  		prec <- getFastOptions("rasterPrecision")
		
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
methods::setMethod(
	f = "Arith",
	signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)
  		region(e2)

		if (is.na(e1)) e1 <- "null()"
		oper <- as.vector(.Generic)[1L]
  		prec <- getFastOptions("rasterPrecision")
		
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
methods::setMethod(
	f = "Arith",
	signature(e1 = "integer", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)
		region(e2)

		if (is.na(e1)) e1 <- "null()"
  		prec <- getFastOptions("rasterPrecision")
		
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
	# f = "Arith",
	# signature(e1 = "missing", e2 = "GRaster"),
    # function(e1, e2) {
	
		# .restore(e2)
		# region(e2)

		# src <- .makeSourceName("math", "rast")

		# oper <- as.vector(.Generic)[1L]
		# print(oper)
		# ex <- if (oper == "-") {
			# paste0(src, " = -1 * ", sources(e2))
		# } else {
			# paste0(src, " = ", sources(e2))
		# }
		# .genericArithRast(x = e2, src = src, ex = ex)
		
	# }
# )

# raster raster
methods::setMethod(
	f = "Arith",
	signature(e1 = "GRaster", e2 = "GRaster"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.restore(e1)
		region(e1)

		oper <- as.vector(.Generic)[1L]
  		prec <- getFastOptions("rasterPrecision")
		
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
methods::setMethod(
	f = "Arith",
	signature(e1 = "GVector", e2 = "GVector"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.restore(e1)

		oper <- as.vector(.Generic)[1L]

		if (oper == "+") {
			out <- union(e1, e2)
		} else if (oper == "-") {
			out <- not(e1, e2)
		} else {
			stop("Only the + and - operators are defined for GVectors.")
		}
		
		out
			
	} # EOF
)
