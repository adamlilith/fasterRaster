#" Arithmetic operations on GRasters
#"
#" @description You can do arithmetic operations on `GRaster`s using normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), and `%/%` (integer division).
#" 
#" @param name Character: Name of the new `GRaster`.
#" @param gn `gname`s of the `GRaster` being operated on
#" @param ex expression for `r.mapcalc`
#"
#" @return A `GRaster`.
#"
#" @example man/examples/ex_GRaster_arithmetic.r
#"
#" @rdname Arithmetic
#" @noRd
.genericArith <- function(name, gn, ex) {

	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c("quiet", "overwrite"), intern = TRUE)
	.makeGRaster(gn, name)
	
}

# raster logical
methods::setMethod("Arith", signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.restore(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"
		
		for (i in 1L:nlyr(e1)) {
		
			gn <- .makeGName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(gn, "= floor(double(", .gnames(e1)[i], ") /", e2, ")")
			} else if (oper == "%%") {
				paste0(gn, " = double(", .gnames(e1)[i], ") %", e2)
			} else {
				paste0(gn, " = double(", .gnames(e1)[i], ")", oper, e2)
			}

			name <- names(e1)[i]
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				out <- c(out, .genericArith(name = name, gn = gn, ex = ex))
			}
		
		} # next layer
		
		out
		
	} # EOF
)

# logical raster
methods::setMethod("Arith", signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
		
		for (i in 1L:nlyr(e2)) {
		
			gn <- .makeGName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(gn, "= floor(", e1, "/ double(", .gnames(e2)[i], "))")
			} else if (oper == "%%") {
				paste0(gn, " = ", e1, "% double(", .gnames(e2)[i], ")")
			} else {
				paste0(gn, " = ", e1, oper, " double(", .gnames(e2)[i], ")")
			}

			name <- names(e2)[i]
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				out <- c(out, .genericArith(name = name, gn = gn, ex = ex))
			}
		
		} # next layer
		
		out

	} # EOF
)

# raster numeric
methods::setMethod("Arith", signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- "null()"
		
		for (i in 1L:nlyr(e1)) {
		
			gn <- .makeGName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(gn, "= floor( double(", .gnames(e1)[i], ") /", e2, ")")
			} else if (oper == "%%") {
				paste0(gn, " = double(", .gnames(e1)[i], ") %", e2)
			} else {
				paste0(gn, " = double(", .gnames(e1)[i], ") ", oper, e2)
			}

			name <- names(e1)[i]
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				out <- c(out, .genericArith(name = name, gn = gn, ex = ex))
			}
		
		} # next layer
		
		out
		
	} # EOF
)

# raster integer
methods::setMethod("Arith", signature(e1 = "GRaster", e2 = "integer"),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- "null()"
		
		for (i in 1L:nlyr(e1)) {
		
			gn <- .makeGName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(gn, "= floor( double(", .gnames(e1)[i], ") /", e2, ")")
			} else if (oper == "%%") {
				paste0(gn, " = double(", .gnames(e1)[i], ") %", e2)
			} else {
				paste0(gn, " = double(", .gnames(e1)[i], ") ", oper, e2)
			}

			name <- names(e1)[i]
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				out <- c(out, .genericArith(name = name, gn = gn, ex = ex))
			}
		
		} # next layer
		
		out
		
	} # EOF
)

# numeric raster
methods::setMethod("Arith", signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		if (is.na(e1)) e1 <- "null()"
		oper <- as.vector(.Generic)[1L]
		
		for (i in 1L:nlyr(e2)) {
		
			gn <- .makeGName("math", "rast")
			ex <- if (oper == "%/%") {
				paste(gn, "= floor(", e1, "/ double(", .gnames(e2)[i], "))")
			} else if (oper == "%%") {
				paste0(gn, " = ", e1, " % double(", .gnames(e2)[i], ")")
			} else {
				paste0(gn, " = ", e1, oper, " double(", .gnames(e2)[i], ")")
			}

			name <- names(e2)[i]
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				out <- c(out, .genericArith(name = name, gn = gn, ex = ex))
			}
		
		} # next layer
		
		out
		
	}
)

# integer raster
methods::setMethod("Arith", signature(e1 = "integer", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		if (is.na(e1)) e1 <- "null()"
		
		for (i in 1L:nlyr(e2)) {
		
			gn <- .makeGName("math", "rast")
			oper <- as.vector(.Generic)[1L]
			ex <- if (oper == "%/%") {
				paste(gn, "= floor(", e1, "/ double(", .gnames(e2)[i], "))")
			} else if (oper == "%%") {
				paste0(gn, " = ", e1, "% double(", .gnames(e2)[i], ")")
			} else {
				paste0(gn, " = ", e1, oper, "double(", .gnames(e2)[i], ")")
			}

			name <- names(e2)[i]
			if (i == 1L) {
				out <- .genericArith(name = name, gn = gn, ex = ex)
			} else {
				out <- c(out, .genericArith(name = name, gn = gn, ex = ex))
			}
		
		} # next layer
		
		out
		
	}
)

# # missing raster
# methods::setMethod("Arith", signature(e1 = "missing", e2 = "GRaster"),
    # function(e1, e2) {
	
		# .restore(e2)

		# gn <- .makeGName("math", "rast")

		# oper <- as.vector(.Generic)[1L]
		# print(oper)
		# ex <- if (oper == "-") {
			# paste0(gn, " = -1 * ", .gnames(e2))
		# } else {
			# paste0(gn, " = ", .gnames(e2))
		# }
		# .genericArith(x = e2, gn = gn, ex = ex)
		
	# }
# )

# raster raster
methods::setMethod("Arith", signature(e1 = "GRaster", e2 = "GRaster"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.restore(e1)

		oper <- as.vector(.Generic)[1L]
		
		if (nlyr(e1) == nlyr(e2)) {

			for (i in 1L:nlyr(e1)) {

				name <- paste0(names(e1)[i], "_", names(e2)[i])
				gn <- .makeGName(name, "rast")

				ex <- if (oper == "%/%") {
					paste0(gn, " = floor(double(", .gnames(e1)[i], ") / double(", .gnames(e2)[i], "))")
				} else if (oper == "%%") {
					paste0(gn, " = double(", .gnames(e1)[i], ") % double(", .gnames(e2)[i], ")")
				} else if (oper == "^") {
					paste0(gn, " = exp(double(", .gnames(e1)[i], "), double(", .gnames(e2)[i], "))")
				} else {
					paste0(gn, "= double(", .gnames(e1)[i], ")", oper, "double(", .gnames(e2)[i], ")")
				}
				
				if (i == 1L) {
					out <- .genericArith(name = name, gn = gn, ex = ex)
				} else {
					this <- .genericArith(name = name, gn = gn, ex = ex)
					out <- c(out, this)
				}
				
			}
			
		} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {

			for (i in 1L:nlyr(e2)) {
			
				name <- paste0(names(e1), "_", names(e2)[i])
				gn <- .makeGName(name, "rast")

				ex <- if (oper == "%/%") {
					paste0(gn, " = floor(double(", .gnames(e1), ") / double(", .gnames(e2)[i], "))")
				} else if (oper == "%%") {
					paste0(gn, " = double(", .gnames(e1), ") % double(", .gnames(e2)[i], ")")
				} else if (oper == "^") {
					paste0(gn, " = exp(double(", .gnames(e1), "), double(", .gnames(e2)[i], "))")
				} else {
					paste0(gn, "= double(", .gnames(e1), ")", oper, "double(", .gnames(e2)[i], ")")
				}
				
				if (i == 1L) {
					out <- .genericArith(name = name, gn = gn, ex = ex)
				} else {
					this <- .genericArith(name = name, gn = gn, ex = ex)
					out <- c(out, this)
				}
				
			} # next layer
			
		} else if (nlyr(e1) > 1L & nlyr(e2) == 1L) {

			for (i in 1L:nlyr(e1)) {
			
				name <- paste0(names(e1)[i], "_", names(e2))
				gn <- .makeGName(name, "rast")

				ex <- if (oper == "%/%") {
					paste0(gn, " = floor(double(", .gnames(e1)[i], ") / double(", .gnames(e2), "))")
				} else if (oper == "%%") {
					paste0(gn, " = double(", .gnames(e1)[i], ") % double(", .gnames(e2), ")")
				} else if (oper == "^") {
					paste0(gn, " = exp(double(", .gnames(e1)[i], "), double(", .gnames(e2), "))")
				} else {
					paste0(gn, "= double(", .gnames(e1)[i], ")", oper, "double(", .gnames(e2), ")")
				}
				
				if (i == 1L) {
					out <- .genericArith(name = name, gn = gn, ex = ex)
				} else {
					this <- .genericArith(name = name, gn = gn, ex = ex)
					out <- c(out, this)
				}
				
			} # next layer
		
		} else {
			stop("Both rasters must have the same number of layers, or at least one raster must have a single layer.")
		}
		out
			
	} # EOF
)
