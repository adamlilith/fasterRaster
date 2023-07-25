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
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "GRaster"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.restore(e1)

		if (nlyr(e1) > 1L & nlyr(e2) == 1L) {
			e2 <- e2[[rep(1L, nlyr(e1))]]
		} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {
			e1 <- e1[[rep(1L, nlyr(e2))]]
		} else if (nlyr(e1) != nlyr(e2)) {
			stop("Rasters must have the same number of layers, or at least one raster must have a single layer.")
		}

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- paste0(names(e1)[i], "_", names(e2)[i])
			gn <- .makeGName(name, "rast")

			ex <- paste(gn, "= ", .gnames(e1)[i], " ", oper, " ", .gnames(e2)[i])
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

# logical raster
methods::setMethod("Ops", signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
		
		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
			
			name <- names(e2)[i]
			gn <- .makeGName(name, "rast")

			ex <- paste(gn, "= ", e1, " ", oper, " ", .gnames(e2)[i])
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

# raster logical
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.restore(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e2)[i]
			gn <- .makeGName(name, "rast")
			
			ex <- paste(gn, "= ", .gnames(e1)[i], " ", oper, " ", e2)
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

# numeric raster
methods::setMethod("Ops", signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		if (is.na(e1)) e1 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
		
			name <- names(e2)[i]
			gn <- .makeGName(name, "rast")

			ex <- paste(gn, "= ", e1, " ", oper, " ", .gnames(e2)[i])
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

# raster numeric
methods::setMethod("Ops", signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- "null()"
		oper <- as.vector(.Generic)[1L]

		for (i in 1L:nlyr(e1)) {
		
			name <- names(e1)[i]
			gn <- .makeGName(name, "rast")

			ex <- paste(gn, "= ", .gnames(e1)[i], " ", oper, " ", e2)
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
