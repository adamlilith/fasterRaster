#' Compare-methods operations on GRasters
#'
#' @description You can do comparative operations on `GRaster`s using normal operators in **R**: `<`, `<=`, `==`, `!=`, `>=`, and `>`.
#' 
#' @param e1,e2 `GRaster`, logical, numeric, or character. Character is useful when using a [categorical raster][tutorial_raster_data_types], in which case you can use something like `raster1 == "Wetlands"` to force all "wetland" cells to be 1 (TRUE) and all others 0 (FALSE) or `NA` (if it was originally `NA`).
#'
#' @returns An "integer `GRaster` with values of 0 (FALSE), 1 (TRUE), or `NA` (neither).
#'
#' @example man/examples/ex_GRaster_arithmetic.r
#'
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
# raster raster
methods::setMethod(
	f = "Ops",
	signature(e1 = "GRaster", e2 = "GRaster"),
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
			src <- .makeSourceName(name, "raster")

			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", sources(e2)[i], "))")
			
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

# logical raster
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
		
		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
			
			name <- names(e2)[i]
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", e1, " ", oper, " ", sources(e2)[i], "))")
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

# raster logical
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.restore(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e2)[i]
			src <- .makeSourceName(name, "rast")
			
			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", e2, "))")
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

# numeric raster
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.restore(e2)

		if (is.na(e1)) e1 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
		
			name <- names(e2)[i]
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", e1, " ", oper, " ", sources(e2)[i], "))")
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

# raster numeric
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- "null()"
		oper <- as.vector(.Generic)[1L]

		for (i in 1L:nlyr(e1)) {
		
			name <- names(e1)[i]
			src <- .makeSourceName(name, "raster")

			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", e2, "))")
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

# raster integer
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "GRaster", e2 = "integer"),
    function(e1, e2) {
	
		.restore(e1)

		if (is.na(e2)) e2 <- "null()"
		oper <- as.vector(.Generic)[1L]

		for (i in 1L:nlyr(e1)) {
		
			name <- names(e1)[i]
			src <- .makeSourceName(name, "rast")

			ex <- paste(src, "= int(if(", sources(e1)[i], " ", oper, " ", e2, "))")
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

# integer raster
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
    f = "Ops",
    signature(e1 = "integer", e2 = "GRaster"),
    function(e1, e2) {
        .restore(e2)

        if (is.na(e1)) e1 <- "null()"

        oper <- as.vector(.Generic)[1L]
        for (i in 1L:nlyr(e2)) {
            name <- names(e2)[i]
            src <- .makeSourceName(name, "rast")

            ex <- paste(src, "= int(if(", e1, " ", oper, " ", sources(e2)[i], "))")
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

# raster character
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "GRaster", e2 = "character"),
    function(e1, e2) {
	
		if (!all(is.factor(e1))) stop("Raster must be categorical for this type of comparison.")
		.restore(e1)

		oper <- as.vector(.Generic)[1L]
		levs <- levels(e1)
		acs <- activeCat(e1, names = TRUE)

		srcs <- .makeSourceName("logical", "raster", nlyr(e1))

		for (i in seq_len(nlyr(e1))) {

			# get value of this category
			ac <- acs[i]
			thisVal <- eval(parse(text = paste0("levs[[", i, "]][", ac, " == e2]")))
			thisVal <- thisVal[[1L]]

			if (length(thisVal) == 0L) {
				this <- 0L * not.na(e1)
			} else {

				if (oper %in% c("<", "<=", ">=", ">")) {

					thisVal <- max(thisVal)
					
					ex <- paste0(srcs[i], " = if(", sources(e1)[i], " ", oper, " ", thisVal, ", 1, 0)")

				thisVal <- max(thisVal)
				} else if (oper %in% c(">", ">=")) {
					
					thisVal <- min(thisVal)

					ex <- paste0(srcs[i], " = if(", sources(e1)[i], " ", oper, " ", thisVal, ", 1, 0)")

				} else if (oper == "==") {

     				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " == ", thisVal[1L])

					if (length(thisVal) > 1L) {
						
						for (count in 2L:length(thisVal)) {
							
							ex <- paste0(ex, " | ", sources(e1)[i], " == ", thisVal)

						}
					}

					ex <- paste0(ex, ", 1, 0)")

				} else if (oper == "!=") {
				
					ex <- paste0(srcs[i], " = if(", sources(e1)[i], " != ", thisVal[1L])

					if (length(thisVal) > 1L) {

						for (count in 2L:length(thisVal)) {
							ex <- paste0(ex, " & ", sources(e1)[i], " != ", thisVal)
						}

					}

					ex <- paste0(ex, ", 1, 0)")
				
				}

				args <- list(
					cmd = "r.mapcalc",
					expression = ex,
					flags = c(.quiet(), "overwrite"),
					intern = TRUE
				)

				do.call(rgrass::execGRASS, args = args)

			}

		} # next layer
		.makeGRaster(srcs, "logical")
		
	} # EOF
)

# character raster
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Ops
methods::setMethod(
	f = "Ops",
	signature(e1 = "character", e2 = "GRaster"),
    function(e1, e2) {
	
		if (!all(is.factor(e2))) stop("Raster must be categorical for this type of comparison.")
		.restore(e2)

		oper <- as.vector(.Generic)[1L]
		levs <- levels(e2)
		acs <- activeCat(e2, names = TRUE)

		srcs <- .makeSourceName("logical", "raster", nlyr(e2))

		for (i in seq_len(nlyr(e2))) {

			# get value of this category
			ac <- acs[i]
			thisVal <- eval(parse(text = paste0("levs[[", i, "]][", ac, " == e1]")))
			thisVal <- thisVal[[1L]]

			if (length(thisVal) == 0L) {
				this <- 0L * not.na(e2)
			} else {

				if (oper %in% c("<", "<=", ">=", ">")) {

					thisVal <- max(thisVal)
					
					ex <- paste0(srcs[i], " = if(", sources(e2)[i], " ", oper, " ", thisVal, ", 1, 0)")

				thisVal <- max(thisVal)
				} else if (oper %in% c(">", ">=")) {
					
					thisVal <- min(thisVal)

					ex <- paste0(srcs[i], " = if(", sources(e2)[i], " ", oper, " ", thisVal, ", 1, 0)")

				} else if (oper == "==") {

     				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " == ", thisVal[1L])

					if (length(thisVal) > 1L) {
						
						for (count in 2L:length(thisVal)) {
							
							ex <- paste0(ex, " | ", sources(e2)[i], " == ", thisVal)

						}
					}

					ex <- paste0(ex, ", 1, 0)")

				} else if (oper == "!=") {
				
					ex <- paste0(srcs[i], " = if(", sources(e2)[i], " != ", thisVal[1L])

					if (length(thisVal) > 1L) {

						for (count in 2L:length(thisVal)) {
							ex <- paste0(ex, " & ", sources(e2)[i], " != ", thisVal)
						}

					}

					ex <- paste0(ex, ", 1, 0)")
				
				}

				args <- list(
					cmd = "r.mapcalc",
					expression = ex,
					flags = c(.quiet(), "overwrite"),
					intern = TRUE
				)

				do.call(rgrass::execGRASS, args = args)

			}

		} # next layer
		.makeGRaster(srcs, "logical")
		
	} # EOF
)
