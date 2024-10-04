#' Compare-methods operations on GRasters and GRegions
#'
#' @description You can do comparative operations on `GRaster`s using normal operators in **R**: `<`, `<=`, `==`, `!=`, `>=`, and `>`. You can also use \code{\link[fasterRaster]{%in%}} for categorical `GRasters` (see `vignette("GRasters", package = "fasterRaster")`).
#'
#' You can also compare two `GRegion`s using the `==` and `!=` operators. Most users of **fasterRaster** will not have to work much with "regions" (see `vignette("regions", package = "fasterRaster")`), so can ignore this functionality. `GRegion`s are the same if they have the same coordinate reference system, location/project and mapset (see `vignette("projects_mapsets", package = "fasterRaster")`), topology (2D or 3D), extent, and resolution. If both are 3D, then they must also have the same vertical extent and number of depths.
#' 
#' @param e1,e2 Values depend on the type of comparison:
#'
#' * Comparing `GRaster`s to logical, numeric, character values: `e1` and `e2` can be any one of these. Comparison to a character string can be useful when using a categorical raster, in which case you can use something like `raster1 == "Wetlands"` to coerce all "wetland" cells to be 1 (TRUE) and all others 0 (FALSE) or `NA` (if it was originally `NA`).
#' * Comparing a `GRegion` to another `GRegion`: `e1` and `e2` must be `GRegion`s!
#'
#' @returns Comparing `GRaster`s: An "integer" `GRaster` with values of 0 (FALSE), 1 (TRUE), or `NA` (neither).
#'
#' Comparing `GRegion`s: Output is logical.
#'
#' @example man/examples/ex_GRaster_comparison_logic.r
#'
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Compare
# raster raster
methods::setMethod(
	f = "Compare",
	signature(e1 = "GRaster", e2 = "GRaster"),
    function(e1, e2) {
	
		compareGeom(e1, e2)
		.locationRestore(e1)
		.region(e1)

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
			src <- .makeSourceName("comparison", "raster")

			ex <- paste0(src, "= int(if(", sources(e1)[i], " ", oper, " ", sources(e2)[i], "))")
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
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.locationRestore(e2)
		.region(e2)

		e1 <- as.integer(e1)
		if (is.na(e1)) e1 <- "null()"
		
		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
			
			name <- names(e2)[i]
			src <- .makeSourceName("comparison", "rast")

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
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {
	
		.locationRestore(e1)
		.region(e1)

		e2 <- as.integer(e2)
		if (is.na(e2)) e2 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e1)[i]
			src <- .makeSourceName("comparison", "rast")
			
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
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {
	
		.locationRestore(e2)
		.region(e2)

		if (is.na(e1)) e1 <- "null()"

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
		
			name <- names(e2)[i]
			src <- .makeSourceName("comparison", "rast")

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
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {
	
		.locationRestore(e1)
		.region(e1)

		if (is.na(e2)) e2 <- "null()"
		oper <- as.vector(.Generic)[1L]

		for (i in 1L:nlyr(e1)) {
		
			name <- names(e1)[i]
			src <- .makeSourceName("comparison", "raster")

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
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "GRaster", e2 = "integer"),
    function(e1, e2) {
	
		.locationRestore(e1)
		.region(e1)

		if (is.na(e2)) e2 <- "null()"
		oper <- as.vector(.Generic)[1L]

		for (i in 1L:nlyr(e1)) {
		
			name <- names(e1)[i]
			src <- .makeSourceName("comparison", "rast")

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
#' @exportMethod Compare
methods::setMethod(
    f = "Compare",
    signature(e1 = "integer", e2 = "GRaster"),
    function(e1, e2) {

        .locationRestore(e2)
		.region(e2)

        if (is.na(e1)) e1 <- "null()"

        oper <- as.vector(.Generic)[1L]
        for (i in 1L:nlyr(e2)) {
            name <- names(e2)[i]
            src <- .makeSourceName("comparison", "rast")

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
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "GRaster", e2 = "character"),
    function(e1, e2) {
	
	if (!all(is.factor(e1))) stop("Raster must be categorical for this type of comparison.")
	
	.locationRestore(e1)
	.region(e1)

	oper <- as.vector(.Generic)[1L]
	levs <- levels(e1)
	acs <- activeCat(e1, names = TRUE)

	nLayers <- nlyr(e1)
	srcs <- .makeSourceName("comparison", "raster", nLayers)

	for (i in seq_len(nLayers)) {

		# get value of this category
		ac <- acs[i]
		thisVal <- eval(parse(text = paste0("levs[[", i, "]][", ac, " == e2]")))
		thisVal <- thisVal[[1L]]

		if (length(thisVal) == 0L) {
			this <- 0L * not.na(e1)
		} else {

			if (oper == "<") {

				thisVal <- min(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == "<=") {

				thisVal <- max(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == ">") {
				
				thisVal <- max(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == ">=") {
				
				thisVal <- min(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == "==") {

				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " == ", thisVal[1L])

				if (length(thisVal) > 1L) {
					for (count in 2L:length(thisVal)) {
						ex <- paste0(ex, " | ", sources(e1)[i], " == ", thisVal[count])
					}
				}

				ex <- paste0(ex, ", 1, 0)")

			} else if (oper == "!=") {
			
				ex <- paste0(srcs[i], " = if(", sources(e1)[i], " != ", thisVal[1L])

				if (length(thisVal) > 1L) {
					for (count in 2L:length(thisVal)) {
						ex <- paste0(ex, " & ", sources(e1)[i], " != ", thisVal[count])
					}
				}

				ex <- paste0(ex, ", 1, 0)")
			
			}

			rgrass::execGRASS(cmd = "r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}

	} # next layer
	.makeGRaster(srcs, "logical")
	
	} # EOF
)

# character raster
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "character", e2 = "GRaster"),
    function(e1, e2) {
	
	if (!all(is.factor(e2))) stop("Raster must be categorical for this type of comparison.")
	.locationRestore(e2)
	.region(e2)

	oper <- as.vector(.Generic)[1L]
	levs <- levels(e2)
	acs <- activeCat(e2, names = TRUE)

	nLayers <- nlyr(e2)
	srcs <- .makeSourceName("comparison", "raster", nLayers)

	for (i in seq_len(nLayers)) {

		# get value of this category
		ac <- acs[i]
		thisVal <- eval(parse(text = paste0("levs[[", i, "]][", ac, " == e1]")))
		thisVal <- thisVal[[1L]]

		if (length(thisVal) == 0L) {
			this <- 0L * not.na(e2)
		} else {

			if (oper == "<") {

				thisVal <- min(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == "<=") {

				thisVal <- max(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == ">") {
				
				thisVal <- max(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == ">=") {
				
				thisVal <- min(thisVal)
				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " ", oper, " ", thisVal, ", 1, 0)")

			} else if (oper == "==") {

				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " == ", thisVal[1L])

				if (length(thisVal) > 1L) {
					for (count in 2L:length(thisVal)) {
						ex <- paste0(ex, " | ", sources(e2)[i], " == ", thisVal[count])
					}
				}

				ex <- paste0(ex, ", 1, 0)")

			} else if (oper == "!=") {
			
				ex <- paste0(srcs[i], " = if(", sources(e2)[i], " != ", thisVal[1L])

				if (length(thisVal) > 1L) {
					for (count in 2L:length(thisVal)) {
						ex <- paste0(ex, " & ", sources(e2)[i], " != ", thisVal[count])
					}
				}

				ex <- paste0(ex, ", 1, 0)")
			
			}

			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)

		}

	} # next layer
	.makeGRaster(srcs, "logical")
		
	} # EOF
)

# GRegion GRegion
#' @aliases Compare-methods
#' @rdname Compare-methods
#' @exportMethod Compare
methods::setMethod(
	f = "Compare",
	signature(e1 = "GRegion", e2 = "GRegion"),
    function(e1, e2) {
	
	oper <- as.vector(.Generic)[1L]
	if (!(oper %in% c("==", "!="))) stop("Can only use == and != operators when performing comparisons involving GRegions.")

	out <- TRUE
	if (.location(e1) != .location(e2)) out <- FALSE
	if (!terra::same.crs(e1, e2)) out <- out & FALSE

	if ((is.3d(e1) & !is.3d(e2)) | (!is.3d(e1) & is.3d(e2))) {
		out <- out & FALSE
	} else if (is.3d(e1) & is.3d(e2)) {
		top1 <- top(e1)
		top2 <- top(e2)
		bottom1 <- bottom(e1)
		bottom2 <- bottom(e2)
		out <- out & omnibus::compareFloat(top1, top2, "==") & omnibus::compareFloat(bottom1, bottom2, "==") & ndepth(e1) == ndepth(e2)
	}

	ext1 <- ext(e1, vector = TRUE)
	ext2 <- ext(e2, vector = TRUE)
	res1 <- res(e1)
	res2 <- res(e2)
	out <- out & all(omnibus::compareFloat(ext1, ext2, "==")) & all(omnibus::compareFloat(res1, res2, "=="))

	if (oper == "!=") out <- !out
	out

	} # EOF
)

### The problem with comparing GRasters and GRegions is that GRegions are always 3D, and GRasters may not be, so the test will always fail.
# # # # GRegion GRaster
# # # #' @aliases Compare-methods
# # # #' @rdname Compare-methods
# # # #' @exportMethod Compare
# # # methods::setMethod(
# # # 	f = "Compare",
# # # 	signature(e1 = "GRegion", e2 = "GRaster"),
# # #     function(e1, e2) {

# # # 	oper <- as.vector(.Generic)[1L]
# # # 	.compareGRegionGRaster(e1 = e1, e2 = e2, oper = oper)

# # # 	} # EOF
# # # )

# # # # GRaster GRegion
# # # #' @aliases Compare-methods
# # # #' @rdname Compare-methods
# # # #' @exportMethod Compare
# # # methods::setMethod(
# # # 	f = "Compare",
# # # 	signature(e1 = "GRaster", e2 = "GRegion"),
# # #     function(e1, e2) {

# # # 	oper <- as.vector(.Generic)[1L]
# # # 	.compareGRegionGRaster(e1 = e1, e2 = e2, oper = oper)

# # # 	} # EOF
# # # )

# # # #' @noRd
# # # .compareGRegionGRaster <- function(e1, e2, oper) {
	
# # # 	if (!(oper %in% c("==", "!="))) stop("Can only use == and != operators when performing comparisons involving GRegions.")

# # # 	out <- TRUE
# # # 	if (.location(e1) != .location(e2)) out <- FALSE
# # # 	if (!terra::same.crs(e1, e2)) out <- out & FALSE

# # # 	if (is.3d(e1) & is.3d(e2)) {
# # # 		top1 <- top(e1)
# # # 		top2 <- top(e2)
# # # 		bottom1 <- bottom(e1)
# # # 		bottom2 <- bottom(e2)
# # # 		out <- out & omnibus::compareFloat(top1, top2, "==") & omnibus::compareFloat(bottom1, bottom2, "==") & ndepth(e1) == ndepth(e2)
# # # 	}

# # # 	ext1 <- ext(e1, vector = TRUE)
# # # 	ext2 <- ext(e2, vector = TRUE)
# # # 	res1 <- res(e1)
# # # 	res2 <- res(e2)
# # # 	out <- out & all(omnibus::compareFloat(ext1, ext2, "==")) & all(omnibus::compareFloat(res1, res2, "=="))

# # # 	if (oper == "!=") out <- !out
# # # 	out

# # # }

# # # # GRegion GVector
# # # #' @aliases Compare-methods
# # # #' @rdname Compare-methods
# # # #' @exportMethod Compare
# # # methods::setMethod(
# # #     f = "Compare",
# # #     signature(e1 = "GRegion", e2 = "GVector"),
# # #     function(e1, e2) {
# # #         oper <- as.vector(.Generic)[1L]
# # #         .compareGRegionGVector(e1 = e1, e2 = e2, oper = oper)
# # #     } # EOF
# # # )

# # # # GVector GRegion
# # # #' @aliases Compare-methods
# # # #' @rdname Compare-methods
# # # #' @exportMethod Compare
# # # methods::setMethod(
# # #     f = "Compare",
# # #     signature(e1 = "GVector", e2 = "GRegion"),
# # #     function(e1, e2) {
# # #         oper <- as.vector(.Generic)[1L]
# # #         .compareGRegionGVector(e1 = e1, e2 = e2, oper = oper)
# # #     } # EOF
# # # )

# # # #' @noRd
# # # .compareGRegionGVector <- function(e1, e2, oper) {
	
# # # 	if (!(oper %in% c("==", "!="))) stop("Can only use == and != operators when performing comparisons involving GRegions.")

# # # 	out <- TRUE
# # # 	if (.location(e1) != .location(e2)) out <- FALSE
# # # 	if (!terra::same.crs(e1, e2)) out <- out & FALSE

# # # 	ext1 <- ext(e1, vector = TRUE)
# # # 	ext2 <- ext(e2, vector = TRUE)
# # # 	out <- out & all(omnibus::compareFloat(ext1, ext2, "==")) & all(omnibus::compareFloat(res1, res2, "=="))

# # # 	if (oper == "!=") out <- !out
# # # 	out

# # # }
