#' Logic-methods operations on GRasters
#'
#' @description You can do logical operations on `GRaster`s. A cell with a value of 1 is interpreted as `TRUE`, and a value of 0 is interpreted as `FALSE`. You can compare:
#' * A `GRaster` to another `GRaster`
#' * A `GRaster` to a logical value (`TRUE` or `FALSE`, but not `NA`--see [not.na()])
#' * A `GRaster` to a numeric or integer value that is 0 or 1
#' 
#' Operators include:
#' * `|`: `TRUE` if either condition is `TRUE` (or 1), but returns `NA` if either condition is `NA`.
#' * `&`: `TRUE` if both conditions are `TRUE` (or 1), but `NA` if either is `NA`.
#'
#' @param e1,e2 Two `GRaster`s, or a `GRaster` and a logical value (`TRUE` or `FALSE`, but not `NA`), a numeric value that is 0 or 1 (but not `NA_real_`), or an integer value that is 0 or 1 (but not `NA_integer_`).
#'
#' @returns A binary `GRaster` (1 ==> `TRUE`, 0 ==> `FALSE`, plus `NA` when comparison results in `NA`).
#'
#' @example man/examples/ex_GRaster_comparison_logic.r
#'
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod Logic
methods::setMethod(
	f = "Logic",
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
			src <- .makeSourceName("logic", "raster")

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
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod Logic
methods::setMethod(
	f = "Logic",
	signature(e1 = "logical", e2 = "GRaster"),
    function(e1, e2) {
	
		.locationRestore(e2)
		.region(e2)

		e1 <- as.integer(e1)
		
		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e2)) {
			
			name <- names(e2)[i]
			src <- .makeSourceName("logic", "rast")

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
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod logic
methods::setMethod(
	f = "Logic",
	signature(e1 = "GRaster", e2 = "logical"),
    function(e1, e2) {

		.locationRestore(e1)
		.region(e1)

		e2 <- as.integer(e2)

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e1)[i]
			src <- .makeSourceName("logic", "rast")
			
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

# raster numeric
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod logic
methods::setMethod(
	f = "Logic",
	signature(e1 = "GRaster", e2 = "numeric"),
    function(e1, e2) {

		.locationRestore(e1)
		.region(e1)

		if (!omnibus::is.wholeNumber(e2) | !(omnibus::compareFloat(e2, 0, "==") | omnibus::compareFloat(e2, 1, "=="))) stop("Can only perform logical operations using GRasters and\n  either another GRaster, TRUE, FALSE, 1, or 0.")
		e2 <- as.integer(e2)

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e1)[i]
			src <- .makeSourceName("logic", "rast")
			
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
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod logic
methods::setMethod(
	f = "Logic",
	signature(e1 = "numeric", e2 = "GRaster"),
    function(e1, e2) {

		.locationRestore(e1)
		.region(e1)

		if (!omnibus::is.wholeNumber(e1) | !(omnibus::compareFloat(e1, 0, "==") | omnibus::compareFloat(e1, 1, "=="))) stop("Can only perform logical operations using GRasters and\n  either another GRaster, TRUE, FALSE, 1, or 0.")
		e1 <- as.integer(e1)

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e2)[i]
			src <- .makeSourceName("logic", "rast")
			
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

# raster integer
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod logic
methods::setMethod(
	f = "Logic",
	signature(e1 = "GRaster", e2 = "integer"),
    function(e1, e2) {

		.locationRestore(e1)
		.region(e1)

		if (!(e2 == 0L | e2 == 1L)) stop("Can only perform logical operations using GRasters and\n  either another GRaster, TRUE, FALSE, 1, or 0.")
		e2 <- as.integer(e2)

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e1)[i]
			src <- .makeSourceName("logic", "rast")
			
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
#' @aliases Logic-methods
#' @rdname Logic-methods
#' @exportMethod logic
methods::setMethod(
	f = "Logic",
	signature(e1 = "integer", e2 = "GRaster"),
    function(e1, e2) {

		.locationRestore(e1)
		.region(e1)

		if (!(e1 == 0L | e1 == 1L)) stop("Can only perform logical operations using GRasters and\n  either another GRaster, TRUE, FALSE, 1, or 0.")
		e1 <- as.integer(e1)

		oper <- as.vector(.Generic)[1L]
		for (i in 1L:nlyr(e1)) {

			name <- names(e2)[i]
			src <- .makeSourceName("logic", "rast")
			
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

# # raster raster
# #' @aliases Logic-methods
# #' @rdname Logic-methods
# #' @exportMethod orOrOr
# methods::setMethod(
# 	f = "orOrOr",
# 	signature = c(e1 = "GRaster", e2 = "GRaster"),
# 	function(e1, e2) {
	
# 	compareGeom(e1, e2)
# 	.locationRestore(e1)
# 	.region(e1)

# 	if (nlyr(e1) > 1L & nlyr(e2) == 1L) {
# 		e2 <- e2[[rep(1L, nlyr(e1))]]
# 	} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {
# 		e1 <- e1[[rep(1L, nlyr(e2))]]
# 	} else if (nlyr(e1) != nlyr(e2)) {
# 		stop("Rasters must have the same number of layers, or at least one raster must have a single layer.")
# 	}

# 	for (i in 1L:nlyr(e1)) {

# 		name <- paste0(names(e1)[i], "_", names(e2)[i])
# 		src <- .makeSourceName("logic", "raster")

# 		ex <- paste0(src, "= int(if(", sources(e1)[i], " ||| ", sources(e2)[i], "))")
# 		this <- .genericArithRast(name = name, src = src, ex = ex)
		
# 		if (i == 1L) {
# 			out <- this
# 		} else {
# 			out <- c(out, this)
# 		}
		
# 	}
# 	out
		
# 	} # EOF
# )

# # raster raster
# #' @aliases Logic-methods
# #' @rdname Logic-methods
# #' @exportMethod orOrOr
# methods::setMethod(
# 	f = "andAndAnd",
# 	signature = c(e1 = "GRaster", e2 = "GRaster"),
# 	function(e1, e2) {
	
# 	compareGeom(e1, e2)
# 	.locationRestore(e1)
# 	.region(e1)

# 	if (nlyr(e1) > 1L & nlyr(e2) == 1L) {
# 		e2 <- e2[[rep(1L, nlyr(e1))]]
# 	} else if (nlyr(e1) == 1L & nlyr(e2) > 1L) {
# 		e1 <- e1[[rep(1L, nlyr(e2))]]
# 	} else if (nlyr(e1) != nlyr(e2)) {
# 		stop("Rasters must have the same number of layers, or at least one raster must have a single layer.")
# 	}

# 	for (i in 1L:nlyr(e1)) {

# 		name <- paste0(names(e1)[i], "_", names(e2)[i])
# 		src <- .makeSourceName("logic", "raster")

# 		ex <- paste0(src, "= int(if(", sources(e1)[i], " &&& ", sources(e2)[i], "))")
# 		this <- .genericArithRast(name = name, src = src, ex = ex)
		
# 		if (i == 1L) {
# 			out <- this
# 		} else {
# 			out <- c(out, this)
# 		}
		
# 	}
# 	out
		
# 	} # EOF
# )

# #' @name `|||`
# #' @title Logic-methods operations on GRasters
# #' @rdname Logic-methods
# #' @export
# `|||` <- function(e1, e2) orOrOr(e1, e2)

# #' @name `&&&`
# #' @title Logic-methods operations on GRasters
# #' @rdname Logic-methods
# #' @export
# `&&&` <- function(e1, e2) andAndAnd(e1, e2)

