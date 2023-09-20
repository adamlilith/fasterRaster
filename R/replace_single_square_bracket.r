#' Replace values of a GRaster
#'
#' @description The `[<-` operator can be used to replace the values of a `GRaster`.
#'
#' @param x A `GRaster`.
#' @param i,j Not used.
#' @param value A numeric, integer, or logical value, or `NA`. Only a single value can be used.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @name [<-
#' @aliases [<-,GRaster,ANY,ANY-method
#' @docType methods
#' @rdname replace_single_square_bracket
#' @exportMethod [<-
setMethod(
	f = "[<-",
	signature = c(x = "GRaster", i = "ANY", j = "ANY"),
	definition = function(x, i, j, value) {
	
	if (missing(i)) {
		i <- NULL
	} else {
		warning("Argument i will be ignored.")
	}
	if (missing(j)) {
		j <- NULL
	} else {
		warning("Argument j will be ignored.")
	}
	
	if (!inherits(value, c("numeric", "integer", "logical"))) stop("Can only assign numeric, integer, or logical values to a raster.")
	if (length(value) != 1L) stop("Cannot assign multiple values to a raster.")
	
	.restore(x)
	region(x)
	
	if (is.na(value)) {
		value <- "null()"
	} else if (is.logical(value)) {
		value <- as.integer(value)
	}
	
	nLayers <- nlyr(x)
	srcs <- .makeSourceName(x, "raster", nLayers)
	for (i in seq_len(nLayers)) {
	
		ex <- paste0(srcs[i], " = ", value)
		args <- list(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c("quiet", "overwrite"),
			intern = TRUE
		)
		do.call(rgrass::execGRASS, args = args)
	
	} # next raster
	.makeGRaster(srcs, 'layer')
	
	} # EOF
)
