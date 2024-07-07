#' Replace values of a GRaster
#'
#' @description The `[<-` operator can be used to replace all of the values of a `GRaster`, or specific values depending on the expression in `i`. For example, you could use `rast[] <- 10` to assign 10 to all cells, or `rast[rast > 0] <- 10` to assign all cells with values >0 to 10.
#'
#' @param x A `GRaster`.
#' @param i Either missing or a conditional statement that resolves to a `GRaster`.
#' @param j Not used
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
	signature = c(x = "GRaster", i = "missing", j = "ANY"),
	definition = function(x, i, j, value) {
	
	i <- NULL
	if (missing(j)) {
		j <- NULL
	} else {
		warning("Argument j will be ignored.")
	}
	
	if (!inherits(value, c("numeric", "integer", "logical"))) stop("Can only assign numeric, integer, or logical values to a raster.")
	if (length(value) != 1L) stop("Cannot assign multiple values to a raster.")
	
	.locationRestore(x)
	.region(x)
	
	if (is.na(value)) {
		value <- "null()"
	} else if (is.logical(value)) {
		value <- as.integer(value)
	}
	
	nLayers <- nlyr(x)
	srcs <- .makeSourceName(x, "raster", nLayers)
	for (count in seq_len(nLayers)) {

		ex <- paste0(srcs[count], " = ", value)

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	} # next raster
	.makeGRaster(srcs, names(x))
	
	} # EOF
)

#' @name [<-
#' @aliases [<-,GRaster,ANY,ANY-method
#' @docType methods
#' @rdname replace_single_square_bracket
#' @exportMethod [<-
setMethod(
	f = "[<-",
	signature = c(x = "GRaster", i = "GRaster", j = "ANY"),
	definition = function(x, i, j, value) {
	
	if (missing(j)) {
		j <- NULL
	} else {
		warning("Argument j will be ignored.")
	}
	
	if (!inherits(value, c("numeric", "integer", "logical"))) stop("Can only assign numeric, integer, or logical values to a raster.")
	if (length(value) != 1L) stop("Cannot assign multiple values to a raster.")
	
	.locationRestore(x)
	.region(x)
	
	if (is.na(value)) {
		value <- "null()"
	} else if (is.logical(value)) {
		value <- as.integer(value)
	}
	
	nLayers <- nlyr(x)
	nLayersI <- nlyr(i)

	if (nLayersI != nLayers) {

		if (nlyr(i) != 1L) warning("The number of layers in the subset raster (inside the `[]`) does not have\n  the same number of layers as the raster to be subset. The layers inside\n  the square brackets will be recycled.")

		iIndex <- rep(seq_len(nLayersI), length.out = nLayers)
		i <- i[[iIndex]]

	}

	srcs <- .makeSourceName(x, "raster", nLayers)
	for (count in seq_len(nLayers)) {

		if (inherits(i, "GRaster")) {
			if (!(.minVal(i)[count] %in% c(NA, 0, 1)) & !(.maxVal(i)[count] %in% c(NA, 0, 1))) stop("The GRaster in `i` must 0, 1, or NA values.")
			ex <- paste0(srcs[count], " = if(", sources(i)[count], " == 1, ", value, ", ", sources(x)[count], ")")
		} else {
			ex <- paste0(srcs[count], " = ", value)
		}

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	} # next raster
	.makeGRaster(srcs, names(x))
	
	} # EOF
)


