#' Remove parts of a GRaster or GVector
#'
#' `crop()` removes parts of a `GRaster` or `GVector` that fall "outside" another raster or vector.
#'
#' @param x A `GRaster` or `GVector` to be cropped.
#' @param y A `GRaster` or `GVector` to serve as a template for cropping.
#'
#' @return A `GRaster` or `GVector`.
#' 
#' @seealso [terra::crop()], [sf::st_crop()]
#' 
#' @example man/examples/ex_crop.r
#'
#' @aliases crop
#' @rdname crop
#' @exportMethod crop
methods::setMethod(
	f = "crop",
	signature = c(x = "GRaster"),
	definition = function(x, y) {
	
	compareGeom(x, y)
	.restore(x)

	### change region to match the extent of y but have the same resolution as x
	region(x)

	args <- list(
		cmd = "g.region",
		flags = c("quiet", "overwrite", "o"),
		intern = TRUE
	)

	if (inherits(y, "GRaster")) {
		args <- c(args, list(raster = .gnames(y)))
	} else if (inherits(y, "GVector")) {
		args <- c(args, list(vector = .gnames(y)))
	}
	
	do.call(rgrass::execGRASS, args=args)

	### crop by creating copy of focal raster
	out <- list()
	gns <- .copyGSpatial(x, reshapeRegion = FALSE)
	out <- .makeGRaster(gns, names(x))
	print("RESOLUTION MAY BE CHANGED!!!")

	out

	} # EOF
)

#' @aliases crop
#' @rdname crop
#' @exportMethod crop
methods::setMethod(
	f = "crop",
	signature = c(x = "GVector"),
	definition = function(x, y) {
	
	compareGeom(x, y)
	.restore(x)

	## reshaping region to y
	args <- list(
		cmd = "g.region",
		flags = c("quiet", "overwrite", "o"),
		intern = TRUE
	)

	if (inherits(y, "GRaster")) {
		args <- c(args, list(raster = .gnames(y)))
	} else if (inherits(y, "GVector")) {
		args <- c(args, list(vector = .gnames(y)))
	}
	
	do.call(rgrass::execGRASS, args=args)

	### crop
	gn <- .makeGName("crop", "vector")
	rgrass::execGRASS("v.clip", input=.gnames(x), clip=.gnames(y), output=gn, flags=c("quiet", "overwrite", "r"), intern=TRUE)

	.makeGVector(gn)

	} # EOF
)
