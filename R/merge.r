#' Combine two or more rasters with different extents and fill in NAs
#'
#' @description `merge()` combines two or more `GRaster`s, possibly with different extents, into a single larger `GRaster`. Where the same cell has different values in each raster, the value of the first raster's cell is used. If this is `NA`, then the value of the second raster's cell is used, and so on.
#'
#' @param x,y,... `GRaster`s.
#'
#' @returns A `GRaster`.
#' 
#' @seealso [terra::merge()], [terra::mosaic()], and **GRASS** module `r.patch`
#'
#' @example man/examples/ex_merge.r
#'
#' @aliases merge
#' @rdname merge
#' @exportMethod merge
methods::setMethod(
	f = "merge",
	signature = c(x = "GRaster", y = "GRaster"),
	definition = function(x, y, ...) {

	.restore(x)
	compareGeom(x, y, lyrs=TRUE, ext=FALSE, rowcol=FALSE, depths=TRUE, res=TRUE, zres=TRUE)
	
	x <- list(x, y, ...)

	# set region to combined extent
	rasts <- paste(sapply(x, .gnames), collapse=",")
	rgrass::execGRASS("g.region", raster=rasts, flags=c("o", "quiet"), intern=TRUE)

	# combine
	gn <- .makeGName("merge", "rast")
	args <- list(
		cmd = "r.patch",
		input = rasts,
		output = gn,
		nprocs = getFastOptions("cores"),
		memory = getFastOptions("memory"),
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)
	
	do.call(rgrass::execGRASS, args=args)
	.makeGRaster(gn, "merge")
	
	} # EOF
)
