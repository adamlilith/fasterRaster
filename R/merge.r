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

	.locationRestore(x)
	compareGeom(x, y, lyrs=TRUE, ext=FALSE, rowcol=FALSE, depths=TRUE, res=TRUE, zres=TRUE)
	
	x <- list(x, y, ...)

	# set region to combined extent
	rasts <- paste(sapply(x, sources), collapse=",")
	rgrass::execGRASS("g.region", raster = rasts, flags=c("o", .quiet()))

	# combine
	src <- .makeSourceName("merge", "raster")
	rgrass::execGRASS(
		cmd = "r.patch",
		input = rasts,
		output = src,
		nprocs = faster("cores"),
		memory = faster("memory"),
		flags = c(.quiet(), "overwrite")
	)

	# combine levels
	levels <- combineLevels(x)
	.makeGRaster(src, levels = levels)
	
	} # EOF
)
