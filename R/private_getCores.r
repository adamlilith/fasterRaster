#' Calculate number of cores to use for multi-core functions
#'
#' This function calculates the number of cores to use for multi-core functions based on user preferences, size of the raster object, number of available cores, and whether or not the user wants to use multi-core functionality even if it's possible to do the entire raster processing in one block.
#' @param rast Raster, RasterStack, or RasterBrick
#' @param cores Integer > 0. Number of cores desired to be used. Default is 2.
#' @return Integer.
#' @keywords internal
.getCores <- function(rast, cores = 2) {

	cpus <- min(cores, parallel::detectCores(logical=FALSE))
	# if (!forceMulti) cpus <- min(cpus, raster::blockSize(rast, minblocks = 1)$n)
	cpus

}
