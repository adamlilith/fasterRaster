#' Apply a formula to each cell of a raster
#'
#' This function applies a user-defined function to each cell in a raster. It functions exactly the same as the \code{\link[raster]{calc}} function in the \pkg{raster} package except that it can use a multi-core implementation to speed processing. **NB**: The code for the multi-core implementation is taken almost verbatim from \code{\link[raster]{clusterR}}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session.
#' @param fun Function. Possibilities include nearly any function in the \pkg{raster} package that operates on a raster and does not require multiple cells to calculate values (i.e., doesn't use a moving window or otherwise summarize values across a raster). Examples include:
#' \itemize{
#' 	\item \code{\link[raster]{calc}}
#' 	\item \code{\link[raster]{extract}} (when used with polygons)
#' 	\item \code{\link[raster]{interpolate}}
#' 	\item \code{\link[raster]{overlay}}
#' 	\item \code{\link[raster]{predict}}
#' 	\item \code{\link[raster]{resample}}
#' }
#' @param cores Integer >0, number of CPU cores to use to calculate the focal function (default is 2).
#' @param forceMulti Logical, if \code{TRUE} (default) then the function will attempt to use the total number of cores in \code{cores}. (Note that this many not necessarily be faster since each core costs some overhead.)  If \code{FALSE}, then the function will use up to \code{cores} if needed (which also may not be faster... it always depends on the problem being computed).
#' @param filename Character, name of file for a new raster (optional).
#' @param na.rm Logical, if code{TRUE} then remove \code{NA} values (if supported by \code{fun}).
#' @param forcefun Logical, useful for debugging. See \code{\link[raster]{calc}}.
#' @param forceapply Logical, useful for debugging. See \code{\link[raster]{calc}}.
#' @param ... Arguments to pass to \code{\link[raster]{writeRaster}}
#' @return A raster object, possibly also written to disk.
#' @examples
#' data(madagascar)
#' out <- fasterCalc(madForest2000, sum, cores=4)
fasterCalc <- function(
	rast,
	fun,
	filename = '',
	na.rm = FALSE,
	cores = 2,
	forceMulti = TRUE,
	...
) {

	# number of cores
	bs <- raster::blockSize(rast)
	cores <- if (forceMulti) {
		min(c(parallel::detectCores(), cores))
	} else {
		bs$n
	}

	# start cores
	raster::beginCluster(n=cores)
	on.exit(raster::endCluster())
	
	# calculate
	fx <- function(x, ...) fun(x, ...)
	out <- raster::clusterR(rast, fun=fx, args=list(fun=fun), export=quote(omnibus::ellipseNames(...)))
	
	raster::endCluster()
	
	out

}
