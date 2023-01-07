#' Spatial extent of a GRASS session
#'
#' This function returns the spatial extent of a \code{GRASS} session. \code{GRASS} sessions must have an extent, and this is usually set by the first raster or vector that is imported into them. Any subsequent rasters or vectors are cropped to this extent (which may or may not be desirable). By default, the function returns an object of class \code{\link[terra]{SpatExtent}} from the \pkg{terra} package. \cr
#' Note that retrieving this information is somewhat of a hack, so it only returns the extent of the "default" location with the "PERMANENT" mapset. These will work for nearly all users of \pkg{fasterRaster}, but if you create non-typical locations or mapsets using \code{\link{initGrass}}, then this function may return erroneous results.
#'
#' @param terra If \code{TRUE} (default), the object that is returned is an \code{\link[terra]{SpatExtent}} object. If \code{FALSE}, then the bounding coordinates are returned as a vector.
#' 
#' @return An object of class \code{\link[terra]{SpatExtent}} or a numeric vector.
#'
#' @seealso \code{\link{fasterExt}}, \code{\link{fasterInfoRast}}, and \code{\link{fasterInfoVect}} in \pkg{fasterRaster};  \code{\link[terra]{res}} in the \code{\link[terra]{terra}} package
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterRes <- function(
	terra = TRUE
) {

	wind <- readLines(paste0(tempdir(), '/default/PERMANENT/WIND'))
	
	ns <- wind[grepl('n-s resol:', wind)]
	ew <- wind[grepl('e-w resol:', wind)]

	ns <- strsplit(ns, ':')[[1L]][2L]
	ew <- strsplit(ew, ':')[[1L]][2L]

	ns <- as.numeric(ns)
	ew <- as.numeric(ew)

	out <- c(x = ew, y = ns)
	out

}
