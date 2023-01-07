#' Spatial extent of a GRASS session
#'
#' This function returns the spatial extent of a \code{GRASS} session. \code{GRASS} sessions must have an extent, and this is usually set by the first raster or vector that is imported into them. Any subsequent rasters or vectors are cropped to this extent (which may or may not be desirable). By default, the function returns an object of class \code{\link[terra]{SpatExtent}} from the \pkg{terra} package. \cr
#' Note that retrieving this information is somewhat of a hack, so it only returns the extent of the "default" location with the "PERMANENT" mapset. These will work for nearly all users of \pkg{fasterRaster}, but if you create non-typical locations or mapsets using \code{\link{initGrass}}, then this function may return erroneous results.
#'
#' @param terra If \code{TRUE} (default), the object that is returned is an \code{\link[terra]{SpatExtent}} object. If \code{FALSE}, then the bounding coordinates are returned as a vector.
#' 
#' @return An object of class \code{\link[terra]{SpatExtent}} or a numeric vector.
#'
#' @seealso \code{\link{fasterRes}}, \code{\link{fasterInfoRast}}, and \code{\link{fasterInfoVect}} in \pkg{fasterRaster};  \code{\link[terra]{SpatExtent}} and \code{\link[terra]{ext}} in the \code{\link[terra]{terra}} package
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterExt <- function(
	terra = TRUE
) {

	wind <- readLines(paste0(tempdir(), '/default/PERMANENT/WIND'))
	
	n <- wind[grepl('north:', wind)]
	s <- wind[grepl('south:', wind)]
	e <- wind[grepl('east:', wind)]
	w <- wind[grepl('west:', wind)]

	n <- strsplit(n, ':')[[1L]][2L]
	s <- strsplit(s, ':')[[1L]][2L]
	e <- strsplit(e, ':')[[1L]][2L]
	w <- strsplit(w, ':')[[1L]][2L]

	n <- as.numeric(n)
	s <- as.numeric(s)
	e <- as.numeric(e)
	w <- as.numeric(w)

	out <- c(xmin = w, xmax = e, ymin = s, ymax = n)
	if (terra) out <- terra::ext(out)
	out

}
