#' Contour vectors from a raster
#'
#' This function creates a vector (\code{SpatialLines} or \code{SpatialLinesDataFrame}) object from a raster representing contour lines in the raster. It utilizes the \code{GRASS} function \code{r.contour}.
#' @param rast A \code{SpatRaster} or the name of a raster already in an existing \code{GRASS} session, with cell values representing elevation (typically in meters).
#' @param levels Numeric vector. Levels of values in \code{rast} at which contours should be drawn. You can specify contour levels using this argument or by providing values for \code{step}, \code{minlevel}, and \code{maxlevel}. \code{levels} will override use of \code{step}, if both of them are specified.
#' @param step Numeric. Increment between contour levels.
#' @param minlevel,maxlevel Numeric or \code{NULL} (default). Minimum and maximum contour levels. If \code{NULL} and \code{step} is not \code{NULL}, then the minimum and maximum values in the raster will be used.
#' @param cut Integer >= 0. Minimum number of points necessary to generate a contour line. A value of 0 implies no limit. Default is 2.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' 
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in \code{GRASS}).
#' @return If \code{grassToR} if \code{TRUE}, then a \code{SpatialLines} or \code{SpatialLinesDataFrame} object with the same coordinate reference system as \code{rast}. Regardless, a vector is written into the \code{GRASS} session. The name of this vector is given by \code{outGrassName}.
#' @details See the documentation for the \code{GRASS} module \code{r.contour}{https://grass.osgeo.org/grass82/manuals/r.contour.html}.
#' @seealso \code{\link[terra]{contour}}
#'
#' @examples man/examples/ex_fasterContour.R
#'
#' @export

fasterContour <- function(
	rast,
	step = NULL,
	minlevel = NULL,
	maxlevel = NULL,
	levels = seq(minValue(rast), maxValue(rast), length.out=5),
	cut = 2,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	outGrassName = 'contours',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(rast=rast, vect=NULL, grassDir=grassDir)
	
	if (!is.null(step) & is.null(minlevel)) minlevel <- terra::minmax(rast)[1L, , drop = TRUE]
	if (!is.null(step) & is.null(maxlevel)) maxlevel <- terra::minmax(rast)[2L, , drop = TRUE]
	
	# execute
	if (!is.null(levels)) {
		rgrass::execGRASS('r.contour', input=input, levels=levels, cut=cut, output=outGrassName, flags=flags, ...)
	} else if (!is.null(step) & is.null(minlevel) & is.null(maxlevel)) {
		rgrass::execGRASS('r.contour', input=input, step=step, minlevel=minlevel, maxlevel=maxlevel, cut=cut, output=outGrassName, flags=flags, ...)
	}
	
	if (grassToR) {

		out <- rgrass::readVECT(outGrassName)
		out
		
	}
	
}
