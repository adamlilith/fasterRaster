#' Contour vectors from a raster
#'
#' This function creates a spatial vector object from a raster representing contour lines in the raster.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' 
#' @param levels Numeric vector. Levels of values in \code{rast} at which contours should be drawn. You can specify contour levels using this argument or by providing values for \code{step}, \code{minlevel}, and \code{maxlevel}. \code{levels} will override use of \code{step}, if both of them are specified.
#' @param cut Integer >= 0. Minimum number of points necessary to generate a contour line. A value of 0 implies no limit. Default is 2.
#'
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in \code{GRASS}).
#' @return If \code{grassToR} if \code{TRUE}, then a \code{SpatialLines} or \code{SpatialLinesDataFrame} object with the same coordinate reference system as \code{rast}. Regardless, a vector is written into the \code{GRASS} session. The name of this vector is given by \code{outGrassName}.
#'
#' @details See the documentation for the \code{GRASS} module \code{r.contour}{https://grass.osgeo.org/grass82/manuals/r.contour.html}.
#'
#' @seealso \code{\link[terra]{as.contour}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.contour.html}{\code{r.contour}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterContour.R
#'
#' @export

fasterContour <- function(
	rast,
	# step = NULL,
	# minlevel = NULL,
	# maxlevel = NULL,
	levels = NULL,
	cut = 2,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = 'rast',
	outGrassName = 'contourVect',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	inRastName <- .getInRastName(inRastName, rast)
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	
	# if (!is.null(step) & is.null(minlevel)) minlevel <- terra::minmax(rast)[1L, , drop = TRUE]
	# if (!is.null(step) & is.null(maxlevel)) maxlevel <- terra::minmax(rast)[2L, , drop = TRUE]
	
	# execute
	# if (!is.null(levels)) {
		rgrass::execGRASS('r.contour', input=input, levels=levels, cut=cut, output=outGrassName, flags=flags, ...)
	# } else if (!is.null(step) & is.null(minlevel) & is.null(maxlevel)) {
		# rgrass::execGRASS('r.contour', input=input, step=step, minlevel=minlevel, maxlevel=maxlevel, cut=cut, output=outGrassName, flags=flags, ...)
	# }
	
	if (grassToR) {

		out <- rgrass::read_VECT(outGrassName)
		if (!is.null(options()$grassVectOut) && !is.na(options()$grassVectOut)) {
			if (options()$grassVectOut == 'sf') out <- sf::st_as_sf(out)
		}
		out
		
	}
	
}
