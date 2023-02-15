#' Spatial extent of a raster or vector
#'
#' Report the spatial extent of raster(s) and/or vector(s) in an active \code{GRASS} session.
#'
#' @param x Name of raster(s) and/or vector(s) in the active \code{GRASS} session. If more than one object is named, then the maximum extent that fully encompasses all objects is returned. If missing, then the extent that encompasses all objects is returned.
#' @param rastOrVect If \code{NULL} (default), then the function will attempt to guess whether \code{x} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. This argument helps disambiguate this case.
#' @param terra If \code{TRUE}, the object that is returned is an \code{\link[terra]{SpatExtent}} object. If \code{FALSE} (default), then the bounding coordinates are returned as a vector.
#' @param names If \code{TRUE} (default), then the vector will have named values.
#' @param ... Other arguments for \code{\link{fasterLs}} like \code{temps} (return temporary files?).
#' 
#' @return A numeric vector or an object of class \code{\link[terra]{SpatExtent}}.
#'
#' @seealso \code{\link{fasterInfo}}, \code{\link{fasterDim}}, and \code{\link{fasterRes}} in \pkg{fasterRaster}; \code{\link[terra]{SpatExtent}} and \code{\link[terra]{ext}} in the \pkg{terra} package; \code{GRASS} modules \href{https://grass.osgeo.org/grass82/manuals/r.info.html}{r.info} and \href{https://grass.osgeo.org/grass82/manuals/v.info.html}{v.info}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterExt <- function(
	x,
	rastOrVect = NULL,
	terra = FALSE,
	names = TRUE,
	...
) {

	# clean and validate inputs
	info <- .rastOrVectAndX(x=x, rastOrVect=rastOrVect, ...)
	x <- info$x
	rastOrVect <- info$rastOrVect
	
	w <- Inf
	e <- -Inf
	s <- Inf
	n <- -Inf

	for (i in seq_along(x)) {

		spatial <- x[i]
		rov <- rastOrVect[i]
		
		if (rov == 'raster') {
		
			suppressMessages(
				info <- rgrass::execGRASS(
					'r.info',
					flags = 'g',
					map = spatial,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
			
		} else if (rov == 'vector') {
		
			suppressMessages(
				info <- rgrass::execGRASS(
					'v.info',
					flags = 'g',
					map = spatial,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
		
		} # if raster or vector
		
		nprime <- info[grepl('north=', info)]
		sprime <- info[grepl('south=', info)]
		eprime <- info[grepl('east=', info)]
		wprime <- info[grepl('west=', info)]

		nprime <- sub(nprime, pattern='north=', replacement='')
		sprime <- sub(sprime, pattern='south=', replacement='')
		eprime <- sub(eprime, pattern='east=', replacement='')
		wprime <- sub(wprime, pattern='west=', replacement='')

		nprime <- as.numeric(nprime)
		sprime <- as.numeric(sprime)
		eprime <- as.numeric(eprime)
		wprime <- as.numeric(wprime)
		
		if (nprime > n) n <- nprime
		if (sprime < s) s <- sprime
		if (eprime > e) e <- eprime
		if (wprime < w) w <- wprime
		
	} # next raster or vector
	
	out <- c(w, e, s, n)
	if (names & !terra) names(out) <- c('xmin', 'xman', 'ymin', 'ymax')
	if (terra) out <- terra::ext(out)
	out

}
