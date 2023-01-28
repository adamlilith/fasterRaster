#' Spatial extent of a raster or vector
#'
#' Report the spatial extent of raster(s) and/or vector(s) in an active \code{GRASS} session.
#'
#' @param x Name of raster(s) and/or vector(s) in the active \code{GRASS} session. If nore than one object is named, then the maximum extent that fully encompasses all objects is returned. If missing, then the extent that encompasses all objects is returned.
#' @param rastOrVect If \code{NULL} (default), then the function will attempt to guess whether \code{x} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. This argument helps disambugiate this case.
#' @param terra If \code{TRUE}, the object that is returned is an \code{\link[terra]{SpatExtent}} object. If \code{FALSE} (default), then the bounding coordinates are returned as a vector.
#' @param names If \code{TRUE} (default), then the vector will have named values.
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
	rastOrVect = c('rasters', 'vectors'),
	terra = FALSE,
	names = TRUE
) {

	rastOrVect <- .getRastOrVect(rastOrVect, n=1, nullOK=TRUE)

	if (missing(x) || is.null(x)) {
		x <- fasterLs()
		rastOrVect <- names(x)
	} else if (is.null(rastOrVect)) {
		spatials <- fasterLs()
		if (sum(x %in% spatials) > length(x)) stop('Cannot resolve if "x" is a raster or vector. Use argument "rastOrVect".')
		rastOrVect <- names(spatials)[match(x, spatials)]
	} else {
		rastOrVect <- rep(rastOrVect, length(x))
	}
	
	if (length(x) == 0L) {
	
		meta <- rgrass::gmeta(ignore.stderr=TRUE)
		n <- meta$n
		s <- meta$s
		e <- meta$e
		w <- meta$w
		
		warning('No spatial objects are in the GRASS session. Using session defaults for the extent.')
		
	} else {
		
		w <- Inf
		e <- -Inf
		s <- Inf
		n <- -Inf

		for (i in seq_along(x)) {

			spatial <- x[i]
			type <- rastOrVect[i]
			
			if (type == 'raster') {
			
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
				
			} else if (type == 'vector') {
			
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
		
	} # if any objects in the session
		
	out <- c(w, e, s, n)
	if (names & !terra) names(out) <- c('xmin', 'xman', 'ymin', 'ymax')
	if (terra) out <- terra::ext(out)
	out

}
