#' Rename a raster or vector in an existing GRASS session
#'
#' Rename a raster or vector in an existing GRASS session.
#'
#' @param from,to Names of the raster or vector to rename.
#' @param type Either \code{'raster'} and/or \code{'vector'}, or \code{NULL} (default). This specifies the type of file to be renamed. Partial matching is allowed. If left as \code{NULL} (default), the function will rename any raster or vector with the given name. Note that unlike in \pkg{R}, \code{GRASS} can have rasters with the same name as vectors.
#' 
#' @return The function invisibly returns \code{TRUE} if the desired rasters and/or vectors were named, and \code{FALSE} if raster and/or vector to be renamed did not exist in the \code{GRASS} session. Notably, a raster or vector or both are renamed in an existing \code{GRASS} session.
#'
#' @seealso \code{\link{fasterLs}}, \code{\link{fasterRm}}, \href{https://grass.osgeo.org/grass82/manuals/g.rename.html}{\code{g.list}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterOperations.r
#'
#' @export
fasterRename <- function(from, to, type = NULL) {
	
	flags <- c('quiet', 'overwrite')
	
	if (is.null(type)) {
	
		wantRaster <- wantVector <- TRUE
	
	} else {
	
		type <- tolower(type)
		n <- nchar(type)

		# want raster?
		
		subs <- rep(NA, length(type))
		for (i in seq_along(type)) subs[i] <- substr('rasters', 1, n[i])
		wantRaster <- any(type %in% subs)
		
		# want vector?
		subs <- rep(NA, length(type))
		for (i in seq_along(type)) subs[i] <- substr('vectors', 1, n[i])
		wantVector <- any(type %in% subs)
	
	}

	fromTo <- c(from, to)

	success <- TRUE

	# rename raster
	if (wantRaster) {
	
		files <- fasterLs('rasters')
		if (!(from %in% files)) {
			warning('A raster with this name does not exist.')
			success <- FALSE
		} else {
			if (to %in% files) warning('Overwriting existing raster.')
			rgrass::execGRASS('g.rename', flags=flags, raster=fromTo)
			success <- TRUE
		}
		
	}

	# rename vector
	if (wantVector) {
	
		files <- fasterLs('vectors')
		if (!(from %in% files)) {
			warning('A vector with this name does not exist.')
			success <- FALSE
		} else {
			if (to %in% files) warning('Overwriting existing vector.')
			rgrass::execGRASS('g.rename', flags=flags, vector=fromTo)
			success <- success & TRUE
		}
		
	}

	invisible(success)

}
