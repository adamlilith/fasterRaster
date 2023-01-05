#' Names of all rasters and/or spatial vectors in a GRASS session
#'
#' Displays the names of all rasters and/or vectors that have been exported to or created in a GRASS session.
#'
#' @param type The type of spatial dataset for which to display names. This can include \code{'rasters'} (all rasters) and/or \code{'vectors'} (all spatial vectors). Partial matching is supported.
#' @param incTemp If \code{TRUE}, then remove any temporary files from the results. By default, these are not returned as part of the results. Some \pkg{fasterRaster} functions create temporary rasters or vectors which begin with the string "\code{__TEMPTEMP}".
#' 
#' @return Nothing (displays names of rasters and/or vectors).
#'
#' @seealso \code{\link{fasterInfoRast}}, \code{\link{fasterInfoVect}}, \href{https://grass.osgeo.org/grass82/manuals/g.list.html}{\code{g.list}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterLs <- function(type = c('raster', 'vector'), incTemp = FALSE) {
	
	flags <- c('quiet', 'overwrite')
	
	type <- tolower(type)
	n <- nchar(type)

	# want raster?
	subs <- rep(NA, length(type))
	for (i in seq_along(type)) subs[i] <- substr('rasters', 1, n[i])
	wantRasters <- any(type %in% subs)
	
	# want vector?
	subs <- rep(NA, length(type))
	for (i in seq_along(type)) subs[i] <- substr('vectors', 1, n[i])
	wantVectors <- any(type %in% subs)

	rasts <- vects <- character()

	if (wantRasters) {
	
		outFile <- paste0(tempdir(), '/__TEMPTEMP_files.csv')
		
		rgrass::execGRASS('g.list', flags=flags, type='raster', output=outFile)
		rasts <- tryCatch(
			read.table(paste0(tempdir(), '/__TEMPTEMP_files.csv')),
			error=function(e) FALSE
		)
		if (!is.logical(rasts[1L])) {
			rasts <- unlist(rasts)
			names(rasts) <- rep('raster', length(rasts))
		} else {
			rasts <- NULL
		}
		
		file.remove(outFile)
	
	}

	if (wantVectors) {
	
		outFile <- paste0(tempdir(), '/__TEMPTEMP_files.csv')

		rgrass::execGRASS('g.list', flags=flags, type='vector', output=paste0(tempdir(), '/__TEMPTEMP_files.csv'))
		vects <- tryCatch(
			read.table(paste0(tempdir(), '/__TEMPTEMP_files.csv')),
			error=function(e) FALSE
		)
		if (!is.logical(vects[1L])) {
			vects <- unlist(vects)
			names(vects) <- rep('vector', length(vects))
		} else {
			vects <- NULL
		}
		
		file.remove(outFile)
	
	}
	
	out <- c(rasts, vects)
	if (!incTemp) {
		keeps <- !grepl(out, pattern='__TEMPTEMP')
		out <- out[keeps]
	}
	
	out

}
