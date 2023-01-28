#' Information on a raster or vector in a GRASS session
#'
#' Displays information on a raster or vector already in an R session. For further
#'
#' @param x The name of the raster(s) or vector(s) in the active \code{GRASS} session. These must be in quotes. If missing, then information on all rasters and vectors is reported.
#' @param rastOrVect Either \code{'rasters'} or \code{'vectors'} (one value per value in \code{x}). In \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{x} is a raster or vector (partial matching is supported). By default, the function will return information on each object in the \code{GRASS} session of the given name(s).
#' @return A \code{data.frame} (or \code{NULL} if no rasters or vectors were found).
#'
#' @seealso \code{\link{fasterLs}}, \code{\link{fasterExt}}, \code{\link{fasterDim}}, and \code{\link{fasterRes}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.info.html}{\code{r.info}} and \href{https://grass.osgeo.org/grass82/manuals/v.info.html}{\code{v.info}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterInfo <- function(
	x,
	rastOrVect = c('rasters', 'vectors')
) {

	rastOrVect <- .getRastOrVect(rastOrVect, n=Inf, nullOK=TRUE)

	if (missing(x)) {
		x <- fasterLs(rastOrVect = unique(rastOrVect))
		rastOrVect <- names(x)
	} else {
		rastOrVect <- rep(rastOrVect, length(x))
	}

	if (length(x) == 0L) {
		warning('No rasters or vectors found in the active GRASS session.')
		out <- NULL
	} else {
		
		spatials <- fasterLs(rastOrVect=unique(rastOrVect), temps=FALSE)
		if (any(!(x %in% spatials))) stop('At least one raster or vector in "x"\n is not in the active GRASS session.')
		
		out <- data.frame()
		for (i in seq_along(x)) {
		
			spatial <- x[i]
			rOrV <- rastOrVect[i]
		
			### raster information
			if (pmatch(rOrV, c('rasters', 'vectors')) == 1L) {

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

				# extent
				west <- info[grepl(info, pattern='west=')]
				east <- info[grepl(info, pattern='east=')]
				south <- info[grepl(info, pattern='south=')]
				north <- info[grepl(info, pattern='north=')]

				west <- sub(west, pattern='west=', replacement='')
				east <- sub(east, pattern='east=', replacement='')
				south <- sub(south, pattern='south=', replacement='')
				north <- sub(north, pattern='north=', replacement='')
				
				west <- as.numeric(west)
				east <- as.numeric(east)
				south <- as.numeric(south)
				north <- as.numeric(north)
			
				# dimensions
				rows <- info[grepl(info, pattern='rows=')]
				cols <- info[grepl(info, pattern='cols=')]

				rows <- sub(rows, pattern='rows=', replacement='')
				cols <- sub(cols, pattern='cols=', replacement='')
				
				rows <- as.numeric(rows)
				cols <- as.numeric(cols)
				
				# resolution
				ewres <- info[grepl(info, pattern='ewres=')]
				nsres <- info[grepl(info, pattern='nsres=')]
			
				ewres <- sub(ewres, pattern='ewres=', replacement='')
				nsres <- sub(nsres, pattern='nsres=', replacement='')
				
				ewres <- as.numeric(ewres)
				nsres <- as.numeric(nsres)
			
				# data type
				grassDataType <- info[grepl(info, pattern='datatype=')]
				grassDataType <- sub(grassDataType, pattern='datatype=', replacement='')
				
				if (grassDataType == 'CELL') {
					terraDataType <- 'INT4S'
					gdalDataType <- 'Int32'
				} else if (grassDataType == 'FCELL') {
					terraDataType <- 'FLT4S'
					gdalDataType <- 'Float32'
				} else if (grassDataType == 'DCELL') {
					terraDataType <- 'FLT8S'
					gdalDataType <- 'Float64'
				}
				
				# number of categories
				numCategories <- info[grepl(info, pattern='ncats=')]
				numCategories <- sub(numCategories, pattern='ncats=', replacement='')
				numCategories <- as.numeric(numCategories)

				out <- rbind(
					out,
					data.frame(
						name = spatial,
						type = 'raster',
						
						west = west,
						east = east,
						south = south,
						north = north,
					
						rows = rows,
						cols = cols,
					
						ewres = ewres,
						nsres = nsres,
						
						grassDataType = grassDataType,
						terraDataType = terraDataType,
						gdalDataType = gdalDataType,
						
						numCategories = numCategories,
						
						bottom = NA,
						top = NA
					),
					make.row.names = FALSE
				)
			
			### vector information
			} else if (pmatch(rOrV, c('rasters', 'vectors')) == 2L) {
			
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

				# extent
				west <- info[grepl(info, pattern='west=')]
				east <- info[grepl(info, pattern='east=')]
				south <- info[grepl(info, pattern='south=')]
				north <- info[grepl(info, pattern='north=')]

				west <- sub(west, pattern='west=', replacement='')
				east <- sub(east, pattern='east=', replacement='')
				south <- sub(south, pattern='south=', replacement='')
				north <- sub(north, pattern='north=', replacement='')
				
				west <- as.numeric(west)
				east <- as.numeric(east)
				south <- as.numeric(south)
				north <- as.numeric(north)
			
				# top/bottom
				bottom <- info[grepl(info, pattern='bottom=')]
				top <- info[grepl(info, pattern='top=')]

				out <- rbind(
					out,
					data.frame(
						name = spatial,
						type = 'vector',
						
						west = west,
						east = east,
						south = south,
						north = north,
					
						rows = NA,
						cols = NA,
					
						ewres = NA,
						nsres = NA,
						
						grassDataType = NA,
						terraDataType = NA,
						gdalDataType = NA,
						
						numCategories = NA,
						
						bottom = bottom,
						top = top
					),
					make.row.names = FALSE
				)
			} else {
				stop('Argument "rastOrVect" must be "raster" or "vector" (one value per value in "x").')
			}
		
		} # next raster/vector
	
		n <- nchar(out$type)
	
		# want raster?
		subs <- rep(NA, length(out$type))
		for (i in seq_along(out$type)) subs[i] <- substr('rasters', 1, n[i])
		wantRasters <- any(out$type %in% subs)
		
		# want vector?
		subs <- rep(NA, length(out$type))
		for (i in seq_along(out$type)) subs[i] <- substr('vectors', 1, n[i])
		wantVectors <- any(out$type %in% subs)

		if (wantRasters & !wantVectors) out$top <- out$bottom <- NULL
		if (!wantRasters & wantVectors) out$rows <- out$cols <- out$ewres <- out$nsres <- out$grassDataType <- out$terraDataType <- out$gdalDataType <- out$numCategories <- NULL

	}
	
	out

}
