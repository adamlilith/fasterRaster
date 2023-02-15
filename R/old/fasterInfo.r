#' Information on a raster or vector in a GRASS session
#'
#' Displays information on a raster or vector already in an R session.
#'
#' @param x Either missing (default), or the name of the raster(s) and/or vector(s) in the active \code{GRASS} session (i.e., in quotes). If missing, then information on all rasters and vectors is reported.
#' @param rastOrVect Either \code{NULL} (default), \code{'rasters'}, or \code{'vectors'}. If \code{NULL}, then the function will return information on all rasters and vectors. In \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{x} is a raster or vector (partial matching is supported).
#' @param ... Additional arguments to send to \code{\link{fasterLs}}. These include argument \code{temps} (\code{TRUE} or \code{FALSE}, which  determine if information on temporary files is returns. Temporary rasters and vectors start with "\code{TEMPTEMP_}".
#'
#' @return A \code{data.frame} (or \code{NULL} if no rasters or vectors were found).
#'
#' @seealso \code{\link{fasterLs}}, \code{\link{fasterExt}}, \code{\link{fasterDim}}, and \code{\link{fasterRes}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.info.html}{\code{r.info}} and \href{https://grass.osgeo.org/grass82/manuals/v.info.html}{\code{v.info}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterInfo <- function(
	x,
	rastOrVect = NULL,
	...
) {

	# clean and validate inputs
	info <- .rastOrVectAndX(x=x, rastOrVect=rastOrVect, ...)
	x <- info$x
	rastOrVect <- info$rastOrVect

	# get information on each object
	out <- data.frame()
	for (i in seq_along(x)) {
	
		spatial <- x[i]
		rov <- rastOrVect[i]
	
		### raster information
		if (pmatch(rov, c('rasters', 'vectors')) == 1L) {

			suppressMessages(
				info1 <- rgrass::execGRASS(
					'r.info',
					flags = 'g',
					map = spatial,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			suppressMessages(
				info2 <- rgrass::execGRASS(
					'r.univar',
					flags = c('r'),
					map = spatial,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			# extent
			west <- info1[grepl(info1, pattern='west=')]
			east <- info1[grepl(info1, pattern='east=')]
			south <- info1[grepl(info1, pattern='south=')]
			north <- info1[grepl(info1, pattern='north=')]

			west <- sub(west, pattern='west=', replacement='')
			east <- sub(east, pattern='east=', replacement='')
			south <- sub(south, pattern='south=', replacement='')
			north <- sub(north, pattern='north=', replacement='')
			
			west <- as.numeric(west)
			east <- as.numeric(east)
			south <- as.numeric(south)
			north <- as.numeric(north)
		
			# dimensions
			rows <- info1[grepl(info1, pattern='rows=')]
			cols <- info1[grepl(info1, pattern='cols=')]

			rows <- sub(rows, pattern='rows=', replacement='')
			cols <- sub(cols, pattern='cols=', replacement='')
			
			rows <- as.numeric(rows)
			cols <- as.numeric(cols)
			
			# resolution
			ewres <- info1[grepl(info1, pattern='ewres=')]
			nsres <- info1[grepl(info1, pattern='nsres=')]
		
			ewres <- sub(ewres, pattern='ewres=', replacement='')
			nsres <- sub(nsres, pattern='nsres=', replacement='')
			
			ewres <- as.numeric(ewres)
			nsres <- as.numeric(nsres)
		
			# data type
			grassDataType <- info1[grepl(info1, pattern='datatype=')]
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
			numCategories <- info1[grepl(info1, pattern='ncats=')]
			numCategories <- sub(numCategories, pattern='ncats=', replacement='')
			numCategories <- as.numeric(numCategories)

			# range of values
			if (numCategories == 0) {

				minVal <- info2[grepl(info2, pattern='minimum: ')]
				maxVal <- info2[grepl(info2, pattern='maximum: ')]
				
				minVal <- sub(minVal, pattern='minimum: ', replacement='')
				maxVal <- sub(maxVal, pattern='maximum: ', replacement='')
				
				minVal <- as.numeric(minVal)
				maxVal <- as.numeric(maxVal)

			} else {
				minVal <- NA
				maxVal <- NA
			}
			
			out <- rbind(
				out,
				data.frame(
					name = spatial,
					type = 'raster',
					topology = NA,
					
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
					minVal = minVal,
					maxVal = maxVal,
					
					bottom = NA,
					top = NA
				),
				make.row.names = FALSE
			)
		
		### vector information
		} else if (pmatch(rov, c('rasters', 'vectors')) == 2L) {
		
			suppressMessages(
				info1 <- rgrass::execGRASS(
					'v.info',
					flags = 'g',
					map = spatial,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)
			
			# topology
			topology <- vectTopo(spatial)

			# extent
			west <- info1[grepl(info1, pattern='west=')]
			east <- info1[grepl(info1, pattern='east=')]
			south <- info1[grepl(info1, pattern='south=')]
			north <- info1[grepl(info1, pattern='north=')]

			west <- sub(west, pattern='west=', replacement='')
			east <- sub(east, pattern='east=', replacement='')
			south <- sub(south, pattern='south=', replacement='')
			north <- sub(north, pattern='north=', replacement='')
			
			west <- as.numeric(west)
			east <- as.numeric(east)
			south <- as.numeric(south)
			north <- as.numeric(north)
		
			# top/bottom
			bottom <- info1[grepl(info1, pattern='bottom=')]
			top <- info1[grepl(info1, pattern='top=')]
			
			bottom <- sub(bottom, pattern='bottom=', replacement='')
			top <- sub(bottom, pattern='top=', replacement='')

			bottom <- as.numeric(bottom)
			top <- as.numeric(top)

			out <- rbind(
				out,
				data.frame(
					name = spatial,
					type = 'vector',
					topology = topology,
					
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
					minVal = NA,
					maxVal = NA,
					
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

	# want raster/vectors?
	wantRasters <- any('raster' %in% out$type)
	wantVectors <- any('vector' %in% out$type)
	
	if (wantRasters & !wantVectors) out$top <- out$bottom <- NULL
	if (!wantRasters & wantVectors) out$rows <- out$cols <- out$ewres <- out$nsres <- out$grassDataType <- out$terraDataType <- out$gdalDataType <- out$numCategories <- out$minVal <- out$maxVal <- NULL
	
	out

}
