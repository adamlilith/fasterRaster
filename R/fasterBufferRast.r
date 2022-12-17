#' Buffer cells of a raster
#'
#' Calculate a buffer around non-\code{NA} cells or cells with values of 0 in a raster. The output will be a raster. This function utilizes the \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.buffer.html}{\code{r.buffer}} and is the same or at least similar to the \pkg{terra} function \code{\link[terra]{buffer}}.

#' This function is a potentially faster version of the \code{\link[terra]{buffer}} function in the \pkg{terra} package for calculating a buffer around non-\code{NA} cells or cells with values of 0 in a raster. The output will be a raster.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' 
#' @param width Numeric. Maximum distance cells must be from focal cells to be within the buffer. Note that this function can only handle one value of \code{width} (unlike the function \code{r.buffer} in \code{GRASS}).
#' @param units Either \code{'meters'} (default), \code{'kilometers'}, \code{'feet'}, \code{'miles'}, or \code{'nautmiles'}. Indicates the units of \code{width}.
#' @param ignore Either {NA} (default) or 0. The buffer will be drawn around cells that are not {NA} or not 0, depending on this value.
#' @param lowMemory Logical. If \code{FALSE} (default) use faster, memory-intensive procedure. If \code{TRUE} then use the slower, low-memory version. To help decide, consider using the low-memory version on a system with 1 GB of RAM for a raster larger than about 32000x32000 cells, or for a system with  with 8 GB of RAM a raster larger than about 90000x90000 cells.
#'
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with a name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{buffer}} in the \pkg{terra} package
#'
#' @examples man/examples/ex_fasterBufferRast.r
#'
#' @export

fasterBufferRast <- function(
	rast,
	width,
	units = 'meters',
	ignore = NA,
	lowMemory = FALSE,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	outGrassName = 'rastBuffer',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (!is.na(ignore) && ignore == 0) flags <- c(flags, 'z')

	# initialize GRASS and export raster to \code{GRASS}
	input <- initGrass(rast=rast, vect=NULL, grassDir=grassDir)
		
	# buffer
	fx <- if (lowMemory) {
		'r.buffer.lowmem'
	} else {
		'r.buffer'
	}

	rgrass::execGRASS(fx, input=input, output=outGrassName, distances=width, units=units, flags=flags)

	# return
	if (grassToR) {

		out <- rgrass::read_RAST(outGrassName)
		names(out) <- outGrassName
		out

	}

}
