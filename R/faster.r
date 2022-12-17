#' Generic function to call a \code{GRASS} function (module)
#'
#' This function is wrapper for \code{\link[rgrass]{execGRASS}}, plus code necessary to initiate a \code{GRASS} session. Many of the functions in \pkg{fasterRaster} actually utilize this function.  This function works best for modules that take one raster and/or one vector as input and produce one raster or vector as output.
#' @param mod Name of \code{GRASS} module (e.g., \code{'r.latlong'} or \code{'v.buffer'}.
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @param rast A \code{SpatRaster} or the name of a raster already in an existing \code{GRASS} session. If the \code{GRASS} module does not use a raster, this can be \code{NULL}.
#' @param vect A \code{Spatvector}, an \code{sf} object, or the name of a spatial vector already in an existing \code{GRASS} session. If the \code{GRASS} module does not use a vector, this can be \code{NULL}.
#' @param rastName Character. Name of the input raster (if any) when exported to \pkg{GRASS}. Default is \code{'rast'}.
#' @param vectName Character. Name of the input vector (if any) when exported to \pkg{GRASS}. Default is \code{'vect'}.
#' @param outType \code{NULL}, \code{'raster'}, or \code{'vector'} (partial string matching is used). Type of output expected from the \code{GRASS} module.
#' @param flags List of flags for the \code{GRASS} module. The default (\code{c('quiet', 'overwrite')}) causes the module to report little/no messages and to overwrite existing files of the same name. For more flags, see the help documentation for the respective \code{GRASS} module.
#' @param ... Arguments to pass to the \code{GRASS} module through \code{\link[rgrass]{execGRASS}}.
#'
#' @return A raster, vector, or some other product of the \code{GRASS} module.
#'
#' @details For a list of vector commands, please see <https://grass.osgeo.org/grass82/manuals/vector.html>. For a list of raster commands, please see <https://grass.osgeo.org/grass82/manuals/raster.html>.
#'
#' @seealso \code{\link[rgrass]{execGRASS}}
#'
#' @examples man/examples/ex_faster.r
#'
#' @export

faster <- function(
	mod,
	rast = NULL,
	vect = NULL,
	rastName = 'rast',
	vectName = 'vect',
	outType = NULL,
	output = 'output',
	flags = c('quiet', 'overwrite'),
	...,
	grassDir = options()$grassDir,
	grassToR = TRUE
) {

	args <- list(cmd=mod, flags=flags)
	args <- c(args, ...)
	args <- c(args, 'output' = output)

	# execute
	if (
		(inherits(rast, c('SpatRaster', 'Raster')) & inherits(vect, 'character')) |
		(inherits(rast, 'character') & inherits(vect, c('SpatVector', 'sf', 'Spatial')))
	) {
	
		stop('Argument "rast" must be:\n* NULL;\n* the name of a raster already in a GRASS session; or\n* a SpatRaster.\nArgument "vect" must be:\n* NULL;\n* the name of a vector already in a GRASS session; or\n* a SpatVector or sf object.\nBoth "rast" and "vect" cannot be NULL simultaneously.\nOne cannot be a name and the other a raster/vector.')
		
	} else if (!is.null(rast) | !is.null(vect)) {
		input <- initGrass(rast=rast, vect=vect, rastName=rastName, vectName=vectName, grassDir=grassDir)
		do.call(rgrass::execGRASS, args=args)
		# rgrass::execGRASS(mod, flags=flags, ...)
	} else {
		rgrass::execGRASS(mod, flags=flags, ...)
	}
	
	if (grassToR) {

		if (pmatch(outType, c('raster', 'vector')) == 1) {
			
			out <- rgrass::read_RAST(output)
			out <- terra::rast(out)
			names(out) <- output
		
		} else if (pmatch(outType, c('raster', 'vector')) == 2) {
		
			out <- rgrass::readVECT(output)
			
		}
			
		out
		
	}

}
