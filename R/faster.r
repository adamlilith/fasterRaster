#' Generic function to call a \code{GRASS} function (module)
#'
#' This function is wrapper for \code{\link[rgrass]{execGRASS}}, plus code necessary to initiate a \code{GRASS} session. Many of the functions in \pkg{fasterRaster} actually utilize this function.  This function works best for modules that take one raster and/or one vector as input and produce one raster or vector as output.
#' @param mod Name of \code{GRASS} module (e.g., \code{'r.latlong'} or \code{'v.buffer'}.
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_inRastName_plural
#' @inheritParams .sharedArgs_inVectName
#' @param rast A \code{SpatRaster} or the name of a raster already in an existing \code{GRASS} session. If the \code{GRASS} module does not use a raster, this can be \code{NULL}.
#' @param vect A \code{Spatvector}, an \code{sf} object, or the name of a spatial vector already in an existing \code{GRASS} session. If the \code{GRASS} module does not use a vector, this can be \code{NULL}.
#' @param out \code{NULL}, \code{'raster'}, or \code{'vector'} (partial string matching is used). Type of output expected from the \code{GRASS} module.
#' @param flags List of flags for the \code{GRASS} module. The default (\code{c('quiet', 'overwrite')}) causes the module to report little/no messages and to overwrite existing files of the same name. For more flags, see the help documentation for the respective \code{GRASS} module.
#' @param ... Arguments to pass to the \code{GRASS} module through \code{\link[rgrass]{execGRASS}}.
#'
#' @return A raster, vector, or some other product of the \code{GRASS} module.
#'
#' @seealso See the \code{OS Geo GRASS} websit for a catalog and detailed descriptions of \href{https://grass.osgeo.org/grass82/manuals/vector.html}{vector} and \href{https://grass.osgeo.org/grass82/manuals/raster.html}{raster} modules. Related functions include \code{\link[rgrass]{execGRASS}} in \pkg{rgrass}, and \code{\link{exportRastToGrass}} and \code{\link{exportVectToGrass}} for exporting rasters and vectors to an existing \code{GRASS} session.
#'
#' @example man/examples/ex_faster.r
#'
#' @export

faster <- function(
	mod,
	rast = NULL,
	vect = NULL,
	...,
	out = NULL,
	flags = c('quiet', 'overwrite'),
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = NULL,
	inVectName = NULL,
	outGrassName = 'output'
) {

	args <- list(cmd=mod, flags=flags)
	args <- c(args, 'output' = outGrassName)
	args <- c(args, list(...))

	# execute
	if (
		(inherits(rast, c('SpatRaster', 'Raster')) & inherits(vect, 'character')) |
		(inherits(rast, 'character') & inherits(vect, c('SpatVector', 'sf', 'Spatial')))
	) {
	
		stop('Argument "rast" must be:\n* NULL;\n* the name of a raster already in a GRASS session; or\n* a SpatRaster.\nArgument "vect" must be:\n* NULL;\n* the name of a vector already in a GRASS session; or\n* a SpatVector or sf object.\nBoth "rast" and "vect" cannot be NULL simultaneously.\nOne cannot be a name and the other a raster/vector.')
		
	} else if (!is.null(rast) | !is.null(vect)) {

		input <- initGrass(rast=rast, vect=vect, inRastName=inRastName, inVectName=inVectName, grassDir=grassDir)
		
		args <- if (!is.null(rast) & is.null(vect)) {
			c(args, input=input[['rastNameInGrass']])
		} else if (is.null(rast) & !is.null(vect)) {
			c(args, input=input[['vectNameInGrass']])
		} else {
			c(args, r=input[['rastNameInGrass']], v=input[['vectNameInGrass']])
		}
	
	}
	
	do.call(rgrass::execGRASS, args=args)
	
	if (grassToR) {

		if (pmatch(out, c('raster', 'vector')) == 1) {
			
			x <- rgrass::read_RAST(outGrassName, flags='quiet')
			names(x) <- outGrassName
		
		} else if (pmatch(out, c('raster', 'vector')) == 2) {
		
			x <- rgrass::read_VECT(outGrassName, flags='quiet')
			if (!is.null(options()$grassVectOut) && !is.na(options()$grassVectOut)) {
				if (options()$grassVectOut == 'sf') x <- sf::st_as_sf(x)
			}
			
			
		}
			
		x
		
	}

}
