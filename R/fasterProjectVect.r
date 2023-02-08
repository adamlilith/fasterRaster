#' Project a spatial vector to a different coordinate reference system
#'
#' Project a spatial vector to a different coordinate reference system. This function works by transferring a vector between two \code{GRASS} \href{location}s with different coordinate reference systems. You can use the default "from" location from which the raster is transferred, or name your own. By default, you do not need to name the destination location (which is named "default"), or you can specify one using the \code{location} argument (supplied in the \code{...}).
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template This provides the "destination" coordinate reference system for the vector. If left as \code{NULL} (default), then it is assumed that there is a current, active \code{GRASS} session, and the vector will be projected to its coordinate reference system. Otherwise, this can also be a \code{SpatVector}, \code{SpatRaster}, \code{sf} object, or the name of a vector or raster in a \code{GRASS} \href{location} that provides a coordinate reference system to which the vector will be projected.
#'	
#' @param inTemplateName The name of the \code{SpatVector} template. Ignored if \code{template} is the name of an object existing in the active \href{location} (i.e., a character).
#'
#' @param buildTopo If \code{FALSE} (default), then vector topology is built. This can take significant time for large vectors. Setting this to \code{FALSE} will not build topology, but is usually only recommended for cases where you will not be using the vector further in \code{GRASS} (i.e., the purpose is to project the vector then import it back to \code{R} or to save it to disk).
#'
#' @param fromLocation The name of the \href{location} from which the vector in \code{vect} will be projected from. If this location does not exist, it will be created, but it can also be a pre-existing location created by another \pkg{fasterRaster} function or using \code{\link{startFaster}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a vector with the same coordinate reference system as \code{template}. Regardless, a vector with the name given by \code{outGrassName} is written into the active \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{project}} in \pkg{terra}; \code{\link{st_transform}} in the \pkg{sf} package; \href{https://grass.osgeo.org/grass82/manuals/v.proj.html}{\code{v.proj}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterProjectVect.r
#'
#' @export

fasterProjectVect <- function(
	vect,
	template = NULL,
	inVectName = NULL,
	inTemplateName = NULL,
	buildTopo = TRUE,
	fromLocation = 'fromLocation',
	outGrassName = inVectName,

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	ells <- list(...)

	### commons v1
	##############

		### arguments
		
		if (!inherits(vect, 'character') & !inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
		inVectName <- .getInVectName(inVectName, vect=vect)
		if (.getSessionStarted() & 'location' %in% names(ells)) {
			ells$location <- NULL
		}
		.checkVectExists(replace=replace, vect=vect, inVectName=inVectName, location=fromLocation, unlist(ells))
		if (is.null(outGrassName)) outGrassName <- inVectName

		if (!is.null(template)) {
			if (inherits(template, 'SpatRaster')) {
				inTemplateName <- .getInRastName(inTemplateName, rast=template)
				.checkRastExists(replace=replace, rast=template, inRastName=inTemplateName, outGrassName=NULL, ...)
			}
			if (inherits(template, c('SpatVector', 'sf'))) {
				inTemplateName <- .getInVectName(inTemplateName, vect=template)
				.checkRastExists(replace=replace, vect=template, inVectName=inTemplateName, outGrassName=NULL, ...)
			}
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterProjectVect')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons
	
	### errors?
	
	### function-specific

	### export focal vector into "from" location
	initsFrom <- inits
	initsFrom$vect <- vect
	initsFrom$inVectName <- inVectName
	initsFrom$location <- fromLocation
	if ('dir' %in% names(inits)) initsFrom$dir <- ells$dir
	fromGrass <- do.call('startFaster', initsFrom)

	### export template to current location or switch to pre-existing location
	inits$vect <- inits$rast <- inits$inVectName <- inits$inRastName <- NULL
	if (!is.null(template)) {
		if (inherits(template, 'SpatRaster')) {
			inits$rast <- template
			inits$inRastName <- inTemplateName
		} else if (inherits(template, c('SpatVector', 'sf'))) {
			inits$vect <- template
			inits$inVectName <- inTemplateName			
		}
	} else {
		inits$restartGrass <- FALSE
	}
	
	if (!('location' %in% inits)) location <- 'default'

	do.call('startFaster', inits)

	### function-specific
	if (!buildTopo) flags <- c(flags, 'b')

	### transfer vector to the target location
	args <- list(
		cmd = 'v.proj',
		input = inVectName,
		output = outGrassName,
		location = fromLocation,
		mapset = 'PERMANENT',
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	do.call(rgrass::execGRASS, args)

	# return
	if (grassToR) {

		out <- fasterWriteVector(outGrassName, paste0(tempfile(), '.gpkg'), flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out
		
	} else { invisible(TRUE) }
	
}
