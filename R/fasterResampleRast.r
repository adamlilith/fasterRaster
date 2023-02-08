#' Resample a raster (change spatial resolution)
#'
#' Resample a raster to a different spatial resolution.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template Any of:
#' \itemize{
#'		\item A \code{SpatRaster}, used to define the new resolution. This object is \emph{not} exported to the \code{GRASS} session.
#'		\item The name of raster already in the \code{GRASS} session.
#'		\item A number indicating the new spatial resolution (cell size in east-west and north-south directions).
#'		\item Two numbers indicating the new spatial resolution in east-west and north-south directions, respectively.
#' }
#' @param catRast If \code{FALSE} (default), then the values in \code{rast} are assumed to be numeric and continuous. If code{FALSE}, then the unique values in \code{rast} are assumed to be categories.
#' @param method Method used for interpolation:
#' \itemize{
#'	\item \code{'bilinear'} (default if \code{catRast} if \code{FALSE}): Uses values from 4 cells
#'	\item \code{'bicubic'}: Uses values from 16 cells
#'	\item \code{'lanczos'}: Uses values from 25 cells
#'	\item \code{'nearest'} (default if \code{catRast} is \code{TRUE}): Uses value from 1 cell
#' }
#' 
#' @return A \code{SpatRaster}. Also creates a raster in a new grass session named \code{outGrassName}.
#' 
#' @seealso \code{\link{fasterRes}} and \code{\link{fasterAggregate}} in \pkg{fasterRaster}; \code{\link[terra]{resample}} and  \code{\link[terra]{aggregate}} in package \pkg{terra}; \code{GRASS} modules \href{https://grass.osgeo.org/grass82/manuals/r.resample.interp.html}{\code{r.resample.interp}} for rasters with continuous values and \href{https://grass.osgeo.org/grass82/manuals/r.resample.html}{\code{r.resample}} for rasters with categorical values
#' 
#' @examples man/examples/ex_fasterCropExtendRast.r
#'
#' @export

fasterResampleRast <- function(
	rast,
	inRastName,
	template,
	method = if (catRast) { 'nearest' } else { 'bilinear' },
	catRast = FALSE,
	outGrassName = 'croppedRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)
	inRastName <- .getInRastName(inRastName, rast)
	
	if (is.null(inits)) inits <- list()

	# initialize GRASS
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)

	# get resolution of the template
	if (inherits(template, 'SpatRaster')) {
		template <- terra::res(template)
	} else if (inherits(template, 'character')) {
		template <- fasterRes(template)
	} else if (inherits(template, 'numeric')) {
		if (length(template) == 1L) template <- rep(template, 2L)
		if (length(template) > 2L) stop('Argument "template" must be a raster in R or GRASS,\nor a numeric with one or two values.')
	} else {
		stop('Argument "template" must be a raster in R or GRASS, or a numeric with one or two values.')
	}

	# resize region
	success <- resizeRegion()
	
	# categorical?
	if (catRast) {
		rgrass::execGRASS('r.resample', input=inRastName, output=outGrassName, flags=flags)
	} else {
		rgrass::execGRASS('r.resample.interp', input=inRastName, output=outGrassName, method=method, flags=flags)
	}
	
	# resize region to encompass all
	success <- resizeRegion()

	# return
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out

	} else { invisible(TRUE) }

}
