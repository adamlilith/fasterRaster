#' User-defined calculations on a single raster
#'
#' This function creates a raster from another raster based on a user-defined function. It utilizes the GRASS module \code{r.mapcalc}.
#' @param ... A set of rasters, raster stacks, raster bricks, or the names rasters in an existing GRASS session, or any combination thereof.
#' @param expression Character. Formula to evaluate. Below are examples where the input raster is named "rast" in the GRASS session and the output raster will be named "out". Note that rasters passed as rasters/stacks/bricks must be referred to in formulae using their raster name (not name in R). In other words, in the examples below we could call a raster in R \code{myRast} but \code{names(myRast)} yields \code{rast}, so we need to call it "\code{rast}" in the formulae. The raster "\code{out}" will be created by the function.
#' \itemize{
#' \item Take the input raster "rast", multiplies each cell by 0, then adds 1 to each cell: \code{'out = (rast * 0) + 1'}
#' \item Take the input raster "rast", multiplies each cell by 0, then adds 1 to each cell: \code{'out = rast^2'}
#' \item Take the input raster "rast" and redefines each cell value to 17: \code{'out = 17'}
#' \item Take the input raster "rast" and redefines each cell value to equal the sum of it and the four cells in the rooks's neighborhood (north, south, east, west of it): \code{'out = rast[0, 0] + rast[-1, 0] + rast[0, -1] + rast[0, 1] + rast[1, 0]'}
#' \item Low-pass (averaging) filter across a 3x3 neighborhood: \code{'out = (rast[-1, -1] + rast[-1, 0] + rast[-1, 1] + rast[0, -1] + rast[0, 0] + rast[0, 1] + rast[1, -1] + rast[1, 0] + rast[1, 1]) / 9'}
#' \item High-pass ("edge-finding") filter across a 3x3 neighborhood: \code{'out = -0.7 * rast[-1, -1] -1 * rast[-1, 0] -0.7 * rast[-1, 1] -1 * rast[0, -1] + 6.8 * rast[0, 0] -1 * rast[0, 1] -0.7 * rast[1, -1] -1 * rast[1, 0] -0.7 * rast[1, 1]'}
#' }
#' See the documentation for \code{r.mapcalc}{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html} for more information. Note that some operations that may make sense in GRASS may not make sense when exported back to R (e.g., changing color).
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName} \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, extent, and resolution as \code{rast}. Regardless, a raster by a name given in \code{expression} (before the "=" symbol) is written into the GRASS session.
#' @details See the documentation for the GRASS module \code{r.mapcalc}{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html}. The function \code{\link{fasterFocal}} \emph{may} be faster for focal calculations.
#' @seealso \code{\link[terra]{calc}}, \code{\link[terra]{focal}}, \code{\link{fasterFocal}}
#' @examples
#'
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#'
#' data(madElev)
#' names(madElev) <- 'madElev'
#' ex <- 'out = madElev^2'
#' madElev2 <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
#' plot(stack(madElev, madElev2))
#'
#' ex <- 'madElev_17 = 17'
#' m_17 <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
#' plot(stack(madElev, m_17))
#'
#' # low-pass filter
#' ex <- 'lowPass = (madElev[-1, -1] + madElev[-1, 0] + madElev[-1, 1] + madElev[0, -1] + madElev[0, 0] + madElev[0, 1] + madElev[1, -1] + madElev[1, 0] + madElev[1, 1]) / 9'
#' lp <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
#' plot(stack(madElev, lp))
#'
#' # high-pass filter
#' ex <- 'highPass = -0.7 * madElev[-1, -1] -1 * madElev[-1, 0] -0.7 * madElev[-1, 1] -1 * madElev[0, -1] + 6.8 * madElev[0, 0] -1 * madElev[0, 1] -0.7 * madElev[1, -1] -1 * madElev[1, 0] -0.7 * madElev[1, 1]'
#' hp <- fasterMapcalc(madElev, expression=ex, grassDir=grassDir)
#' plot(stack(madElev, hp))
#'
#' # Using some rasters already in a GRASS session:
#' # Note we are passing raster "hp" as a raster, not as something already in
#' # GRASS (even though it is). Also, we need to refer to the "hp" raster by
#' # its raster name in the formula (ie, see "names(hp)") since it is passed in
#' # as a raster.
#'
#' input <- initGrass(FALSE, rast=madElev, rastName='madElev', grassDir=grassDir)
#' exportRastToGrass(lp, grassName='lp')
#' ex <- 'out = (lp * highPass) / madElev'
#' mapcalc <- fasterMapcalc('madElev', 'lp', hp, expression=ex,
#' grassDir=grassDir, alreadyInGrass=TRUE)
#' mapcalc
#'
#' # note differences between r.mapcalc and raster function math:
#' mapcalc
#' rast <- (lp * hp) / madElev
#' rast
#'
#' # but they largely look the same
#' plot(stack(mapcalc, rast))
#' plot(mapcalc - rast)
#' }
#' @export

fasterMapcalc <- function(
	...,
	expression,
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	grassToR = TRUE
) {

	flags <- c('quiet', 'overwrite')
	
	# get just rasters
	rasts <- list(...)
	rasts <- rasts[which(sapply(rasts, class) %in% c('RasterLayer', 'RasterBrick', 'RasterStack'))]
	numRasts <- length(rasts)
	if (numRasts > 0) rastNames <- unlist(sapply(rasts, names))
	
	# initialize GRASS
	if (numRasts > 0) {
		input <- initGrass(FALSE, rast=rasts[[1]], vect=NULL, rastName=rastNames[1], grassDir=grassDir)

		# export remaining rasters
		if (numRasts > 1) {
			for (i in 2:length(rasts)) {
				exportRastToGrass(rasts[[i]], grassName=rastNames[i])
			}
		}
		
	}
	
	# execute
	rgrass::execGRASS('r.mapcalc', expression=expression, flags=flags)
	
	if (grassToR) {

		outRastName <- substr(expression, 1, regexpr(pattern='=', expression) - 1)
		outRastName <- raster::trim(outRastName)
		out <- rgrass::read_RAST(outRastName)
		out <- raster::raster(out)
		names(out) <- outRastName
		out
		
	}
	
}
