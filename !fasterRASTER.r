#' XXXXX
#'
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. This function utilizes the \pkg{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/XXXXX.html}{\code{XXXXX}} and is the same or at least similar to the \pkg{terra} function \code{\link[terra]{XXXXX}}.
#'
#' @param 
#' @param grassDir Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. Example for a Mac: \code{"/Applications/GRASS-8.2.app/Contents/Resources"}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName}. The latter is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with a name given by \code{outGrassName} is written into the GRASS session.
#'
#' @seealso \code{\link[terra]{XXXXX}} in the \pkg{terra} package
#' @examples
#'
#' \donttest{
#'
#' # change this to where GRASS is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#'
#'
#' }
#'
#' @export

fasterTEMPLATE <- function(
	rast,
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'rastBuffer',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (!is.na(ignore) && ignore == 0) flags <- c(flags, 'z')

	# initialize GRASS and export raster to GRASS
	input <- initGrass(alreadyInGrass=alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
		
	rgrass::execGRASS('XXXXXXXX', input=input, output=outGrassName, distances=width, units=units, flags=flags)

	# return
	if (grassToR) {

		out <- rgrass::read_RAST(outGrassName)
		names(out) <- outGrassName
		out

	}

}
