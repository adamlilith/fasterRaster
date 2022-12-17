#' XXXXX
#'
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX. This function utilizes the \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/XXXXX.html}{\code{XXXXX}} and is the same or at least similar to the \pkg{terra} function \code{\link[terra]{XXXXX}}.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_grassDir_grassToR_outGrassName
#' 
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a vector. Regardless, a vector with a name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{XXXXX}} in the \pkg{terra} package
#' @examples
#'
#' \donttest{
#'
#' # change this to where \code{GRASS} is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#'
#'
#' }
#'
#' @export

fasterTEMPLATE <- function(
	vect,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	outGrassName = 'vectXXXXX',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (!is.na(ignore) && ignore == 0) flags <- c(flags, 'z')

	# initialize \code{GRASS} and export raster to \code{GRASS}
	input <- initGrass(rast=NULL, vect=vect, grassDir=grassDir)
		
	rgrass::execGRASS('XXXXXXXX', input=input, output=outGrassName, distances=width, units=units, flags=flags)

	# return
	if (grassToR) {

		out <- rgrass::read_RAST(outGrassName)
		names(out) <- outGrassName
		out

	}

}
