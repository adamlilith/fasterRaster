#' Convert a raster to a vector (points, lines, or polygons)
#'
#' Convert a raster to a spatial polygons object (points, lines, or polygons).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param vectType Character. Indicates type of output: \code{point}, \code{line} (not supported yet), or \code{area}.
#' @param agg Logical. If \code{TRUE} (default) then union all points/lines/polygons with the same value into the same "multipart" polygon. This may or may not be desirable. For example, if the raster is vectorized into a polygons object each cell will become a separate polygon. Using this option will merge cells with the same value (even if they are not spatially adjacent one another).
#' @param smooth Logical. If \code{TRUE} then "round" cell corners by connecting the midpoints of corner cells (which leaves out the corner-most triangle of that cell). This option only applies if \code{vectType} is \code{area}. Default is \code{FALSE}.
#' @param calcDensity Logical, if \code{TRUE} then calculate density in the moving window. This will create a raster named \code{density} in the \code{GRASS} environment if \code{grassToR} is \code{FALSE} or return a raster named \code{density} if \code{grassToR} is \code{TRUE}. Default is \code{FALSE}.
#' @param calcConnect Logical. If \code{TRUE} then calculate a connectivity raster (conditional probability a cell with a value of 1 has a value that is also 1) in the moving window. This will create a raster named \code{connect} in the \code{GRASS} environment if \code{grassToR} is \code{FALSE} or return a raster named \code{connect} if \code{grassToR} is \code{TRUE}. Default is \code{FALSE}.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for converting a raster to a vector (i.e., function \code{r.to.vect} in \pkg{GRASS}).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a SpatialPointsDataFrame, SpatialLinesDataFrame, or a SpatialPolygonsDataFrame with the same coordinate reference system as \code{rast}. The field named \code{value} will have the raster values. Regardless, vector object with the name given by \code{outGrassName} will be written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{as.points}}, \code{\link[terra]{as.polygons}}, and \code{\link[terra]{as.lines}} in \pkg{terra}; \code{\link[fasterRaster]{fasterRasterize}} in \code{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.to.vect.html}{\code{r.to.vect}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterVectorize.r
#'
#' @export

fasterVectorize <- function(
	rast,
	vectType,
	agg = TRUE,
	smooth = FALSE,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = ifelse(is.null(names(rast)), 'rast', names(rast)),
	outGrassName = 'rastToVect',
	...
) {

	if (!(vectType %in% c('point', 'line', 'area'))) stop('Argument "vectType" in function fasterVectorize() must be either "point", "line", or "area".')

	flags <- c('quiet', 'overwrite')
	if (smooth & vectType == 'area') flags <- c(flags, 's')
	
	# initialize GRASS
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)

	# vectorize
	rgrass::execGRASS('r.to.vect', input=input, output=outGrassName, type=vectType, flags=flags, ...)
	
	# get raster back to R
	if (grassToR) {
	
		out <- rgrass::read_VECT(outGrassName)
		
		# join output with same values
		if (agg) out <- terra::aggregate(out, by='value')
		out
		
	}

}
