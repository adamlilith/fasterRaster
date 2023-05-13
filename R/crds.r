#' Coordinates of a GVector's features or a GRaster's cell centers
#'
#' @description Returns the coordinates of a `GVector`'s features or the coordinates of the center of cells of a `GRaster`. Note that if you simply want to convert a vector to a points vector, using [as.points()] is faster.
#'
#' @param x A `GVector` or a `GRaster`.
#' @param df Logical: If `TRUE`, return a `data.frame` instead of a `matrix`.
#' @param list Logical: If `TRUE`, return a `list` instead of a `matrix`.
#' @param z If `FALSE` (default), return only x- and y-coordinates. If `TRUE`, return x-, y-, and z-coordinates. For 2-dimensional objects, z-coordinates will all be 0.
#' @param na.rm Logical: If `TRUE`, remove cells that are `NA` (`GRaster`s only).
#'
#' @returns A `matrix`, `data.frame`, or `list`.
#'
#' @seealso [as.points()], [terra::crds()]
#'
#' @example man/examples/ex_crds.r
#'
#' @aliases crds
#' @rdname crds
#' @exportMethod crds
methods::setMethod(
	f = 'crds',
	signature = c(x = 'GVector'),
	function(x, df = FALSE, list = FALSE, z = TRUE) {
	.crdsVect(x, df = df, list = list, z = z)
	} # EOF
)

#' @aliases st_coordinates
#' @rdname crds
#' @exportMethod st_coordinates
methods::setMethod(
	f = 'crds',
	signature = c(x = 'GVector'),
	function(x, z = TRUE) {
	.crdsVect(x, df = FALSE, list = FALSE, z = z)
	} # EOF
)

# extract coordinates for vector
.crdsVect <- function(x, df = FALSE, list = FALSE, z = TRUE) {

	.restore(x)
	
	gm <- geomtype(x)

	# if lines or polygons, convert to points first
	if (gm %in% c('lines', 'polygons')) {
	
		stop('crds() only works with points vectors.')
	
		# gn <- .makeGname('vectToPts', 'vector')
		# rgrass::execGRASS('v.to.points', input=gnames(x), output = gn, use='vertex', flags=c('t', 'quiet', 'overwrite'), intern=TRUE)
		# x <- makeGVector(gn)

		# rgrass::execGRASS('v.out.ascii', input=gnames(x), output = 'C:/ecology/!Scratch/_pts.csv', type='point', format='point', flags=c('quiet', 'overwrite'), intern=TRUE)
		# rgrass::execGRASS('v.out.ascii', input=gnames(x), output = 'C:/ecology/!Scratch/_pts.csv', format='point', flags=c('quiet', 'overwrite'), intern=TRUE)
		# rgrass::execGRASS('v.out.ascii', input=gnames(x))
	
	}
	
	if (gm == 'points') {
		
		data <- rgrass::execGRASS('v.to.db', map=gnames(x), flags='p', option='coor', type='point', intern=TRUE)
		data <- data[-1L]
		cutAt <- which(data == 'Reading features...')
		data <- data[1L:(cutAt - 1L)]
		
		data <- strsplit(data, split='\\|')
		data <- lapply(data, as.numeric)
		out <- do.call(rbind, data)
		
		if (z) {
			out <- out[ , 2L:4L]
			colnames(out) <- c('x', 'y', 'z')
		} else {
			out <- out[ , 2L:3L]
			colnames(out) <- c('x', 'y')
		}
		
	}	
		
	if (list) {
		out <- as.list(out)
	} else if (!df) {
		out <- as.matrix(out)
	}
		
	out
	
}

# extract coordinates for raster
methods::setMethod(
	f = 'crds',
	signature = c(x = 'GRaster'),
	function(x, df = FALSE, na.rm = TRUE, z = TRUE) {

	.restore(x)
	regionShape(x)
	
	stop('TBD; see: https://grass.osgeo.org/grass82/manuals/r.out.xyz.html')

	} # EOF
)

