#' Coordinates of a GVector's features or a GRaster's cell centers
#'
#' @description Returns the coordinates of a `GVector`'s features or the coordinates of the center of cells of a `GRaster`. Note that if you simply want to convert a vector to a points vector, using [as.points()] is faster.
#'
#' @param x A `GVector` or a `GRaster`.
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
	function(x, z = TRUE) {
	.crdsVect(x, z = z)
	} # EOF
)

#' @rdname crds
#' @export
st_coordinates <- function(x, z = z) {
	if (inherits(x, 'GSpatial')) {
		.crdsVect(x, z = z)
	} else {
		sf::st_coordinates(x)
	}
}

# extract coordinates for vector
.crdsVect <- function(x, z = TRUE) {

	.restore(x)
	
	gm <- geomtype(x)

	# if lines or polygons, convert to points first
	if (gm %in% c('lines', 'polygons')) {

		stop('crds() will only work on points vectors.')
		
		# ####### NB seems to work on lines but disagrees with st_coordinates() and crds()

		# if (z && is.3d(x)) warning('z coordinates ignored.')
	
		# gn <- .makeGname('points', 'vect')
		# rgrass::execGRASS('v.to.points', input=gnames(x), output=gn, use='vertex', flags=c('quiet', 'overwrite'), intern=TRUE)
		# pts <- makeGVector(gn)
		# pts <- vect(pts)

		# out <- terra::crds(pts)

	} else if (gm == 'points') {
		
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
		
	if (getFastOptions('useDataTable')) out <- data.table::as.data.table(out)
	out
	
}

# extract coordinates for raster
methods::setMethod(
	f = 'crds',
	signature = c(x = 'GRaster'),
	function(x, z = TRUE, na.rm = TRUE) {

	.restore(x)
	region(x)
	
	flags <- c('quiet', 'overwrite')
	if (!na.rm) flags <- c(flags, 'i')
	
	temp <- paste0(tempfile(), '.csv')
	rgrass::execGRASS('r.out.xyz', input=gnames(x), output=temp, separator='comma', flags=flags, intern=TRUE)
	#see https://grass.osgeo.org/grass82/manuals/r.out.xyz.html
	out <- data.table::fread(temp)
	
	if (is.3d(x)) {
		names(out) <- c('x', 'y', 'z', names(x))
	} else {
		names(out) <- c('x', 'y', names(x))
	}
	
	# convert to numerics
	classes <- sapply(out, class)
	if (any(classes %in% 'character')) {
		chars <- which(classes == 'character')
		for (char in chars) out[ , ..char] <- as.numeric(out[ , ..char])
	}
	
	if (!getFastOptions('useDataTable')) out <- as.data.frame(out)
	out

	} # EOF
)

