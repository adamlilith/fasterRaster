#' Voronoi tessellation
#'
#' @description This function creates a Voronoi tessellation from a set of spatial points or polygons.
#'
#' @param x A `GVector` "points" object.
#' @param buffer Numeric: By default, this function creates a vector that has an extent exactly the same as the input data. However, the apparent extent can be changed by setting this value to one different from 0. Negative values reduce the size of the extent, and positive extend it.  Units are in map units.
#'
#' @returns A `GVector`.
#'
#' @seealso [terra::voronoi()], [sf::st_voronoi()], module `v.voronoi` in **GRASS**
#'
#' @example man/examples/ex_pointOperations.r
#'
#' @aliases voronoi
#' @rdname voronoi
#' @exportMethods voronoi
methods::setMethod(
	f = 'voronoi',
	signature = c(x = 'GVector'),
	definition = function(x, buffer = 0) {

	if (!(geomtype(x) %in% c('points', 'polygons'))) stop('The vector must represent spatial points or polygons.')
	
	.restore(x)

	# do not expand region beyond x
	regionExt(x)
	if (buffer != 0) {
		
		# set region extent to buffered vector
		extent <- regionExt()
		w <- extent['xmin']
		e <- extent['xmax']
		s <- extent['ymin']
		n <- extent['ymax']
		
		w <- w - buffer
		e <- e + buffer
		s <- s - buffer
		n <- n + buffer
		
		w <- as.character(w)
		e <- as.character(e)
		s <- as.character(s)
		n <- as.character(n)
		
		rgrass::execGRASS('g.region', n=n, s=s, e=e, w=w, flags=c('o', 'quiet', 'overwrite'))
		
	}

	gn <- .makeGname('voronoi', 'vect')
	args <- list(
		cmd = 'v.voronoi',
		input = gnames(x),
		output = gn,
		flags = c('quiet', 'overwrite'),
		intern = TRUE
	)

	if (geomtype(x) == 'polygons') args$flags <- c(args$clags, 'a')

	do.call(rgrass::execGRASS, args=args)
	makeGVector(gn)
	
	} # EOF
)
