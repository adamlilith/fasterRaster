#' Minimum convex hull around a spatial vector
#'
#' Create a minimum convex hull around a spatial vector.
#'
#' @param x A `GVector`.
#' @param by Character: If `''` (default), then a convex hull is created for all geometries together. Otherwise, this is the name of a field in the vector. Hulls will be created for each set of geometries with the same value in this column.
#'
#' @return A `GVector`.
#'
#' @seealso [terra::convHull(), `sf::st_convex_hull()`, **GRASS** module `v.hull`
#'
#' @example man/examples/ex_convHull.r
#'
#' @aliases convHull
#' @rdname convHull
#' @exportMethod convHull
methods::setMethod(
	f = 'convHull',
	signature = c(x = 'GVector'),
	definition = function(x, by = '') {
print('HAVE NOT IMPLEMENTES "WHERE" IN CONVHULL()!')	
	n <- nlyr(x)
	out <- list()
	
	gn <- .makeGname('convHull', 'vector', n)
	for (i in seq_len(n)) {
		
		args <- list(
			cmd = 'v.hull',
			input = gnames(x)[i],
			output = gn[i],
			flags = c('quiet', 'overwrite')
		)
	
		do.call(rgrass::execGRASS, args=args)
		
		out[[i]] <- makeVector(gn[i])
		
	} # next layer
	
	out <- c(out)
	out
		
	} # EOF
)

