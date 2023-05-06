#' Make a copy of an object in GRASS
#'
#' Create a copy of a `GRaster` or `GVector` in **GRASS**.  This function is used internally and is of little use to most users.  This only creates a copy of the object in the **GRASS** session--to make a `GRaster` or `GVector`, [makeGRaster()] or [makeGVector()] need to be called after making the copy. Note that if the object is multi-layered, then a copy is made of each layer.
#'
#' @param x `GRaster`, `GVector`, or character: The object or the `gnames` of the object to be copied. Can take multi-layered objects or multiple `gnames`.
#' @param rastOrVect Character: Type of object to copy. Either `raster` or `vector`. Partial matching is used, and case does not matter.
#'
#' @return Character vector representing the new `gnames` of each object, plus makes a copy of the given object(s) in **GRASS**.
#'
#' @aliases copyGSpatial
#' @rdname copyGSpatial
#' @exportMethod copyGSpatial
methods::setMethod(
	f = 'copyGSpatial',
	signature = c(x = 'GRaster'),
	function(x) .copyGRaster(x)
)

#' @aliases copyGSpatial
#' @rdname copyGSpatial
#' @exportMethod copyGSpatial
methods::setMethod(
	f = 'copyGSpatial',
	signature = c(x = 'GVector'),
	function(x) .copyGRaster(x)
)

#' @aliases copyGSpatial
#' @rdname copyGSpatial
#' @exportMethod copyGSpatial
methods::setMethod(
	f = 'copyGSpatial',
	signature = c(x = 'character'),
	function(x, rastOrVect) {
	
	n <- length(x)
	gnsTo <- rep(NA_chatracter_, n)
	
	rastOrVect <- .pmatch(tolower(rastOrVect), c('raster', 'vector'))
	rastOrVect <- rep(rastOrVect, n)

	for (i in seq_len(n)) {
		
		if (rastOrVect[i] == 'raster') {
			x <- makeGRaster(x[i])
			gnsTo[i] <- .copyGRaster(x)
		} else if (rastOrVect[i] == 'vector') {
			makeGVector(x[i])
			gnsTo[i] <- .copyGVector(x)
		}
	
	}
		
	} # EOF

)

.copyGRaster <- function(x) {

	n <- nlyr(x)
	topo <- topology(x)
	rastOrVect <- if (topo == '2D') { 'raster' } else { 'raster3d' }

	.restore(x)
	regionShape(x)

	gn <- gnames(x)

	# from/to GRASS names
	gnTos <- .makeGname(NULL, rastOrVect='raster', n)
	
	# copy
	args <- list(
		cmd = 'g.copy',
		flags = c('quiet', 'overwrite'),
		intern = TRUE
	)
	
	for (i in seq_len(n)) {
		
		gnFrom <- gn[i]
		gnTo <- gnTos[i]
		
		fromTo <- paste0(gnFrom, ',', gnTo)
		if (topo == '2D') {
			thisArgs <- c(args, raster = fromTo)
		} else if (topo == '3D') {
			thisArgs <- c(args, raster_3d = fromTo)
		}
		
		do.call(rgrass::execGRASS, thisArgs)
	
	}
	
	gnTos
	
}

.copyGVector <- function(x = NULL) {

	n <- nlyr(x)
	topo <- topology(x)

	.restore(x)
	regionExt(x)

	gn <- gnames(x)

	# from/to GRASS names
	gnTos <- .makeGname(NULL, rastOrVect='vector', n)
	
	# copy
	args <- list(
		cmd = 'g.copy',
		flags = c('quiet', 'overwrite'),
		intern = TRUE
	)
	
	for (i in seq_len(n)) {
		
		gnFrom <- gn[i]
		gnTo <- gnTos[i]
		
		fromTo <- paste0(gnFrom, ',', gnTo)
		thisArgs <- c(args, vector = fromTo)
		do.call(rgrass::execGRASS, thisArgs)
	
	}
	
	gnTos

}
