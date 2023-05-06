#' "Stack" GRasters or GVectors
#'
#' `GRaster`s and `GVector`s can be "stacked" using this function, effectively creating a multi-layered raster or vector set. Note that this is different from creating a 3-dimensional raster or vector, though such an effect can be emulated using stacking. Stacks can only be created when:
#' * All objects are in the same **GRASS** [location and mapset][tutorial_sessions].
#' * All objects are the same type (`GRaster`s or `GVector`s, but not both).
#' * Additionally, for `GRaster`s:
#'		* Horizontal extents are the same.
#'      * Horizontal dimensions/resolutions are the same.
#'		* The topology (2D or 3D) must be the same. If 3D, then all `GRaster`s must have the same number of depths and vertical extents.
#'  `GVector`s do not have to have the same horizontal or vertical extents and topologies.
#'
#' @param x A `GRaster`, `GVector`, or a list of of `GRaster`s or `GVector`s (but not both),
#' @param ... One or more `GRaster`s or `GVector`s, or a list of `GRaster`s or `GVector`s (but not both).
#'
#' @return A `GRaster`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(f = 'c',
	signature = 'GRaster',
	definition = function(x, ...) {

	.restore(x)
	x <- list(x, ...)

	# unlist lists of GSpatials
	isList <- sapply(x, is.list)
	if (any(isList)) {
		index <- which(isList)
		for (i in index) x[[i]] <- c(unlist(x[[i]]))
	}

	### concatenate
	out <- x[[1L]]
	
	if (length(x) > 1L) {

		for (i in 2L:length(x)) {

			if (!inherits(x[[i]], 'GRaster')) stop('Can only combine GRasters with GRasters.')
			
			comparable(first, x[[i]], fail = TRUE)

			mmx <- minmax(x[[i]])
			mmout <- minmax(out)
			z <- zext(out)
			
			out <- GRaster(
				location = location(out),
				mapset = mapset(out),
				crs = crs(out),
				topology = topology(out),
				gnames = c(gnames(out), gnames(x[[i]])),
				names = c(names(out), names(x[[i]])),
				extent = as.vector(ext(out)),
				ztop = z[['top']],
				zbottom = z[['bottom']],
				datatypeGRASS = c(datatype(out), datatype(x[[i]])),
				dimensions = c(nrow(out), ncol(out), ndepth(out)),
				nLayers = nlyr(out) + nlyr(x[[i]]),
				resolution = res(out),
				nCats = c(ncat(out), ncat(x[[i]])),
				minVal = c(mmout['min', ], mm['min', ]),
				maxVal = c(mmout['max', ], mm['max', ])
			)
		
		} # next GRaster to combine
		
	} # next item
		
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out
	
	} # EOF
)

#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(f = 'c',
	signature = 'GVector',
	definition = function(x, ...) {

	.restore(x)
	x <- list(x, ...)
	
	if (any(is.list(x))) {
		newx <- list()
		for (i in seq_len(x)) {
			newx[[i]] <- if (is.list(x[[i]])) {
				unlist(x[[i]])
			} else {
				x[[i]]
			}
		}
		x <- newx
	}

	### concatenate
	out <- x[[1L]]
	
	if (length(x) > 1L) {

		for (i in 2:length(x)) {
		
			if (!inherits(x[[i]], 'GVector')) stop('Can only combine GVectors with GVectors.')
			
			comparable(out[[1L]], x[[i]], fail = TRUE)

			mmx <- minmax(x[[i]])
			mmout <- minmax(out)
			zx <- zext(x)
			zout <- zext(out)
			
			out <- GVector(
				location = location(out),
				mapset = mapset(out),
				crs = crs(out),
				topology = topology(out),
				gnames = c(gnames(out), gnames(x[[i]])),
				names = c(names(out), names(x[[i]])),
				extent = as.vector(ext(out)),
				ztop = c(zout[['top']], zx[['top']]),
				zbottom = c(zout[['bottom']], zx[['bottom']]),
				nLayers = c(nlyr(out), nlyr(x[[i]])),
				geometry = c(geometry(out), geometry(x[[i]])),
				nFields = c(nFields(out), nFields(x[[i]])),
				fields = c(fields(out), fields(x[[i]])),
				fieldClasses = c(fieldClasses(out), fieldClasses(x[[i]]))
			)
		
		} # next GVector to combine
		
	} # next item
		
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out
	
	} # EOF
)
