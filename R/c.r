#' "Stack" GRasters and combine GVectors
#'
#' @description `GRaster`s can be "stacked" using this function, effectively creating a multi-layered raster. Note that this is different from creating a 3-dimensional raster, though such an effect can be emulated using stacking. `GVector`s can be combined into a single vector.  Stacks can only be created when:
#' * All objects are in the same **GRASS** ["location" and "mapset"][tutorial_sessions].
#' * All objects are the same class (either all `GRaster`s or all `GVector`s).
#' * For `GRaster`s:
#'      * Horizontal extents are the same.
#'      * Horizontal dimensions are the same.
#'      * The topology (2- or 3-dimensional) must be the same. If 3D, then all rasters must have the same number of depths and vertical extents.
#' * For `GVector`s:
#'      * The geometry (points, lines, or polygons) must be the same.
#' If features (boundaries, lines, etc.) of `GVector`s that are combined are identical or nearly identical, they can be cleaned using [removeDupNodes()] and [snap()].
#'
#' @param x A `GRaster` or a `GVector`.
#' @param ... One or more `GRaster`s, one or more `GVector`s, a list of `GRaster`s, or a list of `GVector`s. You can use a mix of lists and individual rasters or vectors.
#' @param df If `TRUE`, copy the data frame of the `x` `GVector` to the output. If `FALSE` (default), the output will not have a data frame.
#'
#' @return A `GRaster`.
#' 
#' @seealso [terra::c()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(f = "c",
	signature = "GRaster",
	definition = function(x, ...) {

	.restore(x)
	dots <- list(...)

	### concatenate
	out <- x
	z <- zext(out)
	dims <- dim(out)
	
	if (length(dots) >= 1L) {

		for (i in seq_along(dots)) {

			if (is.list(dots[[i]])) dots[[i]] <- c(dots[[i]])
			if (!inherits(dots[[i]], "GRaster")) stop("Can only combine GRasters with GRasters.")
			
			compareGeom(out, dots[[i]])
			mmdots <- minmax(dots[[i]])
			
			out <- GRaster(
				location = location(out),
				mapset = mapset(out),
				crs = crs(out),
				projection = .projection(out),
				topology = topology(out),
				sources = c(sources(out), sources(dots[[i]])),
				names = c(names(out), names(dots[[i]])),
				extent = as.vector(ext(out)),
				zextent = z,
				datatypeGRASS = c(out@datatypeGRASS, dots[[i]]@datatypeGRASS),
				dimensions = dims,
				nLayers = nlyr(out) + nlyr(dots[[i]]),
				resolution = res(out),
				minVal = c(.minVal(out), .minVal(dots[[i]])),
				maxVal = c(.maxVal(out), .maxVal(dots[[i]])),
				activeCat = c(out@activeCat, dots[[i]]@activeCat),
				levels = c(out@levels, dots[[i]]@levels)
			)
		
		} # next GRaster to combine
		
	} # next item
	out <- .makeUniqueNames(out)		
	out
	
	} # EOF
)

#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(f = "c",
	signature = "GVector",
	definition = function(x, ..., df = FALSE) {

	flags <- c("quiet", "overwrite")
	if (df) flags <- c(flags, "e")

	.restore(x)
	dots <- list(...)

	# unlist any lists
	if (length(dots) > 0L) {
		
		if (any(sapply(dots, is.list))) {
			newdots <- list()
			for (i in seq_along(dots)) {
				if (is.list(dots[[i]])) {
					newdots <- c(newdots, unlist(dots[[i]]))
				} else {
					newdots <- c(newdots, dots[[i]])
				}
			}
			dots <- newdots
		}
	}

	# comparable?
	if (length(dots) > 0L) {
		for (i in seq_along(dots)) compareGeom(x, dots[[i]], geometry=TRUE, topo=TRUE)
	}

	# sources of inputs
	input <- sources(x)
	if (length(dots) > 0L) input <- c(input, sapply(dots, sources))
	input <- paste(input, collapse=",")

	src <- .makeSourceName("combo", "vector")

	args = list(
		cmd = "v.patch",
		input = input,
		output = src,
		flags = flags
	)

	do.call(rgrass::execGRASS, args=args)

	.makeGVector(src)
	
	} # EOF
)

