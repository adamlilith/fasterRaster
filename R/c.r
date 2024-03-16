#' "Stack" GRasters
#'
#' @description `GRaster`s can be "stacked" using this function, effectively creating a multi-layered raster. This is different from creating a 3-dimensional raster, though such an effect can be emulated using stacking. `GVector`s can be combined into a single vector.  Stacks can only be created when:
#' * All objects are the same class (either all `GRaster`s or all `GVector`s).
#' * All objects have the same coordinate reference system (see crs()).
#' * Horizontal extents are the same (see [ext()]).
#' * Horizontal dimensions are the same (see [res()]).
#' * The topology (2- or 3-dimensional) must be the same. If 3D, then all rasters must have the same number of depths and vertical extents (see [topology()]).
#'
#' Data tables associated with `GVector`s will be combined if each vector has a table and if each table has the same columns and data types. Otherwise, the data table will be combined using [merge()].
#'
#' @param x A `GRaster` or a `GVector`.
#'
#' @param ... One or more `GRaster`s, one or more `GVector`s, a list of `GRaster`s, or a list of `GVector`s. You can use a mix of lists and individual rasters or vectors.
#'
#' @return A `GRaster`.
#' 
#' @seealso [add<-], [terra::c()], \code{\link[terra]{add<-}}
#'
#' @example man/examples/ex_c.r
#'
#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(
	f = "c",
	signature = "GRaster",
	definition = function(x, ...) {

	.locationRestore(x)
	dots <- list(...)

	# unlist any lists
	if (length(dots) > 0L) dots <- omnibus::unlistRecursive(dots)

	### concatenate
	out <- x
	z <- zext(out)
	dims <- dim(out)[1L:3L]

	if (length(dots) >= 1L) {

		for (i in seq_along(dots)) {

			if (!inherits(dots[[i]], "GRaster")) stop("Can only combine GRasters with GRasters.")
			
			compareGeom(out, dots[[i]])
			mmx <- minmax(out)
			mmdots <- minmax(dots[[i]])

			mn <- c(.minVal(out), .minVal(dots[[i]]))
			mx <- c(.maxVal(out), .maxVal(dots[[i]]))
			
			out <- GRaster(
				location = .location(out),
				mapset = .mapset(out),
				workDir = faster("workDir"),
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
				resolution = res3d(out),
				minVal = mn,
				maxVal = mx,
				activeCat = c(out@activeCat, dots[[i]]@activeCat),
				levels = c(out@levels, dots[[i]]@levels)
			)
		
		} # next GRaster to combine
		
	} # next item
	out <- .makeUniqueNames(out)		
	out
	
	} # EOF
)
