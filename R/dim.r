#' @title Number of rows, columns, depths, cells, and layers
#'
#' @description
#' For `GRegion`s: Number of rows, columns, depths, and cells:
#' * `dim()`: Rows and columns
#' * `dim3d()`: Rows, columns, and depths
#' * `nrow()`: Rows
#' * `ncol()`: Columns
#' * `ndepth()`: Depths (for 3-dimensional rasters only)
#' * `ncell()`: Number of cells (2 dimensions)
#' * `ncell3d()`: Number of cells (3 dimensions)
#'
#' For `GRaster`s: As above, plus number of layers:
#' * `nlyr()`: Layers (number of "stacked" rasters--different from depths of a raster).
#'
#' For `GVector`s: Number of geometries and fields (columns):
#' * `dim()`: Number of geometries and number of columns in data table
#' * `nrow()`: Number of geometries
#' * `ncol()`: Number of columns in data table
#'
#' @param x A `GRegion`, `GRaster`, `GVector`, or missing. If missing, then the dimensions of the currently active "region" are returned (see `vignette("regions", package = "fasterRaster")`).
#'
#' @return A numeric value or vector.
#' 
#' @seealso [ngeom()], [nsubgeom()], [nacell()], [nonnacell()], [terra::dim()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases dim
#' @rdname dim
#' @exportMethod dim
methods::setMethod(
	f = "dim",
	signature = "GRegion",
	definition = function(x) {
	c(rows = unname(x@dimensions[1L]), cols = unname(x@dimensions[2L]))
	} # EOF
)


## Error on install: "the method for function 'dim' and signature x="missing" is sealed and cannot be re-defined"
# #' @aliases dim
# #' @rdname dim
# #' @exportMethod dim
# methods::setMethod(
	# f = "dim",
	# signature = "missing",
	# definition = function(x) .region()@dimensions[1L:2L]
# )

#' @aliases dim3d
#' @rdname dim
#' @exportMethod dim3d
methods::setMethod(
	f = "dim3d",
	signature = "missing",
	definition = function(x) .region()@dimensions
)

#' @aliases dim3d
#' @rdname dim
#' @exportMethod dim3d
methods::setMethod(
	f = "dim3d",
	signature = "GRegion",
	definition = function(x) {
	
	c(
		rows = unname(x@dimensions[1L]),
		cols = unname(x@dimensions[2L]),
		depths = unname(x@dimensions[3L])
	)
	
	} # EOF
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = "nrow",
	signature = "missing",
 	definition = function(x) nrow(.region())
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = "nrow",
	signature = "GRegion",
	definition = function(x) unname(x@dimensions[1L])
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = "ncol",
	signature = "missing",
 	definition = function(x) ncol(.region())
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = "ncol",
	signature = "GRegion",
	definition = function(x) unname(x@dimensions[2L])
)

#' @rdname dim
#' @aliases ndepth
#' @exportMethod ndepth
setMethod(
	f = "ndepth",
	signature = "missing",
	definition = function(x) ndepth(.region())
)

#' @rdname dim
#' @aliases ndepth
#' @exportMethod ndepth
setMethod(
	f = "ndepth",
	signature = "GRegion",
	definition = function(x) unname(x@dimensions[3L])
)

#' @rdname dim
#' @aliases ncell
#' @exportMethod ncell
setMethod(
	f = "ncell",
	signature = "missing",
	definition = function(x) ncell(.region())
)

#' @rdname dim
#' @aliases ncell
#' @exportMethod ncell
setMethod(
	f = "ncell",
	signature = "GRegion",
	definition = function(x) prod(x@dimensions[1L:2L])
)

#' @rdname dim
#' @aliases ncell3d
#' @exportMethod ncell3d
setMethod(
	f = "ncell3d",
	signature = "missing",
 	definition = function(x) ncell3d(.region())
)

#' @rdname dim
#' @aliases ncell3d
#' @exportMethod ncell3d
setMethod(
	f = "ncell3d",
	signature = "GRegion",
	definition = function(x) prod(x@dimensions[1L:3L])
)

#' @aliases dim
#' @rdname dim
#' @exportMethod dim
methods::setMethod(
	f = "dim",
	signature = "GVector",
	definition = function(x) dim(x@table)
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = "nrow",
	signature = "GVector",
	definition = function(x) nrow(x@table)
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = "ncol",
	signature = "GVector",
	definition = function(x) ncol(x@table)
)

#' @rdname dim
#' @aliases nlyr
#' @exportMethod nlyr
setMethod(
	f = "nlyr",
	signature = "missing",
 	definition = function(x) nlyr(.region())
)

#' @rdname dim
#' @aliases nlyr
#' @exportMethod nlyr
setMethod(
	f = "nlyr",
	signature = "GRaster",
	definition = function(x) x@nLayers
)
