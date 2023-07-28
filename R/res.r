#' Spatial resolution
#'
#' @description Spatial resolution of a `GRaster`:
#' 
#' `res()`: 2-dimensional resolution (x and y).\cr\cr
#' `res3d()`: 3-dimensinal resolution (z, y, and z).\cr\cr
#' `xres()`, `yres()`, and `zres()`: East-west resolution, north-south resolution, and top-bottom resolution.
#'
#' @param x A `GRaster`, `GRegion`, or missing. If missing, the resolution of the currently active [region][tutorial_regions] is returned.
#'
#' @return A numeric vector. For both `res()` and `res3d()`, the first value is the length of cells in the x-direction and the second the length of cells in the y-direction. For `res3d()` the third value is height of a voxel (the z-direction). `xres()`, `yres()`, and `zres()` each return a single value.
#'
#' @seealso [terra::res()]
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases res
#' @rdname res
#' @exportMethod res
setMethod(
	f = "res",
	signature = "missing",
 	definition = function(x) res(region())
)

#' @aliases res
#' @rdname res
#' @exportMethod res
setMethod(
	f = "res",
	signature = "GRegion",
	definition = function(x) x@resolution[1L:2L]
)

#' @aliases xres
#' @rdname res
#' @exportMethod xres
setMethod(
	f = "xres",
	signature = "missing",
 	definition = function(x) xres(region())
)

#' @aliases xres
#' @rdname res
#' @exportMethod xres
setMethod(
	f = "xres",
	signature = "GRegion",
	definition = function(x) x@resolution[1L]
)

#' @aliases yres
#' @rdname res
#' @exportMethod yres
setMethod(
	f = "yres",
	signature = "missing",
 	definition = function(x) yres(region())
)

#' @aliases yres
#' @rdname res
#' @exportMethod yres
setMethod(
	f = "yres",
	signature = "GRegion",
	definition = function(x) x@resolution[2L]
)

#' @aliases zres
#' @rdname res
#' @exportMethod zres
setMethod(
	f = "zres",
	signature = "missing",
 	definition = function(x) zres(region())
)

#' @aliases zres
#' @rdname res
#' @exportMethod zres
setMethod(
	f = "zres",
	signature = "GRegion",
	definition = function(x) x@resolution[3L]
)

#' @aliases res3d
#' @rdname res
#' @exportMethod res3d
setMethod(
	f = "res3d",
	signature = "missing",
 	definition = function(x) res3d(region())
)

#' @aliases res3d
#' @rdname res
#' @exportMethod res3d
setMethod(
	f = "res3d",
	signature = "GRegion",
	definition = function(x) c(xres=x@resolution[1L], yres=x@resolution[2L], zres=x@resolution[3L])
)
