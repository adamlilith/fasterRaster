#' Number of categories in a GRaster
#'
#' @description Reports number of categories in a categorical `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector with one value per layer in the `GRaster`.
#' 
#' @seealso [terra::levels()], [terra::cats()]
#' 
#' @example man/examples/ex_GRaster.r
#'
#' @aliases ncat
#' @rdname ncat
#' @exportMethod ncat
setMethod(
	f = "ncat",
	signature = "GRaster",
	definition = function(x) x@nCats
)
