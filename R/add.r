#' "Stack" a GRaster
#'
#' @description This function "stacks" one `GRaster` with another. It has the same functionality as [c()].
#'
#' @param x,value A `GRaster`.
#'
#' @returns A `GRaster`.
#'
#' @seealso [c()], [terra::add<-], [terra::c()]
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @name add<-
#' @aliases add<-,GRaster,GRaster-method
#' @docType methods
#' @rdname add
#' @exportMethod add<-
methods::setMethod(
	f = "add<-",
	signature = c(x = "GRaster", value = "GRaster"),
	function(x, value) c(x, value)
)
