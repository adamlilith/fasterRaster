#' Get a GSession's working directory
#'
#' @description This function returns the working directory of a `GSession` object.
#'
#' @param x A `GSession` object.
#'
#' @returns Character.
#'
#' @aliases workDir
#' @rdname workDir
#' @exportMethod
methods::setMethod(
	f = "workDir",
	signature = c(x = "GSession"),
	function(x)	x@workDir
)
