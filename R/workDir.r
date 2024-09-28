#' Get a GLocation's working directory
#'
#' @description This function returns the working directory of a `GLocation` object.
#'
#' @param x A `GLocation` object.
#'
#' @returns Character.
#'
#' @aliases .workDir
#' @rdname workDir
#' @exportMethod .workDir
#' @keywords internal
methods::setMethod(
	f = ".workDir",
	signature = c(x = "GLocation"),
	function(x)	x@workDir
)
