#' Get a GLocation's working directory
#'
#' @description This function returns the working directory of a `GLocation` object.
#'
#' @param x A `GLocation` object or missing. If an object, returns the working folder in which the object is saved by **GRASS**. If missing, then just returns the working folder (same as `faster("workDir")`).
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

#' @aliases .workDir
#' @rdname workDir
#' @exportMethod .workDir
#' @keywords internal
methods::setMethod(
	f = ".workDir",
	signature = c(x = "missing"),
	function(x)	faster("workDir")
)
