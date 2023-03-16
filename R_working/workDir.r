#' GRASS working directory of an object or the currently active GRASS session
#'
#' `workDir()` reports the working directory of a **fasterRaster** `GRaster` or `GVector`, or of the currently active **GRASS** [session]. **GRASS** sessions are stored on disk. This folder can be specified by the user for use later, or (by default) is set in the user's temporary directory.
#'
#' @param x Either:\cr
#' Any object that inherits from `GSession`: `GSpatial`, `GRaster`, or `GVector` objects.\cr
#' Misisng: Reports the current working directory.
#'
#' @return Character vector.
#'
#' @example man/examples/example_sessions.r
#'
#' @export

if (!isGeneric('workDir')) setGeneric('workDir', function(x) standardGeneric('workDir'))
setMethod(
	f = 'workDir',
	signature = 'GSession',
	definition = function(x) x@workDir
)

if (!isGeneric('names<-')) setGeneric(
	'names<-',
	function(x, value) standardGeneric('names<-')
)
setMethod(
	f = 'names<-',
	signature = 'GRaster',
	definition = function(x, value) {
		x@names <- value
		validObject(x)
		x
	}
)
