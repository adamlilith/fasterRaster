#' Replace backslash with forward slash
#'
#' This function is helpful for Windows systems, where paths are usually expressed with left slashes, whereas **R** requires right slashes.
#'
#' @param x A string.
#'
#' @return Character.
#' 
#' @examples
#' 
#' forwardSlash('C:\\ecology\\main project')
#'
#' @export
forwardSlash <- function(x) {
	gsub('\\\\', '/', x)
}


# #' @name leftSlash
# #' @rdname forwardSlash
# #' @export
# leftSlash <- function(x) {
	# gsub('/', '\\\\', x)
# }
