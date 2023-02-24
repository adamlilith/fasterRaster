#' Replace left slash with right slash in a string
#'
#' This function is helpful for Windows systems, where paths are usually expressed with left slashes, whereas **R** requires right slashes.
#'
#' @param x A string.
#'
#' @return Character.
#' 
#' @examples
#' 
#' rightSlash('C:\\ecology\\main project')
#'
#' @export

rightSlash <- function(x) {

	gsub('\\\\', '/', x)

}
