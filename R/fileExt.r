#' File extension
#'
#' @description Returns the file extension. Borrowed from **tools** package.
#'
#' @param x Character.
#'
#' @returns Character.
#'
#' @aliases fileExt
#' @noRd
.fileExt <- function(x) {

	pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")

}
