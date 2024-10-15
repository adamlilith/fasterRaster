#' Has "GRASS" been started or not?
#'
#' @description Returns `TRUE` or `FALSE`, depending on whether a **GRASS** connection has been made or not within the current **R** session. Usually used only by developers. **GRASS** is started the first time [fast()] is used.
#'
#' @returns Logical.
#'
#' @examples
#'
#' grassStarted()
#'
#' @aliases grassStarted
#' @rdname grassStarted
#' @export
grassStarted <- function() {
	exists(".fasterRaster", inherits = TRUE) && .fasterRaster$grassStarted
}
