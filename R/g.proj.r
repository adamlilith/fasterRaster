#' Call GRASS `g.proj` module
#'
#' This function calls the **GRASS** module `g.region` to display information on the projection of the current **GRASS** "project".
#'
#' @returns Displays current projection information for the active "project/location" in **GRASS**.
#'
#' @examples
#'
#' if (grassStarted()) {
#'
#' .g.proj()
#'
#' }
#'
#' @export
#' @keywords internal
.g.proj <- function() {
	out <- rgrass::execGRASS("g.proj", flags = "p", intern = TRUE)
	out
}

