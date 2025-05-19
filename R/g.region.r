#' Call GRASS `g.region` tool
#'
#' This function calls the **GRASS** tool `g.region` to display information on the region of the current **GRASS** "project".
#'
#' @returns Displays current region information for the active "project/location" in **GRASS**.
#'
#' @export
#' @keywords internal
.g.region <- function() {
	out <- rgrass::execGRASS("g.region", flags = "p", intern = TRUE)
	out
}
