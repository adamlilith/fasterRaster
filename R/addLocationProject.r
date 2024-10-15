#' Add a "project" or "location" argument to an "args" list
#'
#' @description Unhelpfully, starting with **GRASS** 8.4, what were previously called "locations" and now called "projects."  These were supposed to be back-compatible, but are not. This function takes a list of arguments that will be passed to [rgrass::execGRASS()] and adds either a "location" or "project" argument, depending on the version of **GRASS** being used.
#'
#' @param args A `list` of arguments to be passed to a **GRASS** module.
#' @param locProj Character: The name of the location or project.
#'
#' @returns A `list`.
#'
#' @keywords internal
.addLocationProject <- function(args, locProj) {

	ver <- grassInfo("versionNumber")
	if (ver <= 8.3) {
		args$location <- locProj
	} else {
		args$project <- locProj
	}
	args

}
