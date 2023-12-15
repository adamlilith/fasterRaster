.onLoad <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$locations <- list()
	.fasterRaster$activeLocation <- NA_character_
	.fasterRaster$messages <- list()
	.fasterRaster$options <- list()
	faster(restore = TRUE)
	
}

.onAttach <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$locations <- list()
	.fasterRaster$activeLocation <- NA_character_
	.fasterRaster$messages <- list()
	.fasterRaster$options <- list()
	faster(restore = TRUE)
	
	ver <- read.dcf(file = system.file("DESCRIPTION", package = pkg), fields = "Version")
	packageStartupMessage(paste(pkg, ver))
	packageStartupMessage("It is recommended to attach the data.table, terra, and sf packages")
	packageStartupMessage("before fasterRaster using, for example, ", dQuote("library(terra)"), ".")
	packageStartupMessage("For guides and table of contents, see ", dQuote("?fasterRaster"), ".")
	
}
