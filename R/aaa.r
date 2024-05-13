.onLoad <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$locations <- list()
	.fasterRaster$activeLocation <- NA_character_
	.fasterRaster$messages <- list()
	.fasterRaster$options <- list()
	# .fasterRaster$maxSqlLength <- 29900L # maximum number of characters (with safety margin) of an SQL query GRASS can handle
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
	packageStartupMessage("To avoid conflicts between functions, please attach the `terra`, `sf`,")
	packageStartupMessage("and `data.table` packages before attaching `fasterRaster` using, for")
	packageStartupMessage("example, `library(terra)`.")
	packageStartupMessage("For guides and table of contents, see `?fasterRaster``.")
	
}
