.onLoad <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$options <- list()
	.fasterRaster$locations <- list()
	setFastOptions(restore = TRUE)
	
}

.onAttach <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$options <- list()
	.fasterRaster$locations <- list()
	setFastOptions(restore = TRUE)
	
	ver <- read.dcf(file=system.file("DESCRIPTION", package=pkg), fields="Version")
	packageStartupMessage(paste(pkg, ver))
	# packageStartupMessage("It is recommended to attach the terra and sf packages before")
	# packageStartupMessage("fasterRaster using, for example, "library(terra)".")
	packageStartupMessage("To use fasterRaster functions, please connect to GRASS using faster().")
	packageStartupMessage("Functions will be executed assuming you are using GRASS ", .grassVerDefault(), ".")
	packageStartupMessage("If you are using a more recent version, some functions will work faster if you")
	packageStartupMessage("use setFastOptions() to set the version to match your GRASS installation.")

	
}
