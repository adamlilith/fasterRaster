.onLoad <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$options <- list()
	setFastOptions(restore = TRUE)
	
}

.onAttach <- function(lib, pkg) {

	.fasterRaster <- new.env(parent = emptyenv())
	.fasterRaster$options <- list()
	setFastOptions(restore = TRUE)
	
	ver <- read.dcf(file=system.file("DESCRIPTION", package=pkg), fields="Version")
	packageStartupMessage(paste(pkg, ver))
	# packageStartupMessage("It is recommended to attach the terra and sf packages before")
	# packageStartupMessage("fasterRaster using, for example, "library(terra)".")
	packageStartupMessage("To use fasterRaster functions, please connect to GRASS using faster().")
	packageStartupMessage("Functions will be executed assuming you are using GRASS ", .grassVerDefault(), ".")
	packageStartupMessage("You can set the version of GRASS you are using with the function setFastOptions().")

	
}
