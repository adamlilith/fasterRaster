.onLoad <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$options <- list()
	setFastOptions(restore = TRUE)

}

.onAttach <- function(lib, pkg) {

	.fasterRaster <- new.env(parent = emptyenv())
	.fasterRaster$options <- list()
	setFastOptions(restore = TRUE)
	
	ver <- read.dcf(file=system.file('DESCRIPTION', package=pkg), fields='Version')
	packageStartupMessage(paste(pkg, ver))
	packageStartupMessage('To use fasterRaster functions, please run startFast().')
	
}
