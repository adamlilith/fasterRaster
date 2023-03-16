.onLoad <- function(lib, pkg) {
	fasterSetOptions(restore = TRUE)
	.setSession()
	.setSessionStarted(FALSE)
}

.onAttach <- function(lib, pkg) {

	ver <- read.dcf(file=system.file('DESCRIPTION', package=pkg), fields='Version')
	packageStartupMessage(paste(pkg, ver))
	packageStartupMessage('For information on getting started, see ?tutotial and ?fasterRaster.')
	packageStartupMessage('Use fasterHelp() to find equivalent functions in fasterRaster, terra,\nsf, and GRASS.')
	
}

# .onUnload <- function(lib, pkg) {

# }
