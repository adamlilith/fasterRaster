.onLoad <- function(lib, pkg) {

	# if (!exists('.fasterRaster', where=globalenv())) {
		# packageStartupMessage('Most fasterRaster functions will not work unless you run startFast().')
	# }

	.fasterRaster <<- new.env()
	opts <- .namesOfOptions()
	
	setFastOptions(restore=TRUE)
	.setHiddenOptions(restore=TRUE)


}

.onAttach <- function(lib, pkg) {

	ver <- read.dcf(file=system.file('DESCRIPTION', package=pkg), fields='Version')
	packageStartupMessage(paste(pkg, ver))
	if (!exists('.fasterRaster', where=globalenv())) {
		packageStartupMessage('To use most fasterRaster functions, please run startFast().')
	}
	
}

.onUnload <- function(lib, pkg) {

	if (exists('.fasterRaster', where=globalenv())) rm(.fasterRaster, envir=globalenv())
	
}

.onUnattach <- function(lib, pkg) {

	if (exists('.fasterRaster', where=globalenv())) rm(.fasterRaster, envir=globalenv())
	
}
