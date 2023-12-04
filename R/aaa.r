.onLoad <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$options <- list(
		grassDir = .grassDirDefault(),
		addonDir = .addonDirDefault(),
		workDir = .workDirDefault(),

		location = .locationDefault(),
		mapset = .mapsetDefault(),

		cores = .coresDefault(),
		verbose = .verboseDefault(),
		memory = .memoryDefault(),
		rasterPrecision = .rasterPrecisionDefault(),
		useDataTable = .useDataTableDefault()
	)
	.fasterRaster$locations <- list()
	.fasterRaster$messages <- list()
	setFastOptions(restore = TRUE)
	
}

.onAttach <- function(lib, pkg) {

	.fasterRaster <<- new.env(parent = emptyenv())
	.fasterRaster$grassStarted <- FALSE
	.fasterRaster$options <- list(
		grassDir = .grassDirDefault(),
		addonDir = .addonDirDefault(),
		workDir = .workDirDefault(),

		location = .locationDefault(),
		mapset = .mapsetDefault(),

		cores = .coresDefault(),
		verbose = .verboseDefault(),
		memory = .memoryDefault(),
		rasterPrecision = .rasterPrecisionDefault(),
		useDataTable = .useDataTableDefault()
	)
	.fasterRaster$locations <- list()
	.fasterRaster$messages <- list()
	setFastOptions(restore = TRUE)
	
	ver <- read.dcf(file = system.file("DESCRIPTION", package = pkg), fields = "Version")
	packageStartupMessage(paste(pkg, ver))
	packageStartupMessage("It is recommended to attach the data.table, terra, and sf packages")
	packageStartupMessage("before fasterRaster using, for example, ", dQuote("library(terra)"), ".")
	packageStartupMessage("For guides and table of contents, see ", dQuote("?fasterRaster"), ".")
	
}
