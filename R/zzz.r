.onUnload <- function(lib, pkg) {
	
	rm(.fasterRaster)

	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

}

.onUnattach <- function(lib, pkg) {

	# remove GRASS files from cache
	if (faster("clean")) mow(verbose = faster("verbose"), ask = FALSE)
	rm(.fasterRaster)

	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

}
