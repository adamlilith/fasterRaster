.onUnload <- function(lib, pkg) {
	
	# remove GRASS files from cache
	mow("*", verbose = faster("verbose"), ask = FALSE)

	# remove special environment
	rm(.fasterRaster)

	# remove GRASS files
	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

}

.onUnattach <- function(lib, pkg) {

	# remove GRASS files from cache
	mow("*", verbose = faster("verbose"), ask = FALSE)

	# remove special environment
	rm(.fasterRaster)

	# remove GRASS files
	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

}
