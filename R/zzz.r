.onUnload <- function(lib, pkg) {
	
	rm(.fasterRaster)

	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

}

.onUnattach <- function(lib, pkg) {

	rm(.fasterRaster)

	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

}
