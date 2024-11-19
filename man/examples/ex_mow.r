if (grassStarted()) {

# Setup
madElev <- fastData("madElev")
elev <- fast(madElev)

# delete GRASS raster attached to `elev`
if (FALSE) {
	mow(elev)
	elev # useless R object still exists, though
	elev * 2 # will fail because raster has been deleted from GRASS
}

}
