if (grassStarted()) {

# Setup
madElev <- fastData("madElev")
elev <- fast(madElev)

mow(elev, ask = TRUE) # delete GRASS raster attached to `elev`
elev # broken R object still exists, though
elev * 2 # will fail because raster has been deleted from GRASS

}
