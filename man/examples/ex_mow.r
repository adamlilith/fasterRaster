if (grassStarted()) {

# Setup
madElev <- fastData("madElev")
elev <- fast(madElev)

mow(elev, ask = TRUE) # delete GRASS raster attached to `elev`

}
