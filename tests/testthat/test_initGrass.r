cat('initGrass')

# drive <- 'C:'
drive <- 'E:'
grassDir <- paste0(drive, '/Program Files/GRASS GIS 8.2') # example for a PC
options(grassDir = grassDir)

test_that('multiplication works', {
	madElev <- rast(paste0(drive, '/Ecology/Drive/R/fasterRaster/inst/extdata/madElev.tif'))
	expect_identical(initGrass(madElev, rastName = 'madElev'), 'madElev')
})
