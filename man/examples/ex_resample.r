\dontrun{
# NB This example is in a "dontrun{}" block because it requires users to have
# GRASS GIS Version 8+ installed on their system.

# IMPORTANT #1: If you already have a GRASS session started, you will need to
# run the line below and the last line in this example to work with it again.
# If you have not started a GRASS session, you can skip this step and go to
# step #2.
opts. <- getFastOptions()

# IMPORTANT #2: Select the appropriate line below and change as necessary to
# where GRASS is installed on your system.
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
grassDir <- '/usr/local/grass' # Linux

# setup
library(terra)

# elevation raster, climate raster, rivers vector
madElev <- fastData('madElev')

# start GRASS session for examples only
faster(x = madElev, grassDir = grassDir,
workDir = tempdir(), location = 'examples') # line only needed for examples

elev <- fast(madElev)

### resample raster to 120 x 120 m
elev120 <- resample(elev, c(120, 120), method='bilinear')
elev
elev120

### resample using a template raster as a template
##################################################

template <- aggregate(elev, 4)

nearest <- resample(elev, template, method = 'nearest')

bilinear <- resample(elev, template, method = 'bilinear')
bilinearNoFB <- resample(elev, template, method = 'bilinear', fallback = FALSE)

bicubic <- resample(elev, template, method = 'bicubic')
bicubicNoFB <- resample(elev, template, method = 'bicubic', fallback = FALSE)

lanczos <- resample(elev, template, method = 'lanczos')
lanczosNoFB <- resample(elev, template, method = 'lanczos', fallback = FALSE)

# rasters resampled without fallback have fewer non-NA cells
resampled <- c(nearest, bilinear, bilinearNoFB, bicubic, bicubicNoFB, lanczos,
    lanczosNoFB)
names(resampled) <- c('nearest', 'bilinear', 'bilinearNoFB', 'bicubic',
    'bicubicNoFB', 'lanczos', 'lanczosNoFB')
ones <- resampled * 0 + 1
global(ones, 'sum') # number of non-NA cells
global(resampled, c('mean', 'sd', 'min', 'max')) # other statistics

# compare fallback to no fallback
frLanczos <- rast(lanczos)
frLanczosNoFB <- rast(lanczosNoFB)

plot(frLanczos, col = 'red',
    main = 'Red: Cells in fallback not non-fallback', legend = FALSE)
plot(frLanczosNoFB, add=TRUE)

# compare fasterRaster with terra
coarserTerra <- aggregate(madElev, 4)
terraLanczos <- resample(madElev, coarserTerra, method = 'lanczos')

frLanczos <- extend(frLanczos, terraLanczos)
frLanczosNoFB <- extend(frLanczosNoFB, terraLanczos)

frLanczos - terraLanczos
frLanczosNoFB - terraLanczos

plot(frLanczos - terraLanczos, main = 'Difference')
plot(frLanczosNoFB - terraLanczos, main = 'Difference')

plot(terraLanczos, col = 'red',
    main = 'Red: Cells in terra not in FR', legend = FALSE)
plot(frLanczos, add=TRUE)

plot(frLanczos, col = 'red',
    main = 'Red: Cells in FR not in terra', legend = FALSE)
plot(terraLanczos, add=TRUE)

# IMPORTANT #3: Revert back to original GRASS session if needed.
fastRestore(opts.)
fastRemove('examples')

}
