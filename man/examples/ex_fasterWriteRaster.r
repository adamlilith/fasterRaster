\dontrun{

library(terra)

# change this according to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

# start GRASS session with a raster
madElev <- fasterData('madElev')
initGrass(rast=madElev, grassDir=grassDir)

# import the raster back from GRASS
madElevImport <- rgrass::read_RAST('madElev', flags='quiet')

madElev
madElevImport

# Save in single- and double-precision floating-point formats...
# This is overkill and for this raster will result in a much larger file
# size than is necessary. This raster only has integers, so it could be
# saves with an appropriate "datatype" argument.
writeRaster4(madElevImport, './madElevImport4.tif')
writeRaster8(madElevImport, './madElevImport8.tif')

# best option for this example (smallest file size)
writeRaster(madElevImport, './madElevImportBest.tif', datatype='INT2S')

# save GeoTIFF directly from GRASS... actually smaller than the original!
fasterWriteRaster('madElev', filename='./madElevFromGrass.tif',
datatype='INT2S')

# save ASCII directly from GRASS
# NB Sometimes creates an error about XML, in which case you may need to
# restart R again... sorry, unsure what's going on here!
fasterWriteRaster('madElev', filename='./madElevFromGrass.asc')

}
