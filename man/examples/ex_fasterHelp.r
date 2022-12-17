fasterHelp('fasterRasterize') # lookup by fasterRaster name
fasterHelp('rasterize', terra = TRUE) # lookup by terra name
fasterHelp('v.to.rast', grass = TRUE) # lookup by GRASS name
fasterHelp('st_buffer', sf = TRUE) # lookup by sf name
fasterHelp('st_buffer', terra = TRUE, sf = TRUE) # warning
fasterHelp('v.to', grass = TRUE, approx = TRUE) # approximate lookup

# The next two lines return the same thing, a table with all GRASS functions
# represented in fasterRaster, and their terra equivalents.
fasterHelp()
data(fasterFunctions)

