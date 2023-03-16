
fasterHelp('fasterRasterize') # lookup by fasterRaster name
fasterHelp('rasterize', where='terra') # lookup by terra name
fasterHelp('st_buffer', where='sf') # lookup by sf name
fasterHelp('v.to.rast', where='GRASS') # lookup by GRASS name
fasterHelp('st_buffer', where='terra') # warning

fasterHelp('buffer', where=c('sf', 'terra')) # search sf and terra

fasterHelp('v.to', where='grass', approx=TRUE) # approximate lookup

fasterHelp('numeric', where='Input') # lfunctions accepting "numeric" input
fasterHelp('numeric', where='Output') # functions producing "numeric" output

fasterHelp() # all fasterRaster functions

# The next two lines return the same thing, a table with all GRASS functions
# represented in fasterRaster, and their terra equivalents.
fasterHelp(where=NULL) # all fasterRaster functions with all information
data(fasterFunctions)

# search all columns for a specific string
fasterHelp(x='around', where=NULL)
