### GENERICS

if (!isGeneric('as.contour')) setGeneric('as.contour', function(x, nlevels, levels) standardGeneric('as.contour'))

if (!isGeneric('c')) setGeneric(name='c', function(x, rastOrVect = NULL) standardGeneric('c'))
if (!isGeneric('comparable')) setGeneric(name='comparable', def=function(x, y, ...) standardGeneric('comparable'))
if (!isGeneric('crs')) setGeneric(name='crs', def=function(x, ...) standardGeneric('crs'))

if (!isGeneric('datatype')) setGeneric(name='datatype', def=function(x, type = 'GRASS') standardGeneric('datatype'))
if (!isGeneric('dim')) setGeneric(name='dim', def=function(x) standardGeneric('dim'))
if (!isGeneric('dim3d')) setGeneric(name='dim3d', def=function(x) standardGeneric('dim3d'))

if (!isGeneric('fast')) setGeneric('fast', function(x, rastOrVect = NULL) standardGeneric('fast'))

if (!isGeneric('.gname')) setGeneric(name='.gname', def=function(x) { standardGeneric('.gname') })
if (!isGeneric('geometry')) setGeneric(name='geometry', def=function(x) standardGeneric('geometry'))

if (!isGeneric('location')) setGeneric(name = 'location', def = function(x) standardGeneric('location'))

if (!isGeneric('mapset')) setGeneric(name = 'mapset', def = function(x) standardGeneric('mapset'))
if (!isGeneric('minmax')) setGeneric(name='minmax', def=function(x) standardGeneric('minmax'))

if (!isGeneric('names')) setGeneric(name='names', function(x) standardGeneric('names'))
if (!isGeneric('ncats')) setGeneric(name='ncats', def=function(x) standardGeneric('ncats'))
if (!isGeneric('ncell')) setGeneric(name='ncell', def=function(x) standardGeneric('ncell'))
if (!isGeneric('ncell3d')) setGeneric(name='ncell3d', def=function(x) standardGeneric('ncell3d'))
if (!isGeneric('ncol')) setGeneric(name='ncol', def=function(x) standardGeneric('ncol'))
if (!isGeneric('ndepth')) setGeneric(name='ndepth', def=function(x) standardGeneric('ndepth'))
if (!isGeneric('nlyr')) setGeneric(name='nlyr', definition = function(x) standardGeneric('nlyr'))
if (!isGeneric('nrow')) setGeneric(name='nrow', def=function(x) standardGeneric('nrow'))

if (!isGeneric('rast')) setGeneric(name='rast', def=function(x) standardGeneric('rast'))
if (!isGeneric('res')) setGeneric(name='res', def=function(x) standardGeneric('res'))
if (!isGeneric('res3d')) setGeneric(name='res3d', def=function(x) standardGeneric('res3d'))

if (!isGeneric('st_crs')) setGeneric(name='st_crs', def = function(x, ...) standardGeneric('st_crs'))

if (!isGeneric('topology')) setGeneric(name='topology', def=function(x) standardGeneric('topology'))

if (!isGeneric('writeRaster')) setGeneric('writeRaster', function(x, ...) standardGeneric('writeRaster'))

if (!isGeneric('zExt')) setGeneric(name='zExt', def=function(x) standardGeneric('zExt'))
