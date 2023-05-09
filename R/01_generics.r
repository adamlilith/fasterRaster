.restore <- NULL
require(sf)
require(stars)
require(terra)

methods::setGeneric(name = 'as.contour', package='terra')
# if (!isGeneric('as.data.frame')) setGeneric('as.data.frame', function(x, row.names = NULL, optional = FALSE, ...) standardGeneric('as.data.frame'))
setGeneric('as.data.frame') # from base

methods::setGeneric(name = 'buffer', package='terra')

# 'c' already in base as S3 generic
methods::setGeneric(name = 'comparable', def = function(x, y, ...) standardGeneric('comparable'))
methods::setGeneric(name = 'connectors', def = function(x, y, ...) standardGeneric('connectors'))
methods::setGeneric(name = 'copyGSpatial', def = function(x, ...) standardGeneric('copyGSpatial'))
methods::setGeneric(name = 'convHull', def = function(x, ...) standardGeneric('convHull'))
# methods::setGeneric(name = 'crs', def = function(x, ...) standardGeneric('crs'))
methods::setGeneric(name = 'crs', package='terra')
methods::setGeneric(name = 'crop', package='terra')

# 'dim' already in base
methods::setGeneric(name = 'datatype', def = function(x, ...) standardGeneric('datatype'))
methods::setGeneric(name = 'distance', def = function(x, y, ...) standardGeneric('distance'))

methods::setGeneric('ext', package = 'terra')

methods::setGeneric(name = 'fast', def = function(x, ...) standardGeneric('fast'))
# methods::setGeneric(name = 'fastClass', def = function(x, ...) standardGeneric('fastClass'))

methods::setGeneric(name = 'geomtype', package='terra')
methods::setGeneric(name = 'gnames', def = function(x) standardGeneric('gnames'))

methods::setGeneric(name = 'is.2d', def = function(x) standardGeneric('is.2d'))
methods::setGeneric(name = 'is.3d', def = function(x) standardGeneric('is.3d'))
methods::setGeneric(name = 'is.lines', package='terra')
methods::setGeneric(name = 'is.points', package='terra')
methods::setGeneric(name = 'is.polygons', package='terra')

methods::setGeneric(name = 'location', def = function(x) standardGeneric('location'))

methods::setGeneric(name = 'mapset', def = function(x) standardGeneric('mapset'))
methods::setGeneric(name = 'minmax', package='terra')

# 'names' already in base
methods::setGeneric(name = 'ncat', def = function(x) standardGeneric('ncat'))
methods::setGeneric(name = 'ncell', package='terra')
methods::setGeneric(name = 'ncell3d', def = function(x) standardGeneric('ncell3d'))
methods::setGeneric(name = 'ndepth', def = function(x) standardGeneric('ndepth'))
methods::setGeneric(name = 'nlyr', package='terra')
methods::setGeneric(name = 'ncol', def = function(x) standardGeneric('ncol')) # in base
methods::setGeneric(name = 'nrow', def = function(x) standardGeneric('nrow')) # in base

methods::setGeneric(name = 'rast', package='terra')
methods::setGeneric(name = 'regionDim', def=function(x, ...) standardGeneric('regionDim'))
methods::setGeneric(name = 'regionExt', def=function(x, ...) standardGeneric('regionExt'))
methods::setGeneric(name = 'regionRes', def=function(x, ...) standardGeneric('regionRes'))
methods::setGeneric(name = 'regionShape', def=function(x, ...) standardGeneric('regionShape'))
methods::setGeneric(name = 'regionZExt', def=function(x, ...) standardGeneric('regionZExt'))
methods::setGeneric(name = 'res', package='terra')
methods::setGeneric(name = 'res3d', def = function(x) standardGeneric('res3d'))

if (!isGeneric('st_bbox')) methods::setGeneric(name = 'st_bbox', def=function(obj, ...) standardGeneric('st_bbox'))
methods::setGeneric(name = 'st_buffer', def=function(x, ...) standardGeneric('st_buffer'))
if (!isGeneric('st_crs')) methods::setGeneric(name = 'st_crs', def=function(x, ...) standardGeneric('st_crs'))
methods::setGeneric(name = 'st_distance', def=function(x, y, ...) standardGeneric('st_distance'))

methods::setGeneric(name = 'topology', def=function(x, ...) standardGeneric('topology'))

methods::setGeneric(name = 'vect', package='terra')

methods::setGeneric(name = 'writeRaster', def=function(x, filename, ...) standardGeneric('writeRaster'))
methods::setGeneric(name = 'writeVector', def=function(x, filename, ...) standardGeneric('writeVector'))

methods::setGeneric(name = 'zext', def = function(x) standardGeneric('zext'))
