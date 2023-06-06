.restore <- NULL
require(sf)
require(stars)
require(terra)

methods::setGeneric(name = 'as.contour', package='terra')
methods::setGeneric(name = 'as.cell', def = function(x, ...) standardGeneric('as.cell'))
methods::setGeneric(name = 'as.fcell', def = function(x, ...) standardGeneric('as.fcell'))
methods::setGeneric(name = 'as.dcell', def = function(x, ...) standardGeneric('as.dcell'))
setGeneric('as.data.frame')

methods::setGeneric(name = 'bottom', def = function(x, ...) standardGeneric('bottom'))
methods::setGeneric(name = 'buffer', package='terra')

# 'c' already in base as S3 generic
methods::setGeneric(name = 'comparable', def = function(x, y, ...) standardGeneric('comparable'))
methods::setGeneric(name = 'connectors', def = function(x, y, ...) standardGeneric('connectors'))
methods::setGeneric(name = 'copyGSpatial', def = function(x, ...) standardGeneric('copyGSpatial'))
methods::setGeneric(name = 'convHull', def = function(x, ...) standardGeneric('convHull'))
methods::setGeneric(name = 'count', def = function(x, ...) standardGeneric('count'))
methods::setGeneric(name = 'crds', def = function(x, ...) standardGeneric('crds'))
methods::setGeneric(name = 'crs', package='terra')
methods::setGeneric(name = 'crop', package='terra')

# 'dim' already in base
methods::setGeneric(name = 'datatype', def = function(x, ...) standardGeneric('datatype'))
methods::setGeneric('delaunay', package = 'terra')
methods::setGeneric(name = 'distance', def = function(x, y, ...) standardGeneric('distance'))
methods::setGeneric(name = 'nunique', def = function(x, ...) standardGeneric('nunique'))

methods::setGeneric(name = 'east', def=function(x, ...) standardGeneric('east'))
methods::setGeneric(name = 'ewres', def=function(x, ...) standardGeneric('ewres'))
methods::setGeneric('ext', package = 'terra')

methods::setGeneric(name = 'fast', def = function(x, ...) standardGeneric('fast'))
methods::setGeneric(name = 'freq', package='terra')

methods::setGeneric(name = 'geomtype', package='terra')
methods::setGeneric(name = 'global', package='terra')
methods::setGeneric(name = 'gnames', def = function(x) standardGeneric('gnames'))

methods::setGeneric(name = 'hillshade', def=function(x, ...) standardGeneric('hillshade'))
methods::setGeneric(name = 'horizonHeight', def = function(x, ...) standardGeneric('horizonHeight'))

methods::setGeneric(name = 'is.2d', def = function(x) standardGeneric('is.2d'))
methods::setGeneric(name = 'is.3d', def = function(x) standardGeneric('is.3d'))
methods::setGeneric(name = 'is.lines', package='terra')
methods::setGeneric(name = 'is.points', package='terra')
methods::setGeneric(name = 'is.polygons', package='terra')
methods::setGeneric(name = 'intercept', def=function(x, ...) standardGeneric('intercept'))

methods::setGeneric(name = 'kurtosis', def=function(x, ...) standardGeneric('kurtosis'))

methods::setGeneric(name = 'location', def = function(x) standardGeneric('location'))

methods::setGeneric(name = 'mapset', def = function(x) standardGeneric('mapset'))
methods::setGeneric(name = 'minmax', package='terra')
methods::setGeneric(name = 'merge', def = function(x, y, ...) standardGeneric('merge'))
methods::setGeneric(name = 'mmode', def = function(x, ...) standardGeneric('mmode'))

# 'names' already in base
methods::setGeneric(name = 'ncat', def = function(x) standardGeneric('ncat'))
methods::setGeneric(name = 'ncell', package='terra')
methods::setGeneric(name = 'ncell3d', def = function(x) standardGeneric('ncell3d'))
methods::setGeneric(name = 'ndepth', def = function(x) standardGeneric('ndepth'))
methods::setGeneric(name = 'nlyr', package='terra')
methods::setGeneric(name = 'north', def=function(x, ...) standardGeneric('north'))
methods::setGeneric(name = 'nsres', def=function(x, ...) standardGeneric('nsres'))
methods::setGeneric(name = 'ncol', def = function(x) standardGeneric('ncol')) # in base
methods::setGeneric(name = 'nrow', def = function(x) standardGeneric('nrow')) # in base

methods::setGeneric(name = 'project', package='terra')

methods::setGeneric(name = 'r2', def=function(x, ...) standardGeneric('r2'))
methods::setGeneric(name = 'rast', package='terra')
methods::setGeneric(name = 'regionDim', def=function(x, ...) standardGeneric('regionDim'))
methods::setGeneric(name = 'regionExt', def=function(x, ...) standardGeneric('regionExt'))
methods::setGeneric(name = 'regionRes', def=function(x, ...) standardGeneric('regionRes'))
methods::setGeneric(name = 'regionRes3d', def=function(x, ...) standardGeneric('regionRes3d'))
methods::setGeneric(name = 'region', def=function(x, ...) standardGeneric('region'))
methods::setGeneric(name = 'regionZDim', def=function(x, ...) standardGeneric('regionZDim'))
methods::setGeneric(name = 'regionZExt', def=function(x, ...) standardGeneric('regionZExt'))
methods::setGeneric(name = 'regionZRes', def=function(x, ...) standardGeneric('regionZRes'))
methods::setGeneric(name = 'res', package='terra')
methods::setGeneric(name = 'resample', package='terra')
methods::setGeneric(name = 'res3d', def = function(x) standardGeneric('res3d'))

methods::setGeneric(name = 'sdpop', def=function(x, ...) standardGeneric('sdpop'))
methods::setGeneric(name = 'skewness', def=function(x, ...) standardGeneric('skewness'))
methods::setGeneric(name = 'slope', def=function(x, ...) standardGeneric('slope'))
methods::setGeneric(name = 'south', def=function(x, ...) standardGeneric('south'))
methods::setGeneric(name = 'stretch', package='terra')
if (!isGeneric('st_bbox')) methods::setGeneric(name = 'st_bbox', def=function(obj, ...) standardGeneric('st_bbox'))
methods::setGeneric(name = 'st_buffer', def=function(x, ...) standardGeneric('st_buffer'))
if (!isGeneric('st_crs')) methods::setGeneric(name = 'st_crs', def=function(x, ...) standardGeneric('st_crs'))
methods::setGeneric(name = 'st_distance', def=function(x, y, ...) standardGeneric('st_distance'))

methods::setGeneric(name = 'top', def=function(x, ...) standardGeneric('top'))
methods::setGeneric(name = 'topology', def=function(x, ...) standardGeneric('topology'))
methods::setGeneric(name = 'tvalue', def=function(x, ...) standardGeneric('tvalue'))

methods::setGeneric(name = 'varpop', def=function(x, ...) standardGeneric('varpop'))
methods::setGeneric(name = 'vect', package='terra')
methods::setGeneric(name = 'voronoi', package='terra')

methods::setGeneric(name = 'west', def=function(x, ...) standardGeneric('west'))
methods::setGeneric(name = 'writeRaster', def=function(x, filename, ...) standardGeneric('writeRaster'))
methods::setGeneric(name = 'writeVector', def=function(x, filename, ...) standardGeneric('writeVector'))

methods::setGeneric(name = 'zext', def = function(x) standardGeneric('zext'))
methods::setGeneric(name = 'zres', def = function(x) standardGeneric('zres'))
