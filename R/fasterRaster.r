#'fasterRaster: Faster raster processing in R using GRASS GIS
#'
#' This package is meant as an add-on to the \pkg{raster} package. It uses multi-core processing and GRASS GIS Version 7 to do some of the more laborious raster operations faster than what can be done using the raster package. For smaller rasters functions in the raster package may actually be faster. Also, many typical raster processing functions can now be done faster with \pck{terra}, the successor to the \pkg{raster} package. Note that to use many of the functions in this package you must have open-source (OS-GEO) GRASS 7 installed on your system.
#'
#' Create an issue on \href{https://github.com/adamlilith/fasterRaster/issues}{GitHub}.
#'
#' @details
#' @section Raster-only operations:
#' 		\code{\link{fasterBufferRast}}: Add buffer to cells in a raster (using GRASS).
#' 		\code{\link{fasterContour}}: Calculate contour vectors from a raster
#' 		\code{\link{fasterConvertDegree}}: Convert degrees from GRASS format (0 = east, 90 = north) to standard (0 = north, 90 = east) (using GRASS).
#' 		\code{\link{fasterFocal}}: Faster focal calculations (using multi-core).
#' 		\code{\link{fasterFragmentation}}: Fragmentation indices following Riitters et al. (2000 Conservation Ecology 4:3; using multi-core).
#' 		\code{\link{fasterHorizon}}: Horizon angle height from a DEM (using GRASS).
#' 		\code{\link{fasterLongLatRasters}}: Create rasters with values equal to cell longitude and latitude (using GRASS).
#' 		\code{\link{fasterProjectRaster}}: Project and resample raster (using GRASS).
#' 		\code{\link{fasterQuantile}}: Quantiles of values in a raster (using GRASS).
#' 		\code{\link{fasterRastDistance}}: Distance from cells with \code{NA}s to closest non-\code{NA} cell (or the inverse of this) (using GRASS).
#' 		\code{\link{fasterRasterize}}: Convert vector to a raster (using GRASS).
#' 		\code{\link{fasterSurfFractal}}: Generate a raster with a fractal pattern (using GRASS).
#' 		\code{\link{fasterTerrain}}: Slope, aspect, and curvature (using GRASS).
#' 		\code{\link{fasterTopidx}}: Topographic wetness index (using GRASS).
#' 		\code{\link{fasterVectorize}}: Convert raster to spatial points, lines, or polygons (using GRASS).
#' 		\code{\link{fasterVectToRastDistance}}: Distance between raster cells and a vector (using GRASS).

## Normal-speed raster processing ##
#' 		\code{\link{fragmentation}}: Calculate landscape fragmentation indices as per Riitter et al. (2000 Conservation Ecology 4:3)

## Utility functions ##
#' 		\code{\link{exportRasterToGrass}}: Export raster to an open GRASS session with support for large rasters/vectors.
#' 		\code{\link{exportVectToGrass}}: Export vector object to an open GRASS session with support for large rasters/vectors.
#' 		\code{\link{initGrass}}: Initialize a GRASS session using a raster or a vector as a template.
#'
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL
