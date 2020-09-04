#'fasterRaster: Faster raster processing in R using GRASS GIS
#'
#' This package is meant as an add-on to the \pkg{raster} package. It uses GRASS GIS Version 7 and some multi-core processing to do some of the more laborious raster operations potentially faster than what can be done using the raster package. For smaller rasters functions in the raster package may actually be faster. Also, many typical raster processing functions can now be done faster with \pkg{terra}, the successor to the \pkg{raster} package. Note that to use many of the functions in this package you must have the stand-alone version of GRASS 7 installed on your system.
#'
#' Create an issue on \href{https://github.com/adamlilith/fasterRaster/issues}{GitHub}.
#'
#' @details
#' @section Raster-only operations:
#' 		\code{\link{fasterBufferRast}}: Add buffer to cells in a raster (using GRASS).\cr
#' 		\code{\link{fasterContour}}: Calculate contour vectors from a raster.\cr
#' 		\code{\link{fasterConvertDegree}}: Convert degrees from GRASS format (0 = east, 90 = north) to standard (0 = north, 90 = east) (using GRASS).\cr
#' 		\code{\link{fasterFocal}}: Faster focal calculations (using multi-core).\cr
#' 		\code{\link{fasterFragmentation}}: Fragmentation indices following Riitters et al. (2000 Conservation Ecology 4:3; using multi-core).\cr
#' 		\code{\link{fasterHorizon}}: Horizon angle height from a DEM (using GRASS).\cr
#' 		\code{\link{fasterLongLatRasters}}: Create rasters with values equal to cell longitude and latitude (using GRASS).\cr
#' 		\code{\link{fasterMapcalc}}: Apply user-defined function to one or more rasters (using GRASS; see also \code{fasterFocal}).\cr
#' 		\code{\link{fasterProjectRaster}}: Project and resample raster (using GRASS).\cr
#' 		\code{\link{fasterQuantile}}: Quantiles of values in a raster (using GRASS).\cr
#' 		\code{\link{fasterRastDistance}}: Distance from cells with \code{NA}s to closest non-\code{NA} cell (or the inverse of this) (using GRASS).\cr
#' 		\code{\link{fasterRasterize}}: Convert vector to a raster (using GRASS).\cr
#' 		\code{\link{fasterSun}}: Solar irradiance and radiance (using GRASS).\cr
#' 		\code{\link{fasterSurfFractal}}: Generate a raster with a fractal pattern (using GRASS).\cr
#' 		\code{\link{fasterTerrain}}: Slope, aspect, and curvature (using GRASS).\cr
#' 		\code{\link{fasterTopidx}}: Topographic wetness index (using GRASS).\cr
#' 		\code{\link{fasterVectorize}}: Convert raster to spatial points, lines, or polygons (using GRASS).\cr
#' 		\code{\link{fasterVectToRastDistance}}: Distance between raster cells and a vector (using GRASS).\cr
#' 
#' @section Normal-speed raster processing:
#' 		\code{\link{fragmentation}}: Calculate landscape fragmentation indices as per Riitter et al. (2000 Conservation Ecology 4:3)  
#' 
#' @section Utility functions:
#' 		\code{\link{exportRastToGrass}}: Export raster to an open GRASS session with support for large rasters/vectors.\cr
#' 		\code{\link{exportVectToGrass}}: Export vector object to an open GRASS session with support for large rasters/vectors.\cr
#' 		\code{\link{initGrass}}: Initialize a GRASS session using a raster or a vector as a template.\cr
#'
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL
