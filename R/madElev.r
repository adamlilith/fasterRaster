#' Spatial data: digital elevation model (DEM) of a portion of Madagascar
#'
#' Rasterized digital elevation model (DEM) of a portion of Madagascar at 7.5-arcmin spatial resolution.
#'
#' @docType data
#'
#' @usage data(madElev)
#'
#' @format An object of class \code{'raster'}. See \code{\link[raster]{raster}}. Values are mean meters above sea level.
#'
#' @keywords datasets
#'
#' @references Danielson, J.J. and Gesch, D.B. 2010. Global Multi-resolution Terrain Elevation Data 2010 (GMTED2010). US Geological Survey. Open-File Report 2011-1073 (\href{https://pubs.usgs.gov/of/2011/1073/pdf/of2011-1073.pdf}{GMTED2010})
#'
#' @source \href{https://lta.cr.usgs.gov/GMTED2010}{GMTED2010}
#'
#' @examples
#' data(madElev)
#' plot(madElev)
'madElev'
