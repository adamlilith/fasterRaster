#' @name regions
#'
#' @title An explanation of \code{GRASS} "regions"
#' 
#' @description Each \code{GRASS} session has a "region" which serves as a template for raster operations. A region does not contain pixel data, but it has a spatial extent and spatial resolution. These can be independent of any rasters or vectors in the \code{GRASS} session. In \code{GRASS}, regions are manipulated with the \code{\href{https://grass.osgeo.org/grass82/manuals/g.region.html}{g.region}} module.
#'
#' The region extent and resolution affects operations on rasters:
#' \itemize{
#' 	\item 	All operations conducted on a raster in \code{GRASS} affect the portion of the raster that overlaps with the current region.
#'
#'  \item	Importing the raster from \code{GRASS} to \code{R} using the \code{\link[rgrass]{read_RAST}} function in the \code{rgrass} package will crop the raster to the region. The raster may also be "padded" with rows and columns that are \code{NA} so that its extent matches the region. If the region has a different spatial resolution (or the same, but the region's registration is different from that of the raster), the raster will be resampled to match the resolution and registration of the region. You can import a raster as-is (i.e., not cropped, padded, resampled, or re-registered) from \code{GRASS} using \code{\link{rastFromGrass}}.
#'
#'  \item	Everything in the previous point also applies to saving a raster using \pkg{rgrass} functions \code{\link[rgrass]{execGRASS}} or \code{\link[rgrass]{parseGRASS}} with \code{GRASS} module \code{\href{https://grass.osgeo.org/grass82/manuals/r.out.gdal.html}{r.out.gdal}} or similar. The raster will be cropped, padded, resampled, and re-registered according to the region. To avoid this, you can use \code{\link{fasterWriteRaster}}.
#' }
#'
#' Regions generally do \emph{not} affect operations on vectors. The few exceptions are those that convert vectors to rasters, but these are again handled automatically by \pkg{fasterRaster} functions.
#'
#' With the exception of the "\code{\link{region}}" functions (e.g., \code{\link{regionExt}}, \code{\link{regionReshape}}), all functions in \pkg{fasterRaster} automatically manage the region so you do not need to. They set the extent and resolution of any rasters being operated on or saved. The few exceptions explicitly use redefinition of the region to crop, trim, or otherwise explicitly change raster operations, but these are also handled automatically. Users can turn off automatic handling of the region by setting the \code{autoRegion} argument that appears in most functions to \code{FALSE}, or for all functions at once using:
#'
#' \code{fasterOptions(autoRegion = FALSE)}
#'
#' It can be turned back on for a particular function by setting \code{autoRegion = TRUE} when using that function, or for all functions by using:
#'
#' \code{fasterOptions(autoRegion = TRUE)}
#'
#' The functions below can be used to view current region settings or to change them:
#'
#' \itemize{
#'	\item \code{\link{regionExt}}: Spatial extent.
#'	\item \code{\link{regionDim}}: Number of rows and columns.
#'	\item \code{\link{regionRes}}: Spatial resolution.
#'	\item \code{\link{regionReshape}}: Change the extent and resolution/dimensions of the region simultaneously.
#' }
#'
#' Since the number of rows and columns, spatial resolution, and spatial extent of a region are interdependent, changing one changes the others.
#'
#' Unlike \code{GRASS}, \pkg{fasterRaster} does not explicitly support multiple regions for the same location, but they could be manipulated using calls to \code{GRASS} module \code{\href{https://grass.osgeo.org/grass82/manuals/g.region.html}{g.region}} through \code{\link[rgrass]{execGRASS}} or \code{\link[rgrass]{parseGRASS}}.
#'
#' @keywords tutorial
#'
#' @example man/examples/ex_regions.r
#'
NULL
