% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fasterRasterize.r
\name{fasterRasterize}
\alias{fasterRasterize}
\title{Rasterize using a call to GRASS GIS.}
\usage{
fasterRasterize(
  vect,
  rast,
  use = "value",
  value = 1,
  field = NULL,
  burn = NULL,
  grassDir = NULL,
  alreadyInGrass = FALSE,
  grassToR = TRUE,
  outGrassName = "vectToRast",
  ...
)
}
\arguments{
\item{vect}{Either a SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame or the name of such a vector dataset already in a GRASS session.}

\item{rast}{Either a raster or the name of a raster in an existing GRASS session. This serves as a template for the new raster.}

\item{use}{Character, indicates the types of values to be "burned" to the raster. Options include
\itemize{
\item \code{value} (default): a user-define value given by \code{value}
\item \code{field}: values directly from a field in \code{vect} named by \code{field}
\item \code{category}: values according to which polygon is covered by a cell named in \code{field}
\item \code{z}: z-coordinate (points or contours only)
\item \code{direction}: flow direction (lines only)
}}

\item{value}{Numeric, value to burn to each cell overlapped by the spatial object in \code{vect}.}

\item{field}{Name of column in \code{vect} with values or category labbels to which to burn to the raster.}

\item{burn}{\code{NULL} or any of \code{'point'}, \code{'line'}, \code{'area'}, \code{'boundary'}, or \code{'centroid'}. This determines the manner in which the vector data is "burned" to the raster. If \code{NULL} (default), then SpatialPoints and SpatialPointsDataFrame objects will rasterized as points, SpatialLines and SpatialLinesDataFrame objects as lines, and SpatialPolygons and SpatialPolygonsDataFrame objects as areas. See \url{https://grass.osgeo.org/grass78/manuals/v.to.rast.html} for more details.}

\item{grassDir}{Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.}

\item{alreadyInGrass}{Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.}

\item{grassToR}{Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{vectToRast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.}

\item{outGrassName}{Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.}

\item{...}{Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{v.to.rast} in GRASS).}
}
\value{
If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster with the name given by \code{outRastName} is written into the GRASS session.
}
\description{
This function is a potentially faster version of the \code{\link[raster]{rasterize}} function in the \pkg{raster} package which converts a spatial points, lines, or polygon into a raster based on a "template" raster. All cells covered by the spatial object can either have values taken from the spatial object or a user-defined.
}
\details{
See the documentation for the GRASS module \code{v.to.rast} at \url{https://grass.osgeo.org/grass78/manuals/v.to.rast.html}.
}
\examples{
\donttest{
# change this according to where GRASS 7 is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac

data(mad0)
data(madForest2000)

# could also use rasterize() or mask() from the raster package which may
# be faster in this example
madMask <- fasterRasterize(vect=mad0, rast=madForest2000, grassDir=grassDir)
# madMask <- rasterize(mad0, madForest2000)
# madMask <- mask(madForest2000, mad0)
plot(madMask, main='Portion of Eastern Madagascar')
plot(mad0, add=TRUE)
}
}
\seealso{
\code{\link[raster]{rasterize}}
}
