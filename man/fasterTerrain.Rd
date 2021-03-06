% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fasterTerrain.r
\name{fasterTerrain}
\alias{fasterTerrain}
\title{Calculate raster slope, aspect, curvature, and partial slopes.}
\usage{
fasterTerrain(
  rast,
  slope = TRUE,
  slopeUnits = "degrees",
  aspect = FALSE,
  northIs0 = TRUE,
  profileCurve = FALSE,
  tanCurve = FALSE,
  eastWestSlope = FALSE,
  northSouthSlope = FALSE,
  grassDir = NULL,
  alreadyInGrass = FALSE,
  grassToR = TRUE,
  outGrassNameSlope = "slope",
  outGrassNameAspect = ifelse(northIs0, "aspectNorthIs0", "aspectEastIs0"),
  outGrassNameProfileCurve = "profileCurve",
  outGrassNameTanCurve = "tanCurve",
  outGrassNameEastWestSlope = "eastWestSlope",
  outGrassNameNorthSouthSlope = "northSouthSlope",
  ...
)
}
\arguments{
\item{rast}{Either a raster with elevation data or the name of a raster in an existing GRASS session.}

\item{slope}{Logical, if \code{TRUE} (default) then calculate slope.}

\item{slopeUnits}{Character, "units" in which to calculate slope: either \code{degrees} for degrees or \code{percent}.}

\item{aspect}{Logical, if \code{TRUE} then calculate aspect. Aspect is given in degrees from North going clockwise (0 = north, 90 = east, 180 = south, 270 = west).}

\item{northIs0}{Logical. If \code{TRUE} (default), aspect will be reported such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If \code{FALSE}, then aspect will be reported such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in GRASS7.}

\item{profileCurve}{Logical, if \code{TRUE}, calculate profile curvature. Default is \code{FALSE}.}

\item{tanCurve}{Logical, if \code{TRUE}, calculate tangential curvature. Default is \code{FALSE}.}

\item{eastWestSlope}{Logical, if \code{TRUE}, calculate slope in east-west direction. Default is \code{FALSE}.}

\item{northSouthSlope}{Logical, if \code{TRUE}, calculate slope in north-south direction. Default is \code{FALSE}.}

\item{grassDir}{Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.}

\item{alreadyInGrass}{Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.}

\item{grassToR}{Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.}

\item{outGrassNameSlope}{Character. Name of slope raster in GRASS. Useful for referring to later in the same GRASS session.}

\item{outGrassNameAspect}{Character. Name of aspect raster in GRASS. Useful for referring to later in the same GRASS session.}

\item{outGrassNameProfileCurve}{Character. Name of profile curve raster in GRASS. Useful for referring to later in the same GRASS session.}

\item{outGrassNameTanCurve}{Character. Name of tangent curve raster in GRASS. Useful for referring to later in the same GRASS session.}

\item{outGrassNameEastWestSlope}{Character. Name of east-west slope raster in GRASS. Useful for referring to later in the same GRASS session.}

\item{outGrassNameNorthSouthSlope}{Character. Name of north-south slope raster in GRASS. Useful for referring to later in the same GRASS session.}

\item{...}{Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{r.slope.aspect} in GRASS).}

\item{outGrassName}{Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, raster(s) given by the names in the \code{outGrassName*} arguments are used are written into the GRASS session.}
}
\description{
This function is a potentially faster version of the \code{\link[raster]{terrain}} function in the \pkg{raster} package for calculating slope and aspect of a raster. It can also calculate profile curvature, tangential curvature, and slope in the east-west or north-south directions.
}
\details{
See the documentation for the GRASS module \code{r.slope.aspect} at \url{https://grass.osgeo.org/grass78/manuals/r.slope.aspect.html}.
}
\examples{
\donttest{
# change this according to where GRASS 7 is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac

data(madElev)

# could also use terrain() which may be faster
# in this example
topo <- fasterTerrain(rast=madElev, slope=TRUE, aspect=TRUE,
grassDir=grassDir)

# terrain function from the raster package... much slower in this example
# slp <- terrain(elev, opt='slope', unit='degrees')
# asp <- terrain(elev, opt='aspect', unit='degrees')
# topo <- stack(slp, asp)
# names(topo) <- c('slope', 'aspect')
plot(topo)
}
}
\seealso{
\code{\link[raster]{terrain}}
}
