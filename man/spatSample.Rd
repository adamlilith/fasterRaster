% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatSample.r
\name{spatSample,GRaster-method}
\alias{spatSample,GRaster-method}
\alias{spatSample}
\alias{spatSample,GVector-method}
\title{Sample random points from a GRaster or GVector}
\usage{
\S4method{spatSample}{GRaster}(
  x,
  size,
  as.points = FALSE,
  values = TRUE,
  cat = FALSE,
  xy = FALSE,
  strata = NULL,
  zlim = NULL,
  seed = NULL
)

\S4method{spatSample}{GVector}(
  x,
  size,
  as.points = FALSE,
  values = TRUE,
  xy = FALSE,
  strata = NULL,
  zlim = NULL,
  seed = NULL
)
}
\arguments{
\item{x}{A \code{GRaster} or \code{GVector}.}

\item{size}{Numeric integer or integer > 0: Number of points to create.}

\item{as.points}{Logical: If \code{FALSE} (default), the output is a \code{data.frame} or \code{data.table}. If \code{TRUE}, the output is a "points" \code{GVector}.}

\item{values}{Logical: If \code{TRUE} (default), values of the \code{GRaster} at points are returned.}

\item{cat}{Logical: If \code{TRUE} and the \code{GRaster} is \link[=tutorial_raster_data_types]{categorical}, then return the category label of each cell. If \code{values} is also \code{TRUE}, then the cell value will also be returned.}

\item{xy}{Logical: If \code{TRUE}, return the longitude and latitude of each point. Default is \code{FALSE}.}

\item{strata}{Either \code{NULL} (default), or a \code{GVector} defining strata. If supplied, the \code{size} argument will be interpreted as number of points to place per geometry in \code{strata}.}

\item{zlim}{Either \code{NULL} (default), or a vector of two numbers defining the lower and upper altitudinal bounds of coordinates.}

\item{seed}{Either \code{NULL} (default) or an integer: Used as the random seed. If \code{NULL}, then \strong{GRASS} will generate its own seed.}
}
\value{
A \code{data.frame}, \code{data.table}, or \code{GVector}.
}
\description{
\code{spatSample()} randomly locates points across a \code{GRaster} or \code{GVector}. It can return a \code{GVector}, the coordinates, values associated with the points, or all of these. If you want to generate a raster with randomly-sampled cells, see \code{\link[=sampleRast]{sampleRast()}}.
}
\examples{
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # raster
madForest <- fastData("madForest2000") # raster
madCoast <- fastData("madCoast4") # vector

# Convert to GRasters and GVectors
elev <- fast(madElev)
forest <- fast(madForest)
coast <- fast(madCoast)

ant <- coast[coast$NAME_4 == "Antanambe"] # subset

# Random cells in non-NA cells:
rand <- sampleRast(elev, 10000)
plot(rand)
nonnacell(rand)

# Use custom values for the mask:
randCustomMask <- sampleRast(elev, 10000, maskvalues = 1:20)
plot(randCustomMask)

# Force selected values to a custom value:
randCustomUpdate <- sampleRast(elev, 10000, updatevalue = 7)
plot(randCustomUpdate)

# Custom values for mask and set selected cells to custom value:
randAll <- sampleRast(elev, 10000, maskvalues = 1:20, updatevalue = 7)
plot(randAll)

}
}
\seealso{
\code{\link[=sampleRast]{sampleRast()}}, \code{\link[terra:sample]{terra::spatSample()}}, module \code{v.random} in \strong{GRASS}
}
