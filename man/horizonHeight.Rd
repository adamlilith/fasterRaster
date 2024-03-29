% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/horizonHeight.r
\name{horizonHeight,GRaster-method}
\alias{horizonHeight,GRaster-method}
\alias{horizonHeight}
\title{Horizon height}
\usage{
\S4method{horizonHeight}{GRaster}(
  x,
  units = "radians",
  directions = c(0, 90, 180, 270),
  northIs0 = TRUE,
  bufferZone = 0,
  distance = 1,
  maxDist = NULL
)
}
\arguments{
\item{x}{A \code{GRaster}.}

\item{units}{Character: Units of the height. Either \code{radians} (default) or \code{degrees}. Partial matching is used.}

\item{directions}{Numeric vector with positive integers between 0 and 359, inclusive: Direction(s) in which to calculate horizon height for each cell. By default, these are given in degrees clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. However, if you set \code{northIs0 = FALSE}, then the directions are given degrees counterclockwise from east, so east is 0, north 90, west 180, and south 270. Regardless, the default is to calculate horizon angle in all four directions. One raster is created per direction. Note that the output will be labeled according to the angle of the directions (e.g., \code{horizonHeight_090} will be horizon height facing east if \code{northIs0 = TRUE} (default), but horizon height facing north if \code{northIs0 = FALSE}. Note that \strong{GRASS} automatically rounds these values down to the nearest integer, so this function does the same but also produces a warning.}

\item{northIs0}{Logical: If \code{TRUE} (default), argument \code{directions} specifies horizon height in "north orientation," or clockwise from 0, so 0 is north, 90 east, 180 south, and 270 west. If \code{FALSE}, angles are in "east orientation", or counterclockwise from east, so east is 0, north 90, west 180, and south 270. North orientation is the default for this function in \strong{R}, but east orientation is the default in the \strong{GRASS} module \code{r.horizon}. \strong{Note:} The \code{\link[=sun]{sun()}} function requires aspect to be in east orientation.}

\item{bufferZone}{Numeric >= 0 (default is 0): A buffer of the specified width will be generated around the raster before calculation of horizon angle. If the coordinate system is in longitude/latitude (e.g., WGS84 or NAD83), then this is specified in degrees. Otherwise units are map units (usually meters).}

\item{distance}{Numeric between 0.5 and 1.5, inclusive (default is 1): This determines the step size when searching for the horizon from a given point. The default value of 1 goes cell-by-cell (i.e., search distance step size is one cell width).}

\item{maxDist}{Either \code{NULL} (default) or numeric >= 0: Maximum distance to consider when finding horizon height in meters. If \code{NULL}, the maximum distance is the full extent of the raster. Smaller values can decrease run time but also reduce accuracy.}
}
\value{
A \code{GRaster} with one or more layers. The layers will be named \code{height_}\emph{xyz}, where \emph{xyz} is degrees from north or from east, depending on whether north or east orientation is used.
}
\description{
\code{horizonHeight()} uses a raster representing elevation to calculate the height of the horizon in a particular direction from each cell on a raster. Height is expressed in radians or degrees from the horizontal.
}
\examples{
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# convert a SpatRaster to a GRaster
elev <- fast(madElev)

# calculate horizon height in north and east directions
hhNorthEast <- horizonHeight(elev, directions = c(0, 90))

# calculate horizon height in east and north directions
hhEastNorth <- horizonHeight(elev, directions = c(0, 90), northIs0 = FALSE)

}
}
\seealso{
The \strong{GRASS} module \code{r.horizon}.
}
