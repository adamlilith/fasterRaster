% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longlat.r
\name{longlat,GRaster-method}
\alias{longlat,GRaster-method}
\alias{longlat}
\title{Create longitude/latitude rasters}
\usage{
\S4method{longlat}{GRaster}(x)
}
\arguments{
\item{x}{A \code{GRaster}.}
}
\value{
A \code{GRaster} stack.
}
\description{
\code{longlat()} creates two rasters, one with cell values equal to the longitude of the cell centers, and one with cell values equal to the latitude of the cell centers. Values will be in decimal degrees, regardless of the projection of the raster.
}
\examples{
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

# Create longitude/latitude rasters
ll <- longlat(elev)
ll # note units of cell values!

}
}
