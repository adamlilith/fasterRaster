% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterize.r
\name{rasterize,GVector,GRaster-method}
\alias{rasterize,GVector,GRaster-method}
\alias{rasterize}
\title{Convert a GVector to a GRaster}
\usage{
\S4method{rasterize}{GVector,GRaster}(x, y, field = "", background = NA, by = NULL, verbose = TRUE)
}
\arguments{
\item{x}{A \code{GVector}.}

\item{y}{A \code{GRaster}: The new raster will have the same extent and resolution as this raster.}

\item{field}{Character: Name of a column in the data table of \code{y} to "burn" into the raster. If not \code{""} (default), then the output will be a \link[=tutorial_raster_data_types]{categorical} raster. If \code{field} is \code{""}, then all geometries will be "burned" to the raster and have the same value.}

\item{background}{Numeric or \code{NA} (default): Value to put in cells that are not covered by the \code{GVector}. Note that if this is not \code{NA} and not an integer, then the output cannot be a categorical raster (i.e., there will be no "levels" table associated with it).}

\item{by}{Either \code{NULL} (default) or character: If this is not \code{NULL}, then the \code{GVector} will be subset by the values in the field named by \code{by}. The output will be a multi-layer raster, with one layer per unique value in \code{by}.}

\item{verbose}{Logical: If \code{by} is not \code{NULL}, display progress.}
}
\value{
A \code{GRaster}.
}
\description{
The \code{rasterize()} function converts a \code{GVector} into a \code{GRaster}.
}
\examples{
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Elevation raster, outline of a part of Madagascar, and rivers vector:
madElev <- fastData("madElev") # raster
madDypsis <- fastData("madDypsis") # points vector
madRivers <- fastData("madRivers") # lines vector
madCoast4 <- fastData("madCoast4") # polygons vector

# Convert to GRaster and GVectors:
elev <- fast(madElev)
dypsis <- fast(madDypsis)
coast4 <- fast(madCoast4)
rivers <- fast(madRivers)

# Convert points, line, and polygons vectors to rasters:
points <- rasterize(dypsis, elev)
lines <- rasterize(rivers, elev)
polys <- rasterize(coast4, elev)
communes <- rasterize(coast4, elev, field = "NAME_4")

plot(points)
plot(lines)
plot(polys)
plot(communes)

# Make one layer per river:
byRiver <- rasterize(rivers, elev, field = "NAM", by = "NAM")
plot(byRiver)

# Change background value:
polys <- rasterize(coast4, elev, background = -1)
plot(polys)

# By geometry onto one rasters:
polys <- rasterize(coast4, elev, byGeom = TRUE, collapse = TRUE)
plot(polys)

# By geometry, separate rasters:
polys <- rasterize(coast4, elev, byGeom = TRUE, collapse = FALSE)
plot(polys)

}
}
\seealso{
\code{\link[terra:rasterize]{terra::rasterize()}}, module \href{https://grass.osgeo.org/grass84/manuals/v.to.rast.html}{\code{v.to.rast}} in \strong{GRASS}
}
