% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copyGSpatial.r
\name{.copyGSpatial,GRaster-method}
\alias{.copyGSpatial,GRaster-method}
\alias{.copyGSpatial}
\alias{.copyGSpatial,GVector-method}
\alias{.copyGSpatial,character-method}
\alias{.copyGRaster}
\alias{.copyGVector}
\title{Make a copy of an object in GRASS}
\usage{
\S4method{.copyGSpatial}{GRaster}(x, reshapeRegion = TRUE)

\S4method{.copyGSpatial}{GVector}(x)

\S4method{.copyGSpatial}{character}(x, type = NULL, topo = NULL, reshapeRegion = TRUE)

.copyGRaster(x, topo = "2D", reshapeRegion = TRUE)

.copyGVector(x)
}
\arguments{
\item{x}{A \code{GVector} or the \code{\link[=sources]{sources()}} name of one.}

\item{reshapeRegion}{Logical. If \code{TRUE}, \code{x} must be a \code{GRaster}.}

\item{type}{Character or \code{NULL} (default): Either "raster" or "vector". If a character, there must be one per value in \code{x}. If \code{NULL}, will attempt to auto-detect (takes longer).}

\item{topo}{"2D" or "3D"}
}
\value{
Character vector representing the \code{\link[=sources]{sources()}} names of the copied object(s), plus makes a copy of the given object(s) in \strong{GRASS}.

\code{\link[=sources]{sources()}} names of copied rasters.
}
\description{
Create a copy of a \code{GRaster} or \code{GVector} in \strong{GRASS}.  This function is used internally and is of little use to most users.  This only creates a copy of the object in the \strong{GRASS} session--to make a \code{GRaster} or \code{GVector}, \code{\link[=.makeGRaster]{.makeGRaster()}} or \code{\link[=.makeGVector]{.makeGVector()}} need to be called after making the copy. Note that if the object is multi-layered, then a copy is made of each layer.
}
\keyword{internal}
