% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exists.r
\name{.exists,GRaster-method}
\alias{.exists,GRaster-method}
\alias{.exists}
\alias{.exists,GVector-method}
\alias{.exists,character-method}
\title{Does the "GRASS" representation of a GRaster or GVector exist?}
\usage{
\S4method{.exists}{GRaster}(x)

\S4method{.exists}{GVector}(x)

\S4method{.exists}{character}(x)
}
\arguments{
\item{x}{A \code{GRaster}, \code{GVector}, or the \code{\link[=sources]{sources()}} name of one.}
}
\value{
Logical.
}
\description{
\code{GRaster}s and \code{GVector}s are \strong{R} objects that contain a pointer to a raster or vector in \strong{GRASS}. Thus, for a \code{GRaster} or \code{GVector} to be functional, the \strong{GRASS} file must exist. This function indicates if that is so.
}
\keyword{internal}
