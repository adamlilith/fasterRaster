% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workDir.r
\name{.workDir,GLocation-method}
\alias{.workDir,GLocation-method}
\alias{.workDir}
\alias{.workDir,missing-method}
\title{Get a GLocation's working directory}
\usage{
\S4method{.workDir}{GLocation}(x)

\S4method{.workDir}{missing}(x)
}
\arguments{
\item{x}{A \code{GLocation} object or missing. If an object, returns the working folder in which the object is saved by \strong{GRASS}. If missing, then just returns the working folder (same as \code{faster("workDir")}).}
}
\value{
Character.
}
\description{
This function returns the working directory of a \code{GLocation} object.
}
\keyword{internal}
