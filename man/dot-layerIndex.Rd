% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layerIndex.r
\name{.layerIndex}
\alias{.layerIndex}
\title{Get index of raster layers}
\usage{
.layerIndex(layer, x, recycle = TRUE, negate = FALSE)
}
\arguments{
\item{layer}{Integer, numeric, logical, or character: Refers to one or more layers.}

\item{x}{A \code{GRaster}.}

\item{recycle}{Logical: If \code{TRUE} (default), and \code{layer} is logical and smaller in number than the number of layers, then recycle the vector of \code{layer}.}

\item{negate}{Logical: If \code{TRUE}, return indices of all layers \emph{not} identified in \code{layer}.}
}
\value{
An integer vector.
}
\description{
Get index of raster layers
}
\keyword{internal}
