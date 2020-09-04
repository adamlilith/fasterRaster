#' Initialize GRASS session and import raster and/or vector(s).
#'
#' This function is a generic worker function for a multi-core implementation of the \code{\link[raster]{focal}} function in the \pkg{raster} package. It is usually called by another function and is thus not of great use to most users.
#' @param i Integer > 0, ID number for the node on which calculations are being conducted.
#' @param blockVals Matrix.
#' @param w Matrix of weights (the moving window), e.g. a 3 by 3 matrix with values 1. The matrix does not need to be square, but the sides must be odd numbers. If you need even sides, you can add a column or row with weights of zero. Alternatively, \code{w} can be an odd integer > 0, in which case a weights matrix \code{w} by \code{w} in size is created with every value equal to 1.
#' @param fun Function. The function must accept multiple numbers and return a single number. For example \code{mean}, \code{min}, or \code{max}. The function should also accept a \code{na.rm} argument (or ignore it, e.g. as one of the 'dots' arguments). For example, \code{length} will fail, but \code{function(blockVals, ...) { na.omit(length(blockVals)) }} works. The default function is \code{sum}.
#' @param na.rm Logical, if code{FALSE} (default) the value computed by \code{fun} will be \code{NA} only if all cells in the window are \code{NA}. Using \code{na.rm = TRUE} is usually not a good idea since it can unbalance weights.
#' @param NAonly Logical, if \code{TRUE} then only cells with a value of \code{NA} have values replaced. Default is \code{FALSE}.
#' @return Matrix.
#' @keywords internal
.workerFocal <- function(i, blockVals, w, fun=sum, na.rm=TRUE, NAonly=FALSE) {

	halfWindowSize <- (nrow(w) - 1) / 2
	startRow <- halfWindowSize + 1
	endRow <- nrow(blockVals) - halfWindowSize
	startCol <- halfWindowSize + 1
	endCol <- ncol(blockVals) - halfWindowSize

	out <- NA * blockVals

	for (countRow in startRow:endRow) {
		for (countCol in startCol:endCol) {

			thisCellVal <- blockVals[countRow, countCol]
			if ((NAonly & is.na(thisCellVal)) || !is.na(thisCellVal)) {
				thisMat <- blockVals[(countRow - halfWindowSize):(countRow + halfWindowSize), (countCol - halfWindowSize):(countCol + halfWindowSize)]
				thisMat <- w * thisMat
				out[countRow, countCol] <- fun(thisMat, na.rm=na.rm)
			}
		}
	}

	out

}
