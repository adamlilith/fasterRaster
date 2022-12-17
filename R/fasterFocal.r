#' Multi-core focal() function
#'
#' This function applies a function that uses values from a "moving window" across a raster. It is exactly the same as the \code{\link[terra]{focal}} function in the \pkg{terra} package except that it can use a multi-core implementation to speed processing.
#' @param rast A \code{SpatRaster}.
#' @param w Matrix of weights (the moving window), e.g. a 3 by 3 matrix with values 1. The matrix does not need to be square, but the sides must be odd numbers. If you need even sides, you can add a column or row with weights of zero. Alternatively, \code{w} can be an odd integer > 0, in which case a weights matrix \code{w} by \code{w} in size is created with every value equal to 1.
#' @param fun Function. The function must accept multiple numbers and return a single number. For example \code{mean}, \code{min}, or \code{max}. The function should also accept a \code{na.rm} argument (or ignore it, e.g. as one of the 'dots' arguments). For example, \code{length} will fail, but \code{function(rast, ...) { na.omit(length(rast)) }} works. The default function is \code{sum}.
#' @param filename Character, name of file for a new raster (optional).
#' @param na.rm Logical, if code{FALSE} (default) the value computed by \code{fun} will be \code{NA} only if all cells in the window are \code{NA}. Using \code{na.rm = TRUE} is usually not a good idea since it can unbalance weights.
#' @param pad Logical, if \code{TRUE} then add virtual rows and columns around the raster so that there are no edge effects. The virtual rows and columns are set to equal \code{padValue}. Default is \code{FALSE}.
#' @param padValue Value to which to set the values of the virtual cells used to pad the raster if \code{pad} is \code{TRUE}.
#' @param progress Logical, if \code{TRUE} display a progress bar. Only works if using multi-core calculation.
#' @param NAonly Logical, if \code{TRUE} then only cells with a value of \code{NA} have values replaced. Default is \code{FALSE}.
#' @param cores Integer >0, number of CPU cores to use to calculate the focal function (default is 2).
#' @param ... Arguments to pass to \code{\link[terra]{writeRaster}}
#'
#' @details The function \code{\link{fasterMapcalc}} \emph{may} be faster.
#'
#' @return A raster object, possibly also written to disk.
#'
#' @seealso \code{\link[terra]{focal}}, \code{\link{fasterMapcalc}}
#'
#' @examples man/examples/ex_fasterFocal.r
#' 
#' @export
fasterFocal <- function(
	rast,
	w = 3,
	fun = sum,
	filename = '',
	na.rm = FALSE,
	pad = FALSE,
	padValue = NA,
	NAonly = FALSE,
	progress = FALSE,
	cores = 2,
	...
) {

	if (inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)

	# get number of cores and chunks of raster
	cores <- .getCores(rast = rast, cores = cores)
	blocks <- raster::blockSize(rast, minblocks=cores)
	
	# single core
	if (cores == 1 | blocks$n == 1) {
	
		out <- terra::focal(x=rast, w=w, fun=fun, filename=filename, na.rm=na.rm, pad=pad, padValue=padValue, NAonly=NAonly, ...)
		
	# multi-core
	} else {

		# weights matrix
		if (inherits(w, 'matrix')) {
		
			badWeights <- if (nrow(w) != ncol(w)) {
				TRUE
			} else if (nrow(w) %% 2 != 1 | ncol(w) %% 2 != 1) {
				TRUE
			} else {
				FALSE
			}
		
		} else if (!inherits(w, 'matrix') & length(w) == 1) {
		
			if (w %% 2 != 1) {
				badWeights <- FALSE
			} else {
				w <- matrix(1, ncol=w, nrow=w)
				badWeights <- FALSE
			}
		
		} else {
			badWeights <- TRUE
		}
		
		if (badWeights) stop('Argument "w" in fasterFocal() function must be an odd integer or a square matrix with an odd number of columns and rows.')
	
		halfWindowSize <- (nrow(w) - 1L) / 2L

		# add padding around raster to avoid edge effects
		if (pad) {
			origExtent <- terra::ext(rast)
			rast <- terra::extend(rast, y=halfWindowSize, value=padValue)
		}
	
		out <- terra::rast(rast)

		# calculate start/end position and size of each block to be sent to a core
		# and start/end position and size of each section in a block that is processed
		# account for overlap between blocks because of using a window
		# rows sent to nodes are "send"
		# rows which receive values are "process"
		maxBlockSize <- blocks$nrows[1]
		startSendRows <- seq(1, nrow(rast), by=maxBlockSize - 2 * halfWindowSize)
		endSendRows <- startSendRows + maxBlockSize - 1
		if (any(endSendRows > nrow(rast))) endSendRows[endSendRows > nrow(rast)] <- nrow(rast)
		numSendRows <- endSendRows - startSendRows + 1
		if (any(numSendRows < nrow(w))) {
			tooSmalls <- which(numSendRows < nrow(w))
			startSendRows <- startSendRows[-tooSmalls]
			endSendRows <- endSendRows[-tooSmalls]
			numSendRows <- numSendRows[-tooSmalls]
		}
		
		startProcessRows <- startSendRows + halfWindowSize
		endProcessRows <- endSendRows - halfWindowSize
		numProcessRows <- endProcessRows - startProcessRows + 1
		
		### initiate cluster
		nodes <- blocks$n
		raster::beginCluster(nodes)
		cluster <- raster::getCluster()
		# on.exit(raster::endCluster())
		on.exit(snow::stopCluster(cluster))

		### start nodes calculating
		xCols <- ncol(rast)

		# data frame to track what's been sent to what "tag" node
		tracker <- data.frame(
			job = seq_along(startSendRows),
			tag = NA,
			sent = FALSE,
			done = FALSE,
			startSendRow = startSendRows,
			endSendRow = endSendRows,
			numSendRow = numSendRows,
			startProcessRow = startProcessRows,
			endProcessRow = endProcessRows,
			numProcessRow = numProcessRows
		)
		
		if (progress) pb <- pbCreate(nrow(tracker))

		for (tag in 1L:nodes) {

			blockVals <- getValues(rast, startSendRows[tag], numSendRows[tag])
			blockVals <- matrix(blockVals, ncol=xCols, byrow=TRUE)
			
			snow::sendCall(cluster[[tag]], fun=workerFocal, args=list(blockVals=blockVals, w=w, fun=fun, na.rm=na.rm, NAonly=NAonly), tag=tag)
			
			tracker$tag[tag] <- tag
			tracker$sent[tag] <- TRUE
		
		}
		
		# initiate file names if needed
		filename <- terra::trim(filename)
		if (!raster::canProcessInMemory(out) & filename == '') {
			filename <- terra::rastTmpFile()
			out <- raster::writeStart(out, filename=filename, ... )
		}

		if (filename != '') {
			out <- raster::writeStart(out, filename=filename, ... )
		} else {
			out <- matrix(nrow=nrow(rast), ncol=ncol(rast))
		}
		
		### get and remember output
		while (sum(tracker$done) < nrow(tracker)) {

			thisTag <- tracker$tag[which(!tracker$done)[1L]]
		
			# receive results from a node
			outFromClust <- snow::recvData(cluster[[thisTag]])
			if (!outFromClust$success) stop('Cluster error.')

			job <- which(outFromClust$tag == tracker$tag & tracker$sent & !tracker$done)
			tag <- outFromClust$tag
			tracker$done[job] <- TRUE
			
			# trim output to processed region
			valsFromClust <- outFromClust$value
			valsFromClust <- valsFromClust[(halfWindowSize + 1):(nrow(valsFromClust) - halfWindowSize), ]
			
			# get block
			if (filename != '') {
				valsFromClust <- c(t(valsFromClust))
				out <- raster::writeValues(out, v=valsFromClust, start=startProcessRows[job])
			} else {
				out[startProcessRows[job]:endProcessRows[job], 1:ncol(rast)] <- valsFromClust
			}

			# need to send more data?
			if (sum(tracker$sent) < nrow(tracker)) {

				job <- which(!tracker$sent & !tracker$done)[1]
				blockVals <- getValues(rast, startSendRows[job], numSendRows[job])
				blockVals <- matrix(blockVals, ncol=xCols, byrow=TRUE)
				
				snow::sendCall(cluster[[tag]], fun=workerFocal, args=list(blockVals=blockVals, w=w, fun=fun, na.rm=na.rm, NAonly=NAonly), tag=tag)
				
				tracker$tag[job] <- tag
				tracker$sent[job] <- TRUE
			
			}
			
			if (progress) pbStep(pb)
			
		} # while not all chunks have been returned

		if (filename != '') {
			
			# write NAs to border rows at top and bottom
			fills <- rep(NA, ncol(rast) * halfWindowSize)
			raster::writeValues(out, v=fills, start=1)
			raster::writeValues(out, v=fills, start=nrow(rast) - halfWindowSize + 1)

			out <- writeStop(out)

		} else {
		
			out <- raster(out)
			terra::crs(out) <- terra::crs(rast)
			terra::ext(out) <- terra::ext(rast)
		
		}
		
		if (progress) pbClose(pb)
		
	} # if multi-core

	if (pad) out <- terra::crop(out, origExtent)
	out

}

#' Initialize \code{GRASS} session and import raster and/or vector(s).
#'
#' This function is a generic worker function for a multi-core implementation of the \code{\link[terra]{focal}} function in the \pkg{terra} package. It is usually called by another function and is thus not of great use to most users.
#' @param i Integer > 0, ID number for the node on which calculations are being conducted.
#' @param blockVals Matrix.
#' @param w Matrix of weights (the moving window), e.g. a 3 by 3 matrix with values 1. The matrix does not need to be square, but the sides must be odd numbers. If you need even sides, you can add a column or row with weights of zero. Alternatively, \code{w} can be an odd integer > 0, in which case a weights matrix \code{w} by \code{w} in size is created with every value equal to 1.
#' @param fun Function. The function must accept multiple numbers and return a single number. For example \code{mean}, \code{min}, or \code{max}. The function should also accept a \code{na.rm} argument (or ignore it, e.g. as one of the 'dots' arguments). For example, \code{length} will fail, but \code{function(blockVals, ...) { na.omit(length(blockVals)) }} works. The default function is \code{sum}.
#' @param na.rm Logical, if code{FALSE} (default) the value computed by \code{fun} will be \code{NA} only if all cells in the window are \code{NA}. Using \code{na.rm = TRUE} is usually not a good idea since it can unbalance weights.
#' @param NAonly Logical, if \code{TRUE} then only cells with a value of \code{NA} have values replaced. Default is \code{FALSE}.
#' @return Matrix.
#' @keywords internal
workerFocal <- function(i, blockVals, w, fun=sum, na.rm=TRUE, NAonly=FALSE) {

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
