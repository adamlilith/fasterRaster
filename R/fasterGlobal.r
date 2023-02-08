#' Statistics summarizing all values of a raster
#'
#' Displays statistics summarizing all values of a.
#'
#' @param rast The name(s) of one or more rasters in the active \code{GRASS} session.
#' @param fun The function used to summarize values. This can be any of:
#' \itemize{
#'	\item \code{'cv'}: Coefficient of variation (expressed as a proportion).
#'	\item \code{'meanAbs'}: Mean of absolute values.
#'	\item \code{'mean'} (default): Average.
#'	\item \code{'minimum'} and \code{'maximum'}: Lowest and highest value across non-\code{NA} cells.
#'	\item \code{'nonNA'}: Total number of non-\code{NA} cells.
#'	\item \code{'NA'}: Total number of \code{NA} cells.
#'	\item \code{'range'}: Range.
#'	\item \code{'sd'}: Sample standard deviation.
#'	\item \code{'sum'}: Sum.
#'	\item \code{'var'}: Sample variance.
#' }
#' @param terra If \code{FALSE} (default), the output is a numeric vector, one value per raster. If \code{TRUE}, then the output is a \code{data.frame} with one row and one column (like \pkg{terra}'s \code{\link[terra]{global}} when applied to a single raster).
#'
#' @return A numeric vector or a \code{data.frame}.
#'
#' @seealso \code{\link{fasterInfo}} in \pkg{fasterRaster}; \code{\link[terra]{global}} in package \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.univar.html}{\code{r.univar}}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterGlobal <- function(
	rast,
	fun = 'mean',
	terra = FALSE
) {

	if (length(fun) != 1L) stop('Only one value of argument "fun" can be used at a time.')

	out <- numeric()
	fun <- tolower(fun)
	
	rasts <- fasterLs(rastOrVect = 'raster', temps=TRUE)

	for (thisRast in rast) {

		if (!(thisRast %in% rasts)) {
			this <- NA
			warning(paste('Raster', thisRast, 'not found.'))
		} else {

			suppressMessages(
				info <- rgrass::execGRASS(
					'r.univar',
					flags = c('r'),
					map = thisRast,
					intern = TRUE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE
				)
			)

			pattern <- if (fun == 'meanabs') {
				'mean of absolute values: '
			} else if (fun == 'mean') {
				'mean: '
			} else if (fun == 'min') {
				'minimum: '
			} else if (fun == 'max') {
				'maximum: '
			} else if (fun == 'na') {
				'total null cells: '
			} else if (fun == 'range') {
				'range: '
			} else if (fun == 'sd') {
				'standard deviation: '
			} else if (fun == 'sum') {
				'sum: '
			} else if (fun == 'var') {
				'variance: '
			} else { 
				NA
			}
			
			if (!is.na(pattern)) {
			
				this <- info[grepl(info, pattern=pattern)]
				this <- sub(this, pattern=pattern, replacement='')
				this <- as.numeric(this)
			
			} else {
			
				if (fun == 'nonna') {
				
					pattern <- 'total null and non-null cells: '
					this1 <- info[grepl(info, pattern=pattern)]
					this1 <- sub(this1, pattern=pattern, replacement='')
					this1 <- as.numeric(this1)
					
					pattern <- 'total null cells: '
					this2 <- info[grepl(info, pattern=pattern)]
					this2 <- sub(this2, pattern=pattern, replacement='')
					this2 <- as.numeric(this2)
					
					this <- this1 - this2
					
				} else if (fun == 'cv') {
				
					pattern <- 'variation coefficient: '
					this <- info[grepl(info, pattern=pattern)]
					this <- sub(this, pattern=pattern, replacement='')
					this <- sub(this, pattern='%', replacement='')
					this <- as.numeric(this)
					this <- this / 100

				}
			}
			
		} # if raster exists in the GRASS session
			
		names(this) <- thisRast
		out <- c(out, this)
		
	} # next raster
		
	if (terra) {
		out <- data.frame(x = out, row.names = rast)
		names(out) <- fun
	}
		
	out

}
