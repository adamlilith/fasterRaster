#' Interpolate values at points to a GRaster using splines
#'
#' @description This function interpolates values in the data table of a "points" `GVector` to a `GRaster` using splines with Tykhonov regularization to avoid overfitting.
#'
#' @param x A "points" `GVector`.
#'
#' @param y A `GRaster`: The output will have the same extent and resolution as this raster.
#'
#' @param field Character or integer or numeric integer: Name or index of the column in `x` with values to interpolate. If `NULL` and if `x` is a 3-dimensional "points" `GVector`, then the interpolation will act on the z-coordinate of each point.
#'
#' @param method Character: The method to use for interpolation can be either `"bilinear"` (default) or `"bicubic"`. Partial matching is used.
#'
#' @param lambda Either `NULL` (default) or numeric value > 0: The Tykhonov regularization parameter. If `NULL`, cross-validation will be used to determine the optimal parameter value. Cross-validation can take quite a while. If you use cross-validation, the output will either be a `GRaster` or a `data.frame`, depending on the value of `interpolate`.
#'
#' @param solver Character: Type of solver to use. Can be either of `"Cholesky"` or `"cg"`. Partial matching is used and case is ignored.
#'
#' @param xlength,ylength Either `NULL` (default), or numeric > 0: Length of the spline step in the x- and y-directions. If `NULL`, these will be set to 4 times the length of the extent in the respective direction.
#'
#' @param interpolate Logical: If `TRUE` (default), then create a `GRaster` with interpolated values. If `FALSE`, return a table with `lambda` values from cross-validation. This argument is ignored if `lambda` is a numeric value.
#'
#' @param verbose Logical: if `TRUE`, display progress.
#'
#' @details If you receive the error, "No data within this subregion. Consider increasing spline step values, try increasing the values of `xlength` and `ylength`.
#'
#' If cross-validation takes too long, or other warnings/errors persist, you can randomly subsample `x` to ~100 points to get an optimum value of `lambda` (using `interpolate = FALSE`), then use this value in the same function again without cross-validation (setting `lambda` equal to this value and `interpolate = TRUE`).
#'
#' @returns Output depends on values of `lambda` and `interpolate`:
#' * `lambda` is `NULL` and `interpolate` is `TRUE`: A `GRaster` with an attribute named `lambdas`. This is a `data.frame` with values of `lambda` that were assessed, plus `mean` (mean residual value) and `rms` (root mean square error). You can see the table using `attr(output_raster, "lambdas", exact = TRUE)`.
#' * `lambda` is `NULL` and `interpolate` is `FALSE`: A `data.frame` with values of `lambdas` that were assessed, plus `mean` (mean residual value) and `rms` (root mean square error). You can see the table using `attr(output_raster, "lambdas", exact = TRUE)`.
#' * `lambda` is a number (`interpolate` is ignored): A `GRaster`.
#'
#' @seealso [interpIDW()], [fillNAs()], **GRASS** module `v.surf.bspline` (see `grassHelp("v.surf.bspline")`)
#'
#' @aliases interpSplines
#' @rdname interpSplines
#' @exportMethod interpSplines
methods::setMethod(
	f = "interpSplines",
	signature = c(x = "GVector", y = "GRaster"),
	function(
		x,
		y,
		field,
		method = "bilinear",
		lambda = NULL,
		solver = "Cholesky",
		xlength = NULL,
		ylength = NULL,
		interpolate = TRUE,
		verbose = is.null(lambda)
	) {

	.interpSplines(
		x = x,
		y = y,
		field = field,
		method = method,
		lambda = lambda,
		solver = solver,
		xlength = xlength,
		ylength = ylength,
		interpolate = interpolate,
		verbose = verbose
	)

	} # EOF
)

# #' @aliases interpSplines
# #' @rdname interpSplines
# #' @exportMethod interpSplines
# methods::setMethod(
# 	f = "interpSplines",
# 	signature = c(x = "GVector", y = "GVector"),
# 	function(
# 		x,
# 		y,
# 		field,
# 		method = "bilinear",
# 		lambda = NULL,
# 		solver = "Cholesky",
# 		xlength = NULL,
# 		ylength = NULL,
#		interpolate = interpolate,
#		verbose = is.null(lambda)
# 	) {

# 	.interpSplines(
# 		x = x,
# 		y = y,
# 		field = field,
# 		method = method,
# 		lambda = lambda,
# 		solver = solver,
# 		xlength = xlength,
# 		ylength = ylength
#		interpolate = interpolate,
#		verbose = verbose
# 	)

# 	} # EOF
# )


#' @noRd
.interpSplines <- function(
		x,
		y,
		field,
		method,
		lambda,
		solver,
		xlength,
		ylength,
		interpolate,
		verbose
) {

	if (!is.null(lambda)) {
		if (lambda <= 0) stop("Argument `lambda` must be NULL or > 0.")
	}

	if (!is.null(lambda)) interpolate <- TRUE

	if (geomtype(x, grass = TRUE) != "point") stop("Argument `x` must be a points GVector.")

	if (inherits(y, "GVector")) {
	
		if (geomtype(y, grass = TRUE) != "point") stop("Argument `y` must be a points GVector or a GRaster.")
	
	}

	method <- omnibus::pmatchSafe(method, c("bilinear", "bicubic"), nmax = 1L)
	solver <- omnibus::pmatchSafe(solver, c("cholesky", "cg"), nmax = 1L)

	compareGeom(x, y)
	.locationRestore(x)
	if (inherits(y, "GRaster")) .region(y)

	# copy values field to x
	if (is.null(field) & is.2d(x)) {
		stop("Argument `field` cannot be NULL if `x` is a 2-dimensional points GVector.")
	} else if (is.numeric(field) | is.integer(field)) {
		field <- names(x)[field]
	}

	if (!is.null(field)) {

		if (anyNA(x@table[[field]])) stop("The column `field` has at least one NA in it. NAs are not permissible.")

		cats <- .vCats(x, db = FALSE)
		db <- data.table::data.table(frid = cats, TEMPTEMP_ = x@table[[field]])
		names(db)[2L] <- field

		.vAttachDatabase(x, table = db, replace = TRUE, cats = cats)

	}

	if (inherits(y, "GVector")) {
		src <- .makeSourceName("v_surf_idw", "vector")
	} else {
		src <- .makeSourceName("v_surf_idw", "raster")
	}

	if (is.null(lambda)) {
		lambda_i <- 0.01
	} else {
		lambda_i <- lambda
	}

	args <- list(
		cmd = "v.surf.bspline",
		input = sources(x),
		column = field,
		method = method,
		solver = solver,
		lambda_i = lambda_i,
		memory = faster("memory"),
		intern = TRUE,
		flags = c(.quiet(), "overwrite")
	)

	if (inherits(y, "GVector")) {
		args$sparse_input <- sources(y)
		args$output <- src
	} else if (inherits(y, "GRaster")) {
		args$raster_output <- src
	}

	if (!is.null(xlength)) args$ew_step = xlength
	if (!is.null(ylength)) args$ns_step = ylength

	if (is.null(lambda)) args$flags <- c(args$flags, "c")

	if (faster("verbose") | verbose) {
		if (is.null(lambda)) {
			omnibus::say("Optimizing lambda...")
		} else {
			omnibus::say("Interpolating...")
		}
	}

	info <- do.call(rgrass::execGRASS, args = args)
	
	# cross-validation for lambda
	if (is.null(lambda)) {

		info <- strsplit(info, split = "\\|")
		info <- info[-1L]
		info <- lapply(info, trimws)
		info <- lapply(info, as.numeric)
		lambdas <- do.call(rbind, info)
		lambdas <- data.table::as.data.table(lambdas)
		names(lambdas) <- c("lambda", "mean", "rms")
		lambdas$optimum <- FALSE
		lambdas$optimum[which.min(lambdas$rms)] <- TRUE

		index <- which.min(lambdas$rms)
		lambda <- lambdas$lambda[index]

		if (interpolate) {

			out <- interpSplines(
				x = x,
				y = y,
				field = field,
				method = method,
				lambda = lambda,
				solver = solver,
				xlength = xlength,
				ylength = ylength,
				verbose = verbose
			)

			attr(out, "lambdas") <- lambdas

		} else {
			out <- lambdas
			if (!faster("useDataTable")) out <- as.data.frame(out)
		}

	} else if (inherits(y, "GVector")) {
		table <- .vAsDataTable(src)
		out <- .makeGVector(src)
	} else if (inherits(y, "GRaster")) {
		out <- .makeGRaster(src, "interpolation")
	}
	out

}
