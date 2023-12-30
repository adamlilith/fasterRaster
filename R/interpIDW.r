#' Interpolate values at points to a GRaster
#'
#' @description This function interpolates values from a set of points to a raster using inverse distance weighting (IDW).
#'
#' @param x A "points" `GVector`.
#'
#' @param y A `GRaster` to serve as a template for interpolation: Only points in `x` that fall inside the extent of the raster will be used for interpolation. You can increase the extent of a `GRaster` using [extend()].
#'
#' @param field Character or integer or numeric integer: Name or index of the column in `x` with values to interpolate. If `NULL` and if `x` is a 3-dimensional "points" `GVector`, then the interpolation will act on the z-coordinate of each point.
#'
#' @param nPoints Integer or numeric integer: Number of nearest points to use for interpolation. The default is to use all points (`Inf`).
#'
#' @param power Numeric value > 0: Power to which to take distance when interpolating. The default value is two, so the value of each point used for interpolation is \eqn{1 / d^2} where *d* is distance.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::interpIDW()], [interpSplines()], [fillNAs()], module (`v.surf.idw`)[https://grass.osgeo.org/grass84/manuals/v.surf.idw.html] in **GRASS**
#'
#' @aliases interpIDW
#' @rdname interpIDW
#' @exportMethod interpIDW
methods::setMethod(
	f = "interpIDW",
	signature = c(x = "GVector", y = "GRaster"),
	function(x, y, field, nPoints = Inf, power = 2) {

	if (geomtype(x, grass = TRUE) != "point") stop("Argument ", sQuote("x"), " must be a points GVector.")

	compareGeom(x, y)
	.locationRestore(x)
	.region(y)

	if (is.infinite(nPoints)) nPoints <- nsubgeom(x)

	# copy values field to x
	if (is.null(field) & is.2d(x)) {
		stop("Argument ", sQuote("field"), " cannot be NULL if ", sQuote("x"), " is a 2-dimensional points GVector.")
	} else if (is.numeric(field) | is.integer(field)) {
		field <- names(x)[field]
	}

	if (!is.null(field)) {

		if (anyNA(x@table[[field]])) stop("The column ", sQuote(field), " has at least one NA in it. NAs are not permissible.")

		cats <- .vCats(x, db = FALSE)

		db <- data.table::data.table(frid = cats, TEMPTEMP_ = x@table[[field]])

		names(db)[2L] <- field

		.vAttachDatabase(x, table = db, replace = TRUE)

	}

	src <- .makeSourceName("v_surf_idw", "raster")

	rgrass::execGRASS(
		cmd = "v.surf.idw",
		input = sources(x),
		output = src,
		npoints = nPoints,
		column = field,
		power = power,
		flags = c(.quiet(), "overwrite")
	)
	.makeGRaster(src, "interpolation")


	} # EOF
)
