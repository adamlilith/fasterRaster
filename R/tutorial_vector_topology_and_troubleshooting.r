#' @name tutorial_vector_topology_and_troubleshooting
#'
#' @title Vector topology and troubleshooting creation of GVectors
#'
#' @description **GRASS** uses a "topological" model for vectors. Topologically correct vectors have several properties, which mainly apply to polygon (**GRASS**: "area") vectors:
#' * Boundaries of polygons do not cross (lines can cross one another, though).
#' * Areas are completely closed (their boundaries form loops).
#' * Shared boundaries of polygons are only represented once in the vector.
#'
#' There are other properties involving combinations of lines and polygons in the same vector, but **fasterRaster** does not allow shapes of different types in the same vector.
#' 
#' By default, the [fast()] function corrects vector topology when creating `GVector`s. Polygon vectors created in other formats may not be topologically correct. For example, the borders of adjacent polygons may cross one another, or there may be small gaps between them. As a result, [fast()] may throw an error when trying to convert them to a `GVector` because it will convert the overlapping parts of polygons into their own polygons, which cannot be matched to a single row in a data table (because the intersection technically belongs to both polygons).
#'
#' ## Should I use topology correction?
#'
#' It depends on what you want to represent with your vector. If the vector is intended to represent polygons that *should* overlap, such as buffers around points, then turning it off is OK. However, if polygons should not exist (e.g., boundaries of adjacent states should not intrude into another state), then it should probably be on. In this case, though, you will also probably want to use the `snap` and `area` arguments in [fast()] to correct the boundaries.
#'
#' ## What are the consequences of turning topology correction on or off?
#'
#' In general, `GVectors` created with or without topology correction should work the same, with two exceptions:
#' 1. A polygons `GVector` created with topology correction might have extra polygons, so will "behave" differently from one that was not topologically corrected.
#' 2. Polygon `GVector`s created *without* topological correction may not subset correctly or at all when using the single-bracket operator, \code{\link[fasterRaster]{[}} (e.g., `vector[23]`).
#'
#' ## Turning topology correction on or off
#'
#' You can control topology correction in two ways:
#' * First, you can turn topology correction on or off globally using the `correct` setting in [faster()]. For example, `faster(correct = TRUE)` turns topology correction on, and `faster(correct = FALSE)` turns it off whenever you use [fast()]. By default, the `correct` option is set to `TRUE` (topology is corrected).
#' * Second, you turn override the global setting and turn topology correction on or off on a case-by-case basis when using the [fast()] function by setting the `correct` argument.
#'
#' ## Correcting topology
#'
#' There are several ways to correct topology. In general order of preference, these are:
#'
#' 1. Automatic basic correction while creating a `GVector` using [fast()]: You can set the `correct` argument in [fast()] to `TRUE` to correct issues on a case-by-case basis, or set the `correct` option in [faster()] to `TRUE` to correct issues every time you use [fast()].
#'
#' 2. Manual correction while creating a `GVector` [fast()]: Regardless of the value of `correct`, you can also use the `snap` and/or `area` arguments in [fast()] to correct topological issues by snapping nearby vertices to one another and/or removing polygons with areas that are smaller than some threshold. The units of `snap` are in map units (usually meters, but degrees for unprojected coordinate reference systems), and meters-squared for `area`.
#'
#' 3. Automatic correction while creating a `GVector` [fast()]: To implement automatic correction, set `snap` and/or `area` to `NULL`, and also define the `iter` argument to an integer >1. When using automatic correction, `fast()` will first try to load the vector without snapping or area removal. If this does not work, it will then use an automated procedure to increase the value(s) or `snap` and/or `area`, load the vector, and repeat, until either the vector is loaded or `iter` attempts have been made.
#'
#' 4. After creating a `GVector`: You can apply [various tools][breakPolys] to snap, remove areas, delete dangles and too-small angles, and do several other tasks related to cleaning vectors, plus [fillHoles()].
#'
#' 5. Most failures to create a `GVector` arise because the number of geometries does not match the number of rows in an associated data table. If you do not need the data table, you can drop it using the `dropTable` argument, as in `fast(..., dropTable = TRUE)`.
#'
#' 6. You can try to fix the vector outside of **fasterRaster**. See the [terra::is.valid()] and [terra::makeValid()] functions in **terra**.
#'
#' Although not really a cleaning tool, you can also use [simplifyGeom()] or [smoothGeom()] to reduce the complexity of vector geometries.
#' 
#' @keywords tutorial
#' @example man/examples/ex_GRaster_GVector.r
NULL
