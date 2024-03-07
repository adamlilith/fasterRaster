#' @name tutorial_vector_topology
#'
#' @title Vector topology
#'
#' @description **GRASS** uses a "topological" model for vectors. Topologically correct vectors have several properties, which mainly apply to polygon (**GRASS**: "area") vectors:
#' * Boundaries of polygons do not cross (lines can cross one another, though).
#' * Shared boundaries of polygons are only represented once in the vector.
#'
#' There are other properties involving combinations of lines and polygons in the same vector, but **fasterRaster** does not allow shapes of different types in the same vector.
#' 
#' By default **GRASS** (not **fasterRaster**) corrects vector topology when loading vectors using the `v.in.ogr` module. Owing to the stipulation that shared boundaries are represented only once, polygons read into **GRASS** polygons created in other software are often topologically incorrect. For example, the borders of adjacent polygons may cross one another, or there may be small gaps between them. The `v.in.ogr` module will try to correct these errors by creating "slivers" where two adjacent polygons slightly overlap, or (optionally), by snapping vertices that are close to one another together. Topologically incorrect lines can be corrected by creating "dangles" in places where line terminuses do not match exactly.
#'
#' By default, **fasterRaster** will *not* correct topological errors when creating a `GVector` with [fast()]. This is a design decision based on the fact that [terra::vect()] does not correct topology, and to the degree possible, **fasterRaster** tries to emulate **terra**. Not correcting topology also has the advantage of loading vectors faster. It also obviates issues matching a vector's data table to each feature. For example, if two polygons overlap slightly, correcting the topology could create a sliver that represents the area of overlap. It is unclear which row of a data table this sliver should match.
#'
#' ## Turning topology correction on or off
#'
#' You can control topology correction in two ways:
#' * First, you can turn topology correction on or off for all uses of [fast()] using the `correct` setting in [faster()]. For example, `faster(correct = TRUE)` turns topology correction on, and `faster(correct = FALSE)` turns it off. By default, the `correct` option is set to `FALSE` (no topology correction).
#' * Second, you turn topology correction on or off on a case-by-case basis using the `correct` argument when using the [fast()] function. This overrides the option set by `faster()`.
#'
#' ## Correcting topology
#'
#' 1. Automatic basic correction while creating a `GVector`: You can set the `correct` argument in [fast()] to `TRUE` to correct issues on a case-by-case basis, or set the `correct` option in [faster()] to `TRUE` to correct issues every time you use [fast()].
#'
#' 2a. Manual correction while creating a `GVector`: Regardless of the value of `correct`, you can also use the `snap` and/or `area` arguments in [fast()] to correct topological issues by snapping nearby vertices to one another and/or removing polygons with areas that are smaller than some threshold.
#'
#' 2b. Automatic advanced correction while creating a `GVector`: You can also attempt automatic snapping and/or area removal. To do this, set `snap` and/or `area` to `NULL`, and also define the `iter` argument to an integer >1. When using automatic correction, `fast()` will first try to load the vector without snapping or area removal. If this does not work, it will then use an automated procedure to increase the value(s) or `snap` and/or `area`, load the vector, and repeat, until either the vector is loaded or `iter` attempts have been made.
#'
#' 3. After creating a `GVector`: You can apply the [various tools][breakPolys] to can snap, remove areas, delete dangles and too-small angles, and do several other tasks related to cleaning vectors, plus [fillHoles()].
#'
#' Although not really a cleaning tool, you can use [simplifyGeom()] or [smoothGeom()] to reduce the complexity of vector geometries.
#' 
#' @keywords tutorial
#' @example man/examples/ex_GRaster_GVector.r
NULL
