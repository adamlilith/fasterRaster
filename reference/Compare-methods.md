# Compare-methods operations on GRasters and GRegions

You can do comparative operations on `GRaster`s using normal operators
in **R**: `<`, `<=`, `==`, `!=`, `>=`, and `>`. You can also use
[`%in%`](https://github.com/adamlilith/fasterRaster/reference/match.md)
for categorical `GRasters` (see
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)).

You can also compare two `GRegion`s using the `==` and `!=` operators.
Most users of **fasterRaster** will not have to work much with "regions"
(see
[`vignette("regions", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/regions.md)),
so can ignore this functionality. `GRegion`s are the same if they have
the same coordinate reference system, location/project and mapset (see
[`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)),
topology (2D or 3D), extent, and resolution. If both are 3D, then they
must also have the same vertical extent and number of depths.

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
Compare(e1, e2)

# S4 method for class 'logical,GRaster'
Compare(e1, e2)

# S4 method for class 'GRaster,logical'
Compare(e1, e2)

# S4 method for class 'numeric,GRaster'
Compare(e1, e2)

# S4 method for class 'GRaster,numeric'
Compare(e1, e2)

# S4 method for class 'GRaster,integer'
Compare(e1, e2)

# S4 method for class 'integer,GRaster'
Compare(e1, e2)

# S4 method for class 'GRaster,character'
Compare(e1, e2)

# S4 method for class 'character,GRaster'
Compare(e1, e2)

# S4 method for class 'GRegion,GRegion'
Compare(e1, e2)
```

## Arguments

- e1, e2:

  Values depend on the type of comparison:

  - Comparing `GRaster`s to logical, numeric, character values: `e1` and
    `e2` can be any one of these. Comparison to a character string can
    be useful when using a categorical raster, in which case you can use
    something like `raster1 == "Wetlands"` to coerce all "wetland" cells
    to be 1 (TRUE) and all others 0 (FALSE) or `NA` (if it was
    originally `NA`).

  - Comparing a `GRegion` to another `GRegion`: `e1` and `e2` must be
    `GRegion`s!

## Value

Comparing `GRaster`s: An "integer" `GRaster` with values of 0 (FALSE), 1
(TRUE), or `NA` (neither).

Comparing `GRegion`s: Output is logical.

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c("elev1", "elev2", "log_elev", "sqrt_elev")

elev
elevs

# Comparisons
elev < 100
elev <= 100
elev == 100
elev != 100
elev > 100
elev >= 100

elev + 100 < 2 * elev

elevs > 10
10 > elevs

# logic
elev < 10 | elev > 200
elev < 10 | cos(elev) > 0.9

elev < 10 | TRUE
TRUE | elev > 200

elev < 10 | FALSE
FALSE | elev > 200

elev < 10 & cos(elev) > 0.9

elev < 10 & TRUE
TRUE & elev > 200

elev < 10 & FALSE
FALSE & elev > 200

}
```
