# Landscape fragmentation class following Riitters et al. (2020)

Riitters et al. (2020) propose a classification scheme for forest
fragmentation (which can be applied to any habitat type). The scheme
relies on calculating density (e.g., number of forested cells in a
window around a focal cell) and connectivity (number of cases where
neighboring cells are both forested). This function calculates these
classes from a `GRaster` or `SpatRaster` in which the focal habitat type
has cell values of 1, and non-focal habitat type has cell values of 0 or
`NA`.

Note that by default, the `SpatRaster` and `GRaster` versions will
create different results around the border of the raster. The
`SpatRaster` version uses the
[`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html)
function, which will not return an `NA` value when its window overlaps
the raster border if the `na.rm` argument is `TRUE`. However, the
`GRaster` version uses the **GRASS** tool `r.neighbors`, which does
return `NA` values in these cases.

The fragmentation classes are:

- Value provided by `none`: None (i.e., no forest; default is `NA`).

- 1: Patch

- 2: Transitional

- 3: Perforated

- 4: Edge

- 5: Undetermined (not possible to obtain when `w = 3`)

- 6: Interior

## Usage

``` r
# S4 method for class 'SpatRaster'
fragmentation(
  x,
  w = 3,
  undet = "undetermined",
  none = NA,
  na.rm = TRUE,
  cores = faster("cores"),
  verbose = TRUE
)

# S4 method for class 'GRaster'
fragmentation(x, w = 3, undet = "undetermined", none = NA, verbose = TRUE)
```

## Arguments

- x:

  A `SpatRaster` or `GRaster` wherein the habitat type has cell values
  of 1, and non-focal habitat type has cell values of 0 or `NA`

- w:

  An odd integer between 3 and 25: Size of the window across which
  fragmentation is calculated (in units of "rows" and "columns"). The
  default is 3, meaning the function uses a 3x3 moving window to
  calculate fragmentation. For large rasters, compute time is
  ~*O*(`N`) + *O*(`N * w^2`), where `N` is the number of cells in the
  raster. So, even a small increase in `w` can increase compute time by
  a lot. Values \>25 will fail. A workaround might be to use
  [`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md)
  with `fun = 'mode'`, apply `fragmentation()`, then
  [`resample()`](https://github.com/adamlilith/fasterRaster/reference/resample.md)
  to the original resolution.

- undet:

  Character: How to assign the "undetermined" case. Valid values are
  `"perforated"` (default), `"edge"`, and `"undetermined"`. Partial
  matching is used. If `Pf` is the proportional density raster cell
  value and `Pff` the proportional connectivity raster cell value, the
  undetermined case occurs when `Pf` \> 0.6 and `Pf == Pff`.

- none:

  Integer or `NA` (default): Value to assign to a cell with no focal
  habitat. Riitters et al. use `NA`. This will be forced to an integer
  if it is not an actual integer.

- na.rm:

  Logical: If `TRUE` (default) and `x` is a `SpatRaster`, then cells
  near the edge of the raster where the window overlaps the edge can
  still be assigned a fragmentation class. If `FALSE`, these cells will
  be assigned a value of `none`.

- cores:

  Integer: Number of processor cores to use for when processing a
  `SpatRaster`.

- verbose:

  Logical: If `TRUE` (default), display progress.

## Value

A categorical `SpatRaster` or `GRaster`. The values assigned to each
class can be seen with
[`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md).

## References

Riitters, K., J. Wickham, R. O'Neill, B. Jones, and E. Smith. 2000.
Global-scale patterns of forest fragmentation. *Conservation Ecology*
4:3. URL: <http://www.consecol.org/vol4/iss2/art3/>. Also note the
[errata](https://www.ecologyandsociety.org/vol4/iss2/art3/errata/january26.2001.html).

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data:
madForest <- fastData("madForest2000") # raster

### Fragmentation classes from a SpatRaster

fragTerra <- fragmentation(madForest)
plot(fragTerra)
levels(fragTerra)
freq(fragTerra)

### Fragmentation classes from a GRaster

# Convert to GRaster:
forest <- fast(madForest)

# Fragmentation class:
frag <- fragmentation(forest)
plot(frag)
levels(frag)
freq(frag)

}
```
