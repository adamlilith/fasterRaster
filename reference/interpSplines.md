# Interpolate values at points to a GRaster using splines

This function interpolates values in the data table of a "points"
`GVector` to a `GRaster` using splines with Tykhonov regularization to
avoid overfitting.

## Usage

``` r
# S4 method for class 'GVector,GRaster'
interpSplines(
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
)
```

## Arguments

- x:

  A "points" `GVector`.

- y:

  A `GRaster`: The output will have the same extent and resolution as
  this raster.

- field:

  Character or integer or numeric integer: Name or index of the column
  in `x` with values to interpolate. If `NULL` and if `x` is a
  3-dimensional "points" `GVector`, then the interpolation will act on
  the z-coordinate of each point.

- method:

  Character: The method to use for interpolation can be either
  `"bilinear"` (default) or `"bicubic"`. Partial matching is used.

- lambda:

  Either `NULL` (default) or numeric value \> 0: The Tykhonov
  regularization parameter. If `NULL`, cross-validation will be used to
  determine the optimal parameter value. Cross-validation can take quite
  a while. If you use cross-validation, the output will either be a
  `GRaster` or a `data.frame`, depending on the value of `interpolate`.

- solver:

  Character: Type of solver to use. Can be either of `"Cholesky"` or
  `"cg"`. Partial matching is used and case is ignored.

- xlength, ylength:

  Either `NULL` (default), or numeric \> 0: Length of the spline step in
  the x- and y-directions. If `NULL`, these will be set to 4 times the
  length of the extent in the respective direction.

- interpolate:

  Logical: If `TRUE` (default), then create a `GRaster` with
  interpolated values. If `FALSE`, return a table with `lambda` values
  from cross-validation. This argument is ignored if `lambda` is a
  numeric value.

- verbose:

  Logical: if `TRUE`, display progress.

## Value

Output depends on values of `lambda` and `interpolate`:

- `lambda` is `NULL` and `interpolate` is `TRUE`: A `GRaster` with an
  attribute named `lambdas`. This is a `data.frame` with values of
  `lambda` that were assessed, plus `mean` (mean residual value) and
  `rms` (root mean square error). You can see the table using
  `attr(output_raster, "lambdas", exact = TRUE)`.

- `lambda` is `NULL` and `interpolate` is `FALSE`: A `data.frame` with
  values of `lambdas` that were assessed, plus `mean` (mean residual
  value) and `rms` (root mean square error). You can see the table using
  `attr(output_raster, "lambdas", exact = TRUE)`.

- `lambda` is a number (`interpolate` is ignored): A `GRaster`.

## Details

If you receive the error, "No data within this subregion. Consider
increasing spline step values, try increasing the values of `xlength`
and `ylength`.

If cross-validation takes too long, or other warnings/errors persist,
you can randomly subsample `x` to ~100 points to get an optimum value of
`lambda` (using `interpolate = FALSE`), then use this value in the same
function again without cross-validation (setting `lambda` equal to this
value and `interpolate = TRUE`).

## See also

[`interpIDW()`](https://github.com/adamlilith/fasterRaster/reference/interpIDW.md),
[`fillNAs()`](https://github.com/adamlilith/fasterRaster/reference/fillNAs.md),
**GRASS** tool `v.surf.bspline` (see `grassHelp("v.surf.bspline")`)
