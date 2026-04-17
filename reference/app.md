# Apply a function to a set of rasters

`app()` applies a function to a set of "stacked" rasters. It is similar
to the
[`terra::app()`](https://rspatial.github.io/terra/reference/app.html)
and
[`terra::lapp()`](https://rspatial.github.io/terra/reference/lapp.html)
functions.

`appFuns()` provides a table of **GRASS** functions that can be used by
`app()` and their equivalents in **R**.

`appCheck()` tests whether a formula supplied to `app()` has any
"forbidden" function calls.

The `app()` function operates in a manner somewhat different from
[`terra::app()`](https://rspatial.github.io/terra/reference/app.html).
The function to be applied *must* be written as a character string. For
example, if the `GRaster` had layer names "`x1`" and "`x2`", then the
function might be like `"= max(sqrt(x1), log(x2))"`. Rasters **cannot**
have the same names as functions used in the formula. In this example,
the rasters could not be named "max", "sqrt", or "log". Note that the
name of a `GRaster` is given by
[`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)–this
can be different from the name of the object in **R**.

The `app()` function will automatically check for `GRaster` names that
appear also to be functions that appear in the formula. However, you can
check a formula before running `app()` by using the `appCheck()`
function. You can obtain a list of `app()` functions using `appFuns()`.
Note that these are sometimes different from how they are applied in
**R**.

Tips:

- Make sure your `GRaster`s have
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md).
  The function matches on these, not the name of the variable you use in
  **R** for the `GRaster`.

- Use `null()` instead of `NA`, and use `isnull()` instead of
  [`is.na()`](https://github.com/adamlilith/fasterRaster/reference/math.md).

- If you want to calculate values using while ignoring `NA` (or `null`)
  values, see the functions that begin with `n` (like `nmean`).

- Be mindful of the data type that a function returns. In **GRASS**,
  these are `CELL` (integer), `FCELL` (floating point values–precise to
  about the 7th decimal place), and `DCELL` (double-floating point
  values–precise to about the 15th decimal place; commensurate with the
  **R** `numeric` type). In cases where you want a `GRaster` to be
  treated like a float or double type raster, wrap the name of the
  `GRaster` in the `float()` or
  [`double()`](https://rdrr.io/r/base/double.html) functions. This is
  especially useful if the `GRaster` might be assumed to be the `CELL`
  type because it only contains integer values. You can get the data
  type of a raster using
  [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md)
  with the `type` argument set to `GRASS`. You can change the data type
  of a `GRaster` using
  [`as.int()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
  [`as.float()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
  and
  [`as.doub()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md).
  Note that categorical rasters are really `CELL` (integer) rasters with
  an associated "levels" table. You can also change a `CELL` raster to a
  `FCELL` raster by adding then subtracting a decimal value, as in
  `x - 0.1 + 0.1`. See
  [`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md).

- The `rand()` function returns integer values by default. If you want
  non-integer values, use the tricks mentioned above to datatype
  non-integer values. For example, if you want uniform random values in
  the range between 0 and 1, use something like
  `= float(rand(0 + 0.1, 1 + 0.1) - 0.1)`.

- `GRaster`s with short names can cause the function to mis-specify the
  formula in **GRASS**. This happens when, for example, the `fun`
  argument contains an "if" statement, and the raster name is "i" or
  "f", which would incorrectly replace those letters in the "if" with
  the **GRASS** name of the raster. To avoid this, avoid using short
  raster names that could be part of longer operators using by `app()`
  (see `appFuns()` for a list of these operators).

## Usage

``` r
# S4 method for class 'GRaster'
app(x, fun, datatype = "auto", seed = NULL)

appFuns(warn = TRUE)

# S4 method for class 'GRaster,character'
appCheck(x, fun, msgOnGood = TRUE, failOnBad = TRUE)
```

## Arguments

- x:

  A `GRaster` with one or more named layers.

- fun:

  Character: The function to apply. This must be written as a character
  string that follows these rules:

  - It must use typical arithmetic operators like `+`, `-`, `*`, `/`
    and/or functions that can be seen using `appFuns(TRUE)`.

  - The
    [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)
    of the rasters do not match any of the functions in the
    `appFuns(TRUE)` table. Note that `x` and `y` are forbidden names :(

  The help page for **GRASS** tool `r.mapcalc` will be especially
  helpful. You can see this page using `grassHelp("r.mapcalc")`.

- datatype:

  Character: This ensures that rasters are treated as a certain type
  before they are operated on. This is useful when using rasters that
  have all integer values, which **GRASS** can assume represent
  integers, even if they are not supposed to. In this case, the output
  of operations on this raster might be an integer if otherwise not
  corrected. Partial matching is used, and options include:

  - `"integer"`: Force all rasters to integers by truncating their
    values. The output may still be of type `float` if the operation
    creates non-integer values.

  - `"float"`: Force rasters to be considered floating-point values.

  - `"double"`: Force rasters to be considered double-floating point
    values.

  - `"auto"` (default): Ensure that rasters are represented by their
    native
    [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md)
    (i.e., "CELL" rasters as integers, "FCELL" rasters as
    floating-point, and "DCELL" as double-floating point).

- seed:

  Numeric integer vector or `NULL` (default): A number for the random
  seed. Used only for `app()` function `rand()`, that generates a random
  number. If `NULL`, a seed will be generated. Defining the seed is
  useful for replicating a raster made with `rand()`. This must be an
  integer!

- warn:

  Logical (function `appFuns()`): If `TRUE` (default), display a warning
  when `allFuns()` is not called interactively.

- msgOnGood:

  Logical (function `appCheck()`): If `TRUE` (default), display a
  message if no overt problems with the raster names and formula are
  detected.

- failOnBad:

  Logical (function `appCheck()`): If `TRUE` (default), fail if overt
  problems with raster names and the formula are detected.

## Value

A `GRaster`.

## See also

[`terra::app()`](https://rspatial.github.io/terra/reference/app.html),
[`terra::lapp()`](https://rspatial.github.io/terra/reference/lapp.html),
[`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md),
[`classify()`](https://github.com/adamlilith/fasterRaster/reference/classify.md),
and especially the **GRASS** manual page for tool `r.mapcalc` (see
`grassHelp("r.mapcalc")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Elevation raster
madElev <- fastData("madElev")

# Convert SpatRaster to a GRaster:
elev <- fast(madElev)

# Create a "stack" of rasters for us to operate on:
x <- c(elev, elev^2, sqrt(elev))

# Demonstrate check for badly-named rasters:
names(x) <- c("cos", "asin", "exp")
fun <- "= cos / asin + exp"
appCheck(x, fun, failOnBad = FALSE)

# Rename rasters acceptable names and run the function:
names(x) <- c("x1", "x2", "x3")
fun <- "= (x1 / x2) + x3"
appCheck(x, fun, failOnBad = FALSE)
app(x, fun = fun)

# This is the same as:
(x[[1]] / x[[2]]) + x[[3]]

# We can view a table of app() functions using appFuns():
appFuns()

# We can also get the same table using:
data(appFunsTable)

# Apply other functions:
fun <- "= median(x1 / x2, x3, x1 * 2, cos(x2))"
app(x, fun = fun)

fun <- "= round(x1) * tan(x2) + log(x3, 10)"
app(x, fun = fun)

# Demonstrate effects of data type:
fun <- "= x1 + x3"
app(x, fun = fun, datatype = "float") # output is floating-point
app(x, fun = fun, datatype = "integer") # output is integer

# Some functions override the "datatype" argument:
fun <- "= sin(x2)"
app(x, fun = fun, datatype = "integer")

# Make a raster with random values [1:4], with equal probability of each:
fun <- "= round(rand(0.5, 4.5))"
rand <- app(elev, fun = fun)
rand

freqs <- freq(rand) # cell frequencies
print(freqs)

}
```
