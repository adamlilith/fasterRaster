# Start the GRASS GUI (potentially dangerous!)

This function starts the **GRASS** GUI. It is provided merely as a
utility... in most cases, it should *not* be used if you are doing any
kind of analysis of rasters or vectors using **fasterRaster**. The
reason for this prohibition is that **fasterRaster** objects, like
`GRaster`s and `GVector`s, are really "pointers" to objects in
**GRASS**. If **fasterRaster** points to a **GRASS** object that is
changed in **GRASS** but not **R**, then **fasterRaster** will not
"know" about it, so changed won't be reflected in the **fasterRaster**
object.

One aspect of the GUI that is useful but will not change objects is to
use it to plot rasters and vectors. However, the a **fasterRaster**
object in **R** will have a different name in **GRASS**. The name in
**GRASS** of a `GVector` or `GRaster` is given by
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md).

## Usage

``` r
# S4 method for class 'missing'
grassGUI()
```

## Value

Nothing (starts the **GRASS** GUI).

## See also

[`mow()`](https://github.com/adamlilith/fasterRaster/reference/mow.md)

## Examples

``` r
if (grassStarted()) {

# DANGER: Making changes to rasters/vectors in the GUI can "break" them in R.
if (interactive()) grassGUI()

}
```
