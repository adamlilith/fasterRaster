# Multivariate environmental similarity surface (MESS)

The multivariate environmental similarity surface (MESS) indicates the
degree to which a set of "projection" environmental conditions fall
inside or outside a set of "reference" conditions. Values of 1 indicate
a location falls at the exact median of all variables. Values of 0
indicate that the location has at least one environmental covariate that
is at the upper or lower end of the range of reference conditions, and
values \<1 indicate that at least one variable falls above or below the
reference conditions. MESS can be used, for example, to indicate the
degree to which a model constructed in one time period and/or location
must extrapolate when projected to another time period and/or location.

## Usage

``` r
# S4 method for class 'GRaster,GRaster'
multivarEnvSim(ref, proj)

# S4 method for class 'GRaster,missing'
multivarEnvSim(ref, proj)

# S4 method for class 'data.frame,GRaster'
multivarEnvSim(ref, proj, na.rm = FALSE)

# S4 method for class 'data.table,GRaster'
multivarEnvSim(ref, proj, na.rm = FALSE)

# S4 method for class 'matrix,GRaster'
multivarEnvSim(ref, proj, na.rm = FALSE)
```

## Arguments

- ref:

  A `data.frame`, `data.table`, a points `GVector`, or "stack" of
  `GRasters`: This represents the set of "reference" environmental
  conditions:

  - `data.frame` or `data.table`: There must be one column per layer in
    `proj`, and the columns must have the same names as the layers in
    `proj`.

  - `GRaster` with one or more layers: Must have the same
    [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)
    as the `GRaster`s in `proj`. Values are assumed to be continuous
    (not categorical/factors).

- proj:

  A `GRaster` or missing. If a `GRaster`, it must have the same layers
  as can have with one or more layers as `ref`. Values are assumed to be
  continuous (not categorical/factors). If missing, then `ref` is used,
  in which case the output represents the relative difference of each
  cell from the overall layer median.

- na.rm:

  Logical: If `FALSE` (default), and `ref` is a `data.frame`,
  `data.table`, or `matrix`, and has `NA`s in it, then the function will
  likely fail.

## Value

A `GRaster` "stack". There will be one layer per layer in `ref`,
indicating the MESS score for that variable. There will also be a layer
named "MESS" which represents the MESS value across all variables (the
minimum value of each of the individual MESS rasters). A final layer
represents the layer which is most different (has the lowest MESS
value).

## References

Elith, J, Kearney, M, and Phillips, S. 2010. The art of modelling
range-shifting species. *Methods in Ecology and Evolution* 1:330-342.
[doi:10.1111/j.2041-210X.2010.00036.x](https://doi.org/10.1111/j.2041-210X.2010.00036.x)
(see especially the Supplement)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Climatic conditions with 4 variables
madChelsa <- fastData("madChelsa")
chelsa <- fast(madChelsa)

# Simulate new conditions by multiplying values by (1 + small number)
proj <- 1.05 * chelsa
names(proj) <- names(chelsa) # proj must have same names as ref

messes <- multivarEnvSim(ref = chelsa, proj = proj)
plot(messes)

# Where is at least one variable outside the reference range?
extrap <- messes[["MESS"]] < 0
levs <- data.frame(value = 0:1, labels = c('no extrapolation', 'extrapolation'))
levels(extrap) <- levs
plot(extrap)

# Using a data frame as "reference" conditions:
madDypsis <- fastData("madDypsis") # Dypsis occurrences
dypsis <- fast(madDypsis)

dypEnv <- extract(chelsa, dypsis)
dypMess <- multivarEnvSim(ref = dypEnv, proj = proj)
plot(dypMess)

}
```
