# Arithmetic operations on GRasters

**`GRaster`s**: You can do arithmetic operations on `GRaster`s and using
normal operators in **R**: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), and
`%/%` (integer division).

**`GVector`s**: You can also do arithmetic operations on `GVector`s:  
  
`+` operator: Same as
[`union()`](https://github.com/adamlilith/fasterRaster/reference/union.md)  
`-` operator: Same as
[`erase()`](https://github.com/adamlilith/fasterRaster/reference/erase.md)  
`*` operator: Same as
[`intersect()`](https://github.com/adamlilith/fasterRaster/reference/intersect.md)  
`/` operator: Same as
[`xor()`](https://github.com/adamlilith/fasterRaster/reference/xor.md)  

## Usage

``` r
# S4 method for class 'GRaster,logical'
Arith(e1, e2)

# S4 method for class 'logical,GRaster'
Arith(e1, e2)

# S4 method for class 'GRaster,numeric'
Arith(e1, e2)

# S4 method for class 'GRaster,integer'
Arith(e1, e2)

# S4 method for class 'numeric,GRaster'
Arith(e1, e2)

# S4 method for class 'integer,GRaster'
Arith(e1, e2)

# S4 method for class 'GRaster,GRaster'
Arith(e1, e2)

# S4 method for class 'GVector,GVector'
Arith(e1, e2)
```

## Arguments

- e1, e2:

  `GRaster`s, `GVector`s, `numeric`s, `integer`s, or `logical`s.

## Value

A `GRaster`.

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c("elev1", "elev2", "log_elev", "sqrt_elev")

elev
elevs

# do some math
elev + 100
elev - 100
elev * 100
elev / 100
elev ^ 2
elev %/% 100 # divide then round down
elev %% 100 # modulus

100 + elev
100 %/% elev
100 %% elev

elevs + 100
100 + elevs

# math with logicals
elev + TRUE
elev - TRUE
elev * TRUE
elev / TRUE
elev ^ TRUE
elev %/% TRUE # divide then round down
elev %% TRUE # modulus

elevs + TRUE
TRUE + elevs

# Raster interacting with raster(s):
elev + elev
elev - elev
elev * elev
elev / elev
elev ^ log(elev)
elev %/% sqrt(elev) # divide then round down
elev %% sqrt(elev) # modulus

elevs + elev
elev * elevs

# sign
abs(-1 * elev)
abs(elevs)

# powers
sqrt(elevs)

# trigonometry
sin(elev)
cos(elev)
tan(elev)

asin(elev)
acos(elev)
atan(elev)

atan(elevs)
atan2(elev, elev^1.2)
atan2(elevs, elev^1.2)
atan2(elev, elevs^1.2)
atan2(elevs, elevs^1.2)

# logarithms
exp(elev)
log(elev)
ln(elev)
log2(elev)
log1p(elev)
log10(elev)
log10p(elev)
log(elev, 3)

log(elevs)

# rounding
round(elev + 0.5)
floor(elev + 0.5)
ceiling(elev + 0.5)
trunc(elev + 0.5)

}
```
