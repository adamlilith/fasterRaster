# Get a GLocation's working directory

This function returns the working directory of a `GLocation` object.

## Usage

``` r
# S4 method for class 'GLocation'
.workDir(x)

# S4 method for class 'missing'
.workDir(x)
```

## Arguments

- x:

  A `GLocation` object or missing. If an object, returns the working
  folder in which the object is saved by **GRASS**. If missing, then
  just returns the working folder (same as `faster("workDir")`).

## Value

Character.
