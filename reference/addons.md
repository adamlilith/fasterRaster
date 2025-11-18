# Test if addons directory exists and if an addon is installed

These functions handle **GRASS** addons, which are optional tools that
can be installed. Most functions in **fasterRaster** rely on "base"
**GRASS** tools (not addons), but a few do.

- `addons()`: Either lists all installed addons or verifies if one or
  more specific addons are installed.

- `installAddon()`: Installs a **GRASS** addon. An addon typically only
  needs installed once. You can install an addon, quit and restart
  **R**, attach **fasterRaster**, and any installed addons can be used
  without using this function again.

- `removeAddon()`: Delete an installed addon from your system.

## Usage

``` r
addons(x = NULL)

installAddon(x, check = TRUE)

removeAddon(x, check = TRUE)

.addons(x)
```

## Arguments

- x:

  Either `NULL` or a character specifying the name of a **GRASS** addons
  tool. If `NULL`, a vector of installed addons is returned. If a
  character vector is provided, a logical vector is returned, one per
  value in `x` that indicates if the respective addon is installed.

- check:

  Logical: If `TRUE`, check to see if the addon is available
  (`installAddon()`)or if it is installed (`removeAddon()`).

## Value

`addons()`: Logical. The other functions invisibly return a logical
value indicating if the operation succeeded or not.

## Examples

``` r
if (grassStarted()) {

# What addons are installed?
addons()

# Is a specific addon installed?
addons(c("v.centerpoint", "fake.addon"))

}
```
