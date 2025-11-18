# Identify terrain feature types

Geomorphons are idealized terrain types calculated from an elevator
raster based on a moving window of a given size. The window is a torus
(which can have an inner radius of 0, so can also be a circle), which
allows it to identify geomorphons of a given size while ignoring ones
larger or smaller. There are 10 basic geomorphons. Consult the the
manual for **GRASS** tool `r.geomorphon` using
`grassHelp("r.geomorphon")` for more details and diagrams of each type
of geomorphon. Geomorphon types include:

1.  Flat areas: Focal area has approximately the same elevation as
    surrounding areas

2.  Pits: An area is lower than all other surrounding areas

3.  Valley: Focal area has elevation similar to two opposing side of the
    window but lower than the other two opposing sides

4.  Footslope: Focal region is at the "bottom" of a slope

5.  Hollow: A small valley/indention in the crest of a hill

6.  Slope: Cells in the window form an approximately uniform slope

7.  Spur: An extrusion at the foot of a hill (i.e.,, a small hill
    extending out from the foot of a slope)

8.  Shoulder: The crest of a slope

9.  Ridge: Opposite of a valley; focal area is higher than two opposing
    sides but approximately the same elevation as the other two opposing
    sides

10. Peak: Focal area is higher than any other in the window

## Usage

``` r
# S4 method for class 'GRaster'
geomorphons(
  x,
  inner = 0,
  outer = 3,
  unit = "cells",
  flat = 1,
  flatDist = 0,
  mode = "1"
)
```

## Arguments

- x:

  A single-layer `GRaster`, typically representing elevation.

- inner, outer:

  Integer: Inner and outer radii of the torus used to identify
  geomorphons, in cells or meters (set by argument `unit`). The inner
  default value is 0 and the outer default value is 3. The `outer`
  radius sets the maximum size of a geomorphon that that can be
  identified, and `inner` sets the smallest size. If `unit` is
  `"meters"`, the value of `outer` must be larger than the smaller
  dimension of any cell in the east-west and north-south directions.

- unit:

  Character: Units of `inner` and outer; can be either `"cells"`
  (default) or `"meters"`. Partial matching is used.

- flat:

  Numeric value \>= 0: Minimum difference (in degrees) between the focal
  area areas around it for a geomorphon to be considered as "flat".
  Larger cells (i.e., ~1 km resolution or larger) require smaller values
  (\<\<1) to correctly identify flat areas. Higher values result in more
  areas being classified as "flat" geomorphons. The default value is 1.

- flatDist:

  Numeric: Distance (in meters) to correct for the effect of large
  distances on the diminished capacity to identify "flat" geomorphons.
  If the distance between the focal area and a surrounding area
  surpasses this distance, then the effective value of `flat` will be
  reduced

- mode:

  Character: Method for implementing the zenith/line-of-site search.
  Partial matching is used:

  - `"1"` (default): The "original" geomorphon mode (in **GRASS** tool
    `r.geomorphon`, the "anglev1" method)

  - `"2"`: Better handling of cases with equal zenith/nadir angles (the
    "anglev2" method)

  - `"2d"`: As `"2"`, but takes into account zenith/nadir distance
    ("anglev2_distance" method)

## Value

A categorical `GRaster` where each geomorphon is a category (see
[`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)).

## See also

**GRASS** manual for tool `r.geomorphon` (see
`grassHelp("r.geomorphon")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster:
elev <- fast(madElev)

# Geomorphons:
geos <- geomorphons(elev)
geos
levels(geos) # levels
freq(geos) # frequencies across cells

col <- c("gray90", "red", "orange", "blue", "green", "pink", "firebrick",
  "purple", "gray50", "black")
plot(geos, col = col)

}
```
