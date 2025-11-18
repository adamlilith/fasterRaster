# Solar radiance and irradiance

The `sun()` function calculates beam (direct), diffuse and ground
reflected solar irradiation for a given day and set of topographic and
atmospheric conditions. The function relies on the **GRASS** tool
`r.sun`, the manual page for which contains a detailed explanation (see
`grassHelp("r.sun")`)

## Usage

``` r
sun(
  elevation,
  coeff_bh,
  coeff_dh,
  slope,
  aspect,
  hh,
  horizon_step = 90,
  albedo = 0.2,
  linke = 3,
  day = 1,
  step = 0.5,
  declination = NULL,
  solar_constant = 1367,
  distance_step = 1,
  npartitions = 1,
  beam_rad = TRUE,
  diff_rad = TRUE,
  refl_rad = TRUE,
  glob_rad = TRUE,
  insol_time = TRUE,
  lowMemory = FALSE
)
```

## Arguments

- elevation:

  A `GRaster` with values representing elevation (typically in meters).

- coeff_bh:

  A `GRaster`: A raster with values of the real-sky beam radiation
  coefficient. Valid values are between 0 and 1.

- coeff_dh:

  A `GRaster`: A raster with values of the real-sky diffuse radiation
  coefficient. Valid values are between 0 and 1.

- slope:

  A `GRaster`: This is a raster representing topographic slope in
  degrees. It can be generated using
  [`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md).

- aspect:

  A `GRaster`: This is a raster representing topographic aspect in
  degrees. It can be generated using
  [`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md).
  If generated with that function, "east orientation" *must* be used
  (i.e., argument `northIs0` must be `FALSE`).

- hh:

  A "stack" of `GRaster`s: This represents height of the horizon in
  radians in particular directions. Horizon height can be calculated
  using
  [`horizonHeight()`](https://github.com/adamlilith/fasterRaster/reference/horizonHeight.md).
  The directions *must* be in "east orientation" (i.e., argument
  `northIs0` in `horzionHeight()` must be `FALSE`). The directions must
  correspond with the sequence given by `horizon_step` (see next
  argument). For example, if `horizon_step` is 90, then `hh` must
  contain rasters representing horizon height at 0 (east), 90 (north),
  180 (west), and 270 (south) aspects.

- horizon_step:

  Numeric \>0: Difference between angular steps in which horizon height
  is measured. One horizon height raster will be made per value from 0
  to (360 - `horizon_step`) degrees.

- albedo:

  A `GRaster` or a numeric value: This is either a raster with values of
  ground albedo or a numeric value (in which case albedo is assumed to
  be the same everywhere). Albedo is unit-less, and the default value is
  0.2.

- linke:

  A `GRaster` or a numeric value: This is either a raster with values of
  the Linke atmospheric turbidity coefficient or a numeric value (in
  which case the same value is assumed for all locations). The Linke
  coefficient is unit-less. The default value is 3, but see also the
  **GRASS** manual page for tool `r.sun` (`grassHelp("r.sun")`).

- day:

  Positive integer between 1 to 365, inclusive: Day of year for which to
  calculate ir/radiation. Default is 1 (January 1st).

- step:

  Positive integer between 0 and 24, inclusive. Time step in hours for
  all-day radiation sums. Decimal values are OK.

- declination:

  Numeric or `NULL` (default). Declination value. If `NULL`, this is
  calculated automatically.

- solar_constant:

  Positive numeric: The solar constant (solar energy hitting the top of
  the atmosphere). Default is 1367. Units are W / m^2.

- distance_step:

  Positive numeric between 0.5 and 1.5, inclusive: Sampling distance
  coefficient. Default is 1.

- npartitions:

  Positive numeric. Number of chunks in which to read input files.
  Default is 1.

- beam_rad:

  Logical: If `TRUE` (default), generate a raster with beam irradiation
  with units of Wh / m^2 / day ("mode 2" of the `r.sun` **GRASS** tool).

- diff_rad:

  Logical: If `TRUE` (default), generate a raster representing
  irradiation in Wh / m^2 /day

- refl_rad:

  Logical: If `TRUE` (default), generate a raster with ground-reflected
  irradiation with units of Wh / m^2 / day ("mode 2" of the `r.sun`
  **GRASS** tool).

- glob_rad:

  Logical:. If `TRUE` (default), generate a raster with total
  irradiance/irradiation with units of Wh / m^2 / day ("mode 2" of the
  `r.sun` **GRASS** tool).

- insol_time:

  Logical: If `TRUE` (default), generate a raster with total insolation
  time in hours ("mode 2" of the `r.sun` **GRASS** tool).

- lowMemory:

  Logical: If `TRUE`, use the low-memory version of the `r.sun`
  **GRASS** tool. The default is `FALSE`.

## Value

A raster or raster stack stack with the same extent, resolution, and
coordinate reference system as `elevation`. Assuming all possible
rasters are generated they represent:

- `beam_rad`: Beam radiation (Watt-hours/m2/day)

- `diff_rad`: Diffuse radiation (Watt-hours/m2/day)

- `refl_rad`: Reflected radiation (Watt-hours/m2/day)

- `glob_rad`: Global radiation (Watt-hours/m2/day)

- `insol_time`: Insolation duration (hours)

## See also

[`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md),
[`horizonHeight()`](https://github.com/adamlilith/fasterRaster/reference/horizonHeight.md),
**GRASS** manual page for tool `r.sun` (see `grassHelp("r.sun")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)

### Calculate input rasters

# Values below are just a guess
coeff_bh <- coeff_dh <- elev
coeff_bh[] <- 0.4
coeff_dh[] <- 0.6

slope <- terrain(elev, "slope")
aspect <- terrain(elev, "aspect", northIs0 = FALSE)

horizon_step <- 90
hh <- horizonHeight(elev, step = horizon_step, northIs0 = FALSE)

### calculate solar ir/radiance

solar <- sun(
  elevation = elev,
  coeff_bh = coeff_bh,
  coeff_dh = coeff_dh,
  slope = slope,
  aspect = aspect,
  hh = hh,
  horizon_step = horizon_step,
  albedo = 0.2,
  linke = 1.5,
  day = 1,
  step = 0.5,
  declination = NULL,
  solar_constant = 1367,
  
  distance_step = 1,
  npartitions = 1,

  beam_rad = TRUE,
  diff_rad = TRUE,
  refl_rad = TRUE,
  glob_rad = TRUE,
  insol_time = TRUE,

  lowMemory = FALSE
)

solar

}
```
