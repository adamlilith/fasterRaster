# Save a GVector to disk

This function saves a `GVector` to disk directly from a **GRASS**
session.

By default, files will be of OGC GeoPackage format (extension
"`.gpkg`"), but this can be changed with the `format` argument. You can
see a list of supported formats by simply using this function with no
arguments, as in `writeVector()`, or by consulting the online help page
for **GRASS** tool `v.out.ogr` (see `grassHelp("v.out.ogr")`).

Note that if the vector has a data table attached and at least one
numeric or integer column has an `NA` or `NaN` value, the function will
yield a warning like:

    Warning 1: Invalid value type found in record 2 for field column_with_NA_or_NaN. This warning will no longer be emitted.

Also note that despite the promise, this warning will be displayed
again.

## Usage

``` r
# S4 method for class 'GVector,character'
writeVector(
  x,
  filename,
  overwrite = FALSE,
  format = NULL,
  attachTable = TRUE,
  ...
)

# S4 method for class 'missing,missing'
writeVector(x, filename)
```

## Arguments

- x:

  A `GVector`.

- filename:

  Character: Path and file name.

- overwrite:

  Logical: If `FALSE` (default), do not save over existing files.

- format:

  Character or `NULL`: File format. If `NULL` (default), then the
  function will attempt to get the format from the file name extension.
  Partial matching is used and case is ignored. You can see a list of
  formats using `writeVector()` (no arguments). Some common formats
  include:

  - `"GPKG"`: OGC GeoPackage (extension `.gpkg`).

  - `"CSV"`: Comma-separated value... saves the data table only, not the
    geometries (extension `.csv`).

  - `"ESRI Shapefile"`: ESRI shapefile (extension `.shp`).

  - `"GeoJSON"`: GeoJSON (extension `GeoJSON`)

  - `"KML"`: Keyhole Markup Language (extension `.kml`)

  - `"netCDF"`: NetCDF (extension `.ncdf`)

  - `"XLSX"`: MS Office Open XML spreadsheet (extension `.xlsx`).

- attachTable:

  Logical: If `TRUE` (default), attach the attribute to table to the
  vector before saving it. If `FALSE`, the attribute table will not be
  attached.

- ...:

  Additional arguments to send to **GRASS** tool `v.out.ogr` (see
  `grassHelp("v.out.ogr")`).

## Value

Invisibly returns a `GRaster` (the input, `x`). Also saves the vector to
disk.

## See also

[`terra::writeVector()`](https://rspatial.github.io/terra/reference/writeVector.html),
[`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html),
**GRASS** tool `v.out.ogr` (see `grassHelp("v.out.ogr")`)

[`terra::writeVector()`](https://rspatial.github.io/terra/reference/writeVector.html),
the **GRASS** tool manual page for `v.out.ogr` (see
`grassHelp("v.out.ogr")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madRivers <- fastData("madRivers")

# What file formats can we attempt to write?
writeVector()

# Convert SpatVector to GVector
rivers <- fast(madRivers)
rivers

# Save GVector to disk as GeoPackage
filename <- tempfile(fileext = ".gpkg")
writeVector(rivers, filename)

# Save GVector to disk as ESRI Shapefile
filename <- tempfile(fileext = ".shp")
writeVector(rivers, filename)

# Save GVector to disk as Google Earth KML
filename <- tempfile(fileext = ".klm")
writeVector(rivers, filename)

# Save GVector data table to disk as comma-separated file
filename <- tempfile(fileext = ".csv")
writeVector(rivers, filename)

# Save GVector data table to disk as NetCDF
filename <- tempfile(fileext = ".ncdf")
writeVector(rivers, filename)

# Save GVector data table to disk as Excel file
filename <- tempfile(fileext = ".xlsx")
writeVector(rivers, filename)

}
```
