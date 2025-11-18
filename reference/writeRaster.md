# Save a GRaster to disk

This function saves a `GRaster` to disk directly from a **GRASS**
session. It is faster than using
[`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md),
then saving the output of that to disk (because
[`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md)
actually save the raster to disk, anyway).

The function will attempt to ascertain the file type to be ascertained
from the file extension, but you can specify the format using the
`format` argument (see entry for `...`). You can see a list of supported
formats by simply using this function with no arguments, as in
`writeRaster()`, or by consulting the online help page for the **GRASS**
tool `r.out.gdal` (see `grassHelp("r.out.gdal")`). Only the `GeoTIFF`
file format is guaranteed to work for multi-layered rasters.

The function will attempt to optimize the `datatype` argument, but this
can take a long time. You can speed this up by setting `datatype`
manually. Note that if you are saving a "stack" of `GRaster`s with
different `datatype`s, the one with the highest information density will
be used (e.g., low-bit integer \< high-bit integer \< floating-point \<
double-floating point). This can make rasters with lower datatypes much
larger on disk. In these cases, it make be best to save rasters with
similar `datatype`s together.

## Usage

``` r
# S4 method for class 'GRaster,character'
writeRaster(
  x,
  filename,
  overwrite = FALSE,
  datatype = NULL,
  byLayer = FALSE,
  names = TRUE,
  levelsExt = NULL,
  compress = "LZW",
  warn = TRUE,
  ...
)

# S4 method for class 'missing,missing'
writeRaster(x, filename)
```

## Arguments

- x:

  A `GRaster` or missing: If missing, a table of supported file types is
  reported.

- filename:

  Character: Path and file name.

- overwrite:

  Logical: If `FALSE` (default), do not save over existing file(s).

- datatype:

  `NULL` (default) or character: The datatype of the values stored in
  non-ASCII rasters. If `NULL`, this will be ascertained from the
  raster, and the function usually does a good job at it. However, you
  can force it manually, but note that in some cases, trying to save a
  `GRaster` using an inappropriate `datatype` for its values can result
  in an error or in the function exiting without an error but also
  without having written the raster to disk. The argument can take any
  of those shown below under the first four columns, but whatever is
  used, it will be converted to the **GDAL** version.

  |                  |           |           |           |                                                               |
  |------------------|-----------|-----------|-----------|---------------------------------------------------------------|
  | **fasterRaster** | **terra** | **GRASS** | **GDAL**  | **Values**                                                    |
  | `integer`        | `INT1U`   | `CELL`    | `Byte`    | Integer values from 0 to 255                                  |
  | `integer`        | `INT2U`   | `CELL`    | `UInt16`  | Integer values from 0 to 65,534                               |
  | `integer`        | `INT2S`   | `CELL`    | `Int16`   | Integer values from -32,767 to -32,767                        |
  | `integer`        | `INT4S`   | `CELL`    | `Int32`   | Integer values from -2,147,483,647 to 2,147,483,647           |
  | `float`          | `FLT4S`   | `FCELL`   | `Float32` | Values from -3.4E+38 to 3.4E+38, including decimal values     |
  | `double`         | `FLT8S`   | `DCELL`   | `Float64` | Values from -1.79E+308 to 1.79E+308, including decimal values |
  | `factor`         | `INT`\*   | `CELL`    | `INT*`    | Integer values corresponding to categories                    |

  `*` Depends on the integers (signed/unsigned, range of values).
  Categorical rasters will have an associated file saved with them that
  has category values and labels. The file name will be the same as the
  raster's file name, but end with the extension given by `levelsExt`
  (`.csv` by default).

- byLayer:

  Logical: If `FALSE` (default), multi-layer rasters will be saved in
  one file. If `TRUE`, the each layer will be saved in a separate file.
  The filename from `filename` will be amended so that it ends with
  `_<name>` (then the file extension), where `<name>` is give by
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md).
  Note that if any characters in raster names will not work in a file
  name, then the function will fail (e.g., a backslash or question
  mark).

- names:

  Logical: If `TRUE` (default), save a file with raster layer names. The
  file will have the same name as the raster file but end with
  "`_names.csv`". Currently, the
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md)
  attribute of rasters cannot be saved in the raster, which can create
  confusion when multi-layered rasters are saved. Turning on this option
  will save the ancillary file with layer names. If it exists, this file
  will be read by
  [`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
  so layer names are assigned when the raster is read by that function.
  The absence of a "names" file will not create any issues with this
  function or
  [`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md),
  other than not having the metadata on layer names.

- levelsExt:

  Character, logical, or `NULL` (default): Name of the file extension
  for the "levels" file that accompanies a categorical `GRaster`. When
  saving categorical rasters, the raster file is accompanied with a
  "levels" file that contain information on the levels of the raster.
  This file is the same as `filename`, except it has a different
  extension. Valid values depend on how many raster layers are saved at
  a time (case is ignored):

  - DefaultOne raster layer: `".csv"`

  - Two or more layers, with at least one categorical raster: `".rds"`,
    `".rda"`, `".rdat"`, `".rdata"`

  - Any: `NULL` or `TRUE` automatically selects either `".csv"` (one
    raster layer) or `".rds` (two or more)

  - Any: `FALSE` disables saving of a levels file.

- compress:

  Character: Type of compression to use for GeoTIFF files:

  - `"LZW"` (default)

  - `"DEFLATE"`

  - `"PACKBITS"`

  - `"LZMA"`

  - `NULL`: No compression is used, but the file can still be reduced in
    size by using zip, gzip, or other compressions.

- warn:

  Logical: If `TRUE` (default), display a warning if the `datatype`
  argument does not match the value given by `datatype(x, "GDAL")`, or
  if the `fileExt` argument will not work with the given raster and so
  has been automatically changed.

- ...:

  Additional arguments. These can include:

  - `bigTiff`: Logical: If `TRUE`, and the file format is a GeoTIFF and
    would be larger than 4 GB (regardless of compression), then the file
    will be saved in BIGTIFF format.

  - `format`: Character, indicating file format. This is usually
    ascertained from the file extension, but in case this fails, it can
    be stated explicitly. When using other formats, you may have to
    specify the `createopts` argument, too (see help page for **GRASS**
    tool `r.out.gdal`). Two common formats include:

    - `"GTiff"` (default): GeoTIFF `filename` ends in `.tif`.

    - `"ASC"`: ASCII `filename` ends in `.asc`

  - Additional arguments to send to **GRASS** modules `r.out.gdal` and
    `r.out.ascii`.

  - `precision`: Numeric: For ASCII files, you may need to state the
    number of significant digits. 32-bit values have 7 digits and 64-bit
    values have 16. So in these cases the argument would be
    `precision=7` or `precision=16`.

## Value

A `GRaster` (invisibly). A raster is also saved to disk.

## See also

[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
**GRASS** tool `r.out.gdal` (see `grassHelp("r.out.gdal")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madChelsa <- fastData("madChelsa")

### What raster formats can we attempt to write?
writeRaster()

### Save GRaster to disk (using temporary file)
elev <- fast(madElev)
filename <- tempfile(fileext = ".tif")
writeRaster(elev, filename)

# Load raster from disk
elev2 <- fast(filename)
elev2

### Save multi-layer GRaster to disk in one file (using temporary file)
chelsa <- fast(madChelsa)
filename <- tempfile(fileext = ".tif")
writeRaster(chelsa, filename)

# Load raster from disk
chelsa2 <- fast(filename)
chelsa2

### Save multi-layer GRaster to disk layer-by-layer (using temporary file)
chelsa <- fast(madChelsa)
filename <- tempfile(fileext = ".tif")
writeRaster(chelsa, filename, byLayer = TRUE)

# Load one of the rasters from disk
filename2 <- sub(filename, pattern = ".tif", replacement = "_bio1.tif")
chelsaBio1 <- fast(filename2)
chelsaBio1

}
```
