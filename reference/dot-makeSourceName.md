# Make unique GRASS name for rasters, vectors, etc.

Make unique GRASS name for rasters, vectors, etc.

## Usage

``` r
.makeSourceName(x = NULL, type = NULL, n = 1L, name = NULL)
```

## Arguments

- x:

  Character or `NULL`: Descriptive string. **Developers, please note**:
  To assist with debugging, **GRASS** objects created by a **GRASS**
  tool have the tool named in this argument (with underscores). Example:
  "v_in_ogr" or "r_resample".

- type:

  Character: `raster`, `raster3D`, `vector`, or `table`.

- n:

  Numeric integer: Number of names to make

- name:

  `NULL` (default) or `character`: Name of the output, attached as an
  attribute.

## Value

Character vector.
