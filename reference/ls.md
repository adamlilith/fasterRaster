# List objects in the active GRASS session

Display the `sources` (see
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md))
of all rasters and/or vectors that have been exported to or created in
the active **GRASS** session"s location and mapset.

## Usage

``` r
.ls(type = c("rasters", "vectors", "rasters3d", "groups"))
```

## Arguments

- type:

  The type of spatial objects to display. This can include `"rasters"`
  (all rasters), `"vectors"` (all spatial vectors), `"rasters3d"`
  (3D-rasters), and/or `"groups"` (groups). Partial matching is
  supported. If missing, all objects are displayed.

## Value

Character vector of names of **GRASS** objects.

## See also

[`ls()`](https://rdrr.io/r/base/ls.html)
