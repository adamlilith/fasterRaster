# Setup fasterRaster for ABS

This is a secret function to be used for faster development of
**fasterRaster**. It calls
[`faster()`](https://github.com/adamlilith/fasterRaster/reference/faster.md)
to set the install directory for **GRASS**, increases default memory,
and number of cores. The function assumes development is on a Windows
machine.

## Usage

``` r
.backdoor(start = FALSE, ver = "84")
```

## Arguments

- start:

  Logical: If `TRUE`, start the **GRASS** session by creating the
  `madElev` `GRaster`.

- ver:

  Character: **GRASS**: e.g., "83" or "84".

## Value

`TRUE` (invisibly).
