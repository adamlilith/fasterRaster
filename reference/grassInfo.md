# GRASS citation, version, and copyright information

Report the **GRASS** citation, version/release year, version number, or
copyright information.

## Usage

``` r
grassInfo(x = "citation")
```

## Arguments

- x:

  Character: What to return. Any of:

  - `"citation"` (default)

  - `"copyright"`: Copyright information

  - `"version"`: Version number and release year

  - `"versionNumber"`: Version number as numeric, major and minor only
    (e.g., 8.4)

  Partial matching is used and case is ignored.

## Value

Character.

## Examples

``` r
if (grassStarted()) {

# Citation
grassInfo()

# Version number
grassInfo("version")

# Version number
grassInfo("versionNumber")

# Version number
grassInfo("versionNumber")

# Copyright
grassInfo("copyright")

}
```
