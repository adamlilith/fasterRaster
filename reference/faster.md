# Set or get options shared across fasterRaster functions

`faster()` either sets or gets options used across **fasterRaster**
functions. Its use can vary:

- Get current values of a particular option: Use
  `faster("option_name")`. Values will remain unchanged.

- Get current values of all options: Use `faster()` (no arguments).
  Values will remain unchanged.

- Get default values of a particular option: Use
  `faster("option_name", default = TRUE)`. Values will remain unchanged.

- Get default values of all options: Use `faster(default = TRUE)`.
  Values will remain unchanged.

- Set values of particular options: Use the form
  `faster(option 1 = value1, option2 = value2)`.

- Set all options to their defaults: Use `faster(restore = TRUE)`.

You cannot simultaneously get and set options.

To run most **fasterRaster** functions, you must set the `grassDir`
option.

## Usage

``` r
faster(..., default = FALSE, restore = FALSE)
```

## Arguments

- ...:

  Either:

  - A character vector: Name(s) of option(s) to get values of;

  - An option and the value of the option using an `option = value`
    pattern; or

  - A names `list` with names that match the options you wish to change,
    and with values to assign to each option.

  Options include:

  - `grassDir` (character): The folder in which **GRASS** is installed
    on your computer. You must set this option to run most
    **fasterRaster** functions. Depending on your operating system, your
    install directory will look something like this:

    - Windows: `"C:/Program Files/GRASS GIS 8.4"`

    - Mac OS: `"/Applications/GRASS-8.4.app/Contents/Resources"`

    - Linux: `"/usr/local/grass"`

  - `addonsDir` (character): Folder in which **GRASS** addons are
    stored. If `NA` and `grassDir` is not `NA`, this will be assumed to
    be `file.path(grassDir, "addons")`. The default values is `NA`.

  - `cores` (integer/numeric integer): Number of processor cores to use
    on a task. The default is 2. Some **GRASS** modules are
    parallelized.

  - `memory` (integer/numeric): The amount of memory to allocate to a
    task, in GB, for **GRASS**. The default is 2048 MB (i.e., 2 GB).
    Some **GRASS** modules can take advantage of more memory.

  - `useDataTable` (logical): If `FALSE` (default), functions that
    return tabular output produce `data.frame`s. If `TRUE`, output will
    be `data.table`s from the **data.table** package. This can be much
    faster, but it might require you to know how to use `data.table`s if
    you want to manipulate them in **R**. You can always convert them to
    `data.frame`s using
    [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

  - `verbose` (logical): If `TRUE`, show progress during function
    operations and other messages. Default is `FALSE`. This overrides
    the value of any `verbose` argument in a function.

  - `debug` (logical): If `TRUE`, show **GRASS** messages and otherwise
    hidden slots in classes. This is mainly used for debugging, so most
    users will want to keep this at its default, `FALSE`.

  - `workDir` (character): The folder in which **GRASS** rasters,
    vectors, and other objects are created and manipulated. By default,
    this is given by
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html). Note that on
    some systems, changing the default folder to somewhere else can
    cause problems with **fasterRaster** being able to find rasters in
    **GRASS** that have been created.

- default:

  Logical: Return the default value(s) of the option(s). The default
  value of `default` is `FALSE`.

- restore:

  Logical: If `TRUE`, the all options will be reset to their default
  values. The default is `FALSE`.

## Value

If options are changed, then a named list of option values *before* they
were changed is returned invisibly.

If option values are requested, a named list with option values is
returned (not invisibly).

## Examples

``` r
if (grassStarted()) {

# See current values for options:
faster("grassDir")
faster("cores")
faster("memory")
faster("useDataTable")
faster() # all options

# See default values for options:
faster("cores", default = TRUE)
faster(default = TRUE) # all options

# Set options (change accordingly for your system!!!)
if (FALSE) {

   opts. <- faster() # remember starting values of options

   faster(grassDir = "C:/Program Files/GRASS GIS 8.4")
   faster(verbose = TRUE, memory = 1024, cores = 1)

   faster(c("grassDir", "verbose", "memory", "cores"))

   faster(opts.) # reset options to starting values

}

}
```
