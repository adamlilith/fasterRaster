# Open the help page for a GRASS tool

This function opens the manual page for a **GRASS** tool (function) in
your browser.

## Usage

``` r
grassHelp(x, online = FALSE)
```

## Arguments

- x:

  Character: Any of:

  - The name of a **GRASS** tool (e.g., `"r.mapcalc"`).

  - `"toc"`: **GRASS** manual table of contents.

  - `"index"`: Display an index of topics.

- online:

  Logical: If `FALSE` (default), show the manual page that was included
  with your installation of **GRASS** on your computer. If `FALSE`, show
  the manual page online (requires an Internet connection). In either
  case, the manual page will display for the version of **GRASS** you
  have installed.

## Value

Nothing (opens a web page).

## Examples

``` r
if (grassStarted() & interactive()) {

# Open help pages for `r.mapcalc` and `r.sun`:
grassHelp("r.mapcalc")
grassHelp("r.sun")

# GRASS table of contents:
grassHelp("toc")

# Index page:
grassHelp("index")

}
```
