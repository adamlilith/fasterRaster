# Replace NAs in a data.table or data.frame column, or in a vector

This function replaces `NA`s in one or more `data.table`, `data.frame`,
or `matrix` columns, or in vectors, with a user-defined value.

## Usage

``` r
# S4 method for class 'data.frame'
replaceNAs(x, replace, cols = NULL)

# S4 method for class 'matrix'
replaceNAs(x, replace, cols = NULL)

# S4 method for class 'data.table'
replaceNAs(x, replace, cols = NULL)

# S4 method for class 'numeric'
replaceNAs(x, replace)

# S4 method for class 'integer'
replaceNAs(x, replace)

# S4 method for class 'logical'
replaceNAs(x, replace)

# S4 method for class 'character'
replaceNAs(x, replace)
```

## Arguments

- x:

  A `data.table` or `data.frame` or `matrix`, or a vector of numeric,
  integer, logical, or character values.

- replace:

  A value of any atomic class (numeric, integer, character, Date, etc.):
  Value to to replace `NA`s.

- cols:

  `NULL`, character, numeric, integer, or logical vector: Indicates
  columns for which to replace `NA`s. If `NULL`, then all columns will
  have `NA`s replaced. If a character, this is the column name(s). If
  numeric or integer, this is the columns' indices. If logical, columns
  with `TRUE` have `NA`s replaced. If a logical vector has fewer than
  the total number of columns, it will be recycled.

## Value

A `data.table`, `data.frame`, `matrix`, or vector.

## Examples

``` r
library(data.table)

dt <- data.table(
   x = 1:10,
   y = letters[1:10],
   z = rnorm(10)
)

# make some values NA
dt[x == 4 | x == 8, y := NA_character_]
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.85520250
#>  2:     2      b -0.28689522
#>  3:     3      c  0.89496163
#>  4:     4   <NA>  0.06730444
#>  5:     5      e -0.16267634
#>  6:     6      f -0.82731017
#>  7:     7      g  1.87650562
#>  8:     8   <NA>  0.76644020
#>  9:     9      i  0.97995670
#> 10:    10      j  1.32178099
dt
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.85520250
#>  2:     2      b -0.28689522
#>  3:     3      c  0.89496163
#>  4:     4   <NA>  0.06730444
#>  5:     5      e -0.16267634
#>  6:     6      f -0.82731017
#>  7:     7      g  1.87650562
#>  8:     8   <NA>  0.76644020
#>  9:     9      i  0.97995670
#> 10:    10      j  1.32178099

# Replace NAs:
replaceNAs(dt, replace = -99, cols = "y")
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.85520250
#>  2:     2      b -0.28689522
#>  3:     3      c  0.89496163
#>  4:     4    -99  0.06730444
#>  5:     5      e -0.16267634
#>  6:     6      f -0.82731017
#>  7:     7      g  1.87650562
#>  8:     8    -99  0.76644020
#>  9:     9      i  0.97995670
#> 10:    10      j  1.32178099
dt
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.85520250
#>  2:     2      b -0.28689522
#>  3:     3      c  0.89496163
#>  4:     4    -99  0.06730444
#>  5:     5      e -0.16267634
#>  6:     6      f -0.82731017
#>  7:     7      g  1.87650562
#>  8:     8    -99  0.76644020
#>  9:     9      i  0.97995670
#> 10:    10      j  1.32178099

# Drop rows:
dropped <- dropRows(dt, 8:10)
dropped
#>        x      y           z
#>    <int> <char>       <num>
#> 1:     1      a -0.85520250
#> 2:     2      b -0.28689522
#> 3:     3      c  0.89496163
#> 4:     4    -99  0.06730444
#> 5:     5      e -0.16267634
#> 6:     6      f -0.82731017
#> 7:     7      g  1.87650562

# NB May not print... in that case, use:
print(dropped)
#>        x      y           z
#>    <int> <char>       <num>
#> 1:     1      a -0.85520250
#> 2:     2      b -0.28689522
#> 3:     3      c  0.89496163
#> 4:     4    -99  0.06730444
#> 5:     5      e -0.16267634
#> 6:     6      f -0.82731017
#> 7:     7      g  1.87650562

# We can also use replaceNAs() on vectors:
y <- 1:10
y[c(2, 10)] <- NA
replaceNAs(y, -99)
#>  [1]   1 -99   3   4   5   6   7   8   9 -99

# Same as:
y <- 1:10
y[c(2, 10)] <- NA
y[is.na(y)] <- -99
```
