# Remove rows in a data.table, data.frame, or matrix.

As of September of 2023, the **data.table** package does not have a
function for removing rows by index. This function does this job.

## Usage

``` r
# S4 method for class 'data.table'
dropRows(x, drops)

# S4 method for class 'data.frame'
dropRows(x, drops)

# S4 method for class 'matrix'
dropRows(x, drops)
```

## Arguments

- x:

  A `data.table` or `data.frame`.

- drops:

  Numeric, integer, or logical vector: Indices or indicators of rows to
  remove.

  If a logical vector is supplied, rows that correspond to `TRUE` will
  be removed. If the vector is shorter than the number of rows, values
  of `drops` will be recycled.

## Value

A `data.table` or `data.frame`.

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following objects are masked from ‘package:fasterRaster’:
#> 
#>     %notin%, as.data.table

dt <- data.table(
   x = 1:10,
   y = letters[1:10],
   z = rnorm(10)
)

# make some values NA
dt[x == 4 | x == 8, y := NA_character_]
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.25148344
#>  2:     2      b  0.44479712
#>  3:     3      c  2.75541758
#>  4:     4   <NA>  0.04653138
#>  5:     5      e  0.57770907
#>  6:     6      f  0.11819487
#>  7:     7      g -1.91172049
#>  8:     8   <NA>  0.86208648
#>  9:     9      i -0.24323674
#> 10:    10      j -0.20608719
dt
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.25148344
#>  2:     2      b  0.44479712
#>  3:     3      c  2.75541758
#>  4:     4   <NA>  0.04653138
#>  5:     5      e  0.57770907
#>  6:     6      f  0.11819487
#>  7:     7      g -1.91172049
#>  8:     8   <NA>  0.86208648
#>  9:     9      i -0.24323674
#> 10:    10      j -0.20608719

# Replace NAs:
replaceNAs(dt, replace = -99, cols = "y")
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.25148344
#>  2:     2      b  0.44479712
#>  3:     3      c  2.75541758
#>  4:     4    -99  0.04653138
#>  5:     5      e  0.57770907
#>  6:     6      f  0.11819487
#>  7:     7      g -1.91172049
#>  8:     8    -99  0.86208648
#>  9:     9      i -0.24323674
#> 10:    10      j -0.20608719
dt
#>         x      y           z
#>     <int> <char>       <num>
#>  1:     1      a -0.25148344
#>  2:     2      b  0.44479712
#>  3:     3      c  2.75541758
#>  4:     4    -99  0.04653138
#>  5:     5      e  0.57770907
#>  6:     6      f  0.11819487
#>  7:     7      g -1.91172049
#>  8:     8    -99  0.86208648
#>  9:     9      i -0.24323674
#> 10:    10      j -0.20608719

# Drop rows:
dropped <- dropRows(dt, 8:10)
dropped
#>        x      y           z
#>    <int> <char>       <num>
#> 1:     1      a -0.25148344
#> 2:     2      b  0.44479712
#> 3:     3      c  2.75541758
#> 4:     4    -99  0.04653138
#> 5:     5      e  0.57770907
#> 6:     6      f  0.11819487
#> 7:     7      g -1.91172049

# NB May not print... in that case, use:
print(dropped)
#>        x      y           z
#>    <int> <char>       <num>
#> 1:     1      a -0.25148344
#> 2:     2      b  0.44479712
#> 3:     3      c  2.75541758
#> 4:     4    -99  0.04653138
#> 5:     5      e  0.57770907
#> 6:     6      f  0.11819487
#> 7:     7      g -1.91172049

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
