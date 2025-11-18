# Format a numeric series into an SQL value call

This function takes as its argument a vector of integers or numeric
values, and converts sequential runs to a range while keeping
non-sequential values as-is. For example,
`c(1, 5, 6, 7, 8, 9, 15, 16, 20)` becomes `"1,5-9,15-16,20"`. This
reduces the number of characters necessary to supply to a SQL condition.
This function is mainly of use to developers.

## Usage

``` r
seqToSQL(x, maxChar = 29900, sort = TRUE)
```

## Arguments

- x:

  A vector of numerical values. The vector should be sorted from lowers
  to highest for the most efficient "compression" of sequential ranges.
  Values will be coerced to class `integer`.

- maxChar:

  Integer or numeric: Maximum number of characters to include in the
  output. If the output has more than this number of characters, the
  remainder is dropped, and the `trim` attribute of the output is set to
  `TRUE`. The default is 29900, which is the maximum length of an SQL
  statement that **GRASS** seems to be able to handle (minus a safety
  margin).

- sort:

  Logical: If `TRUE` (default), sort `x` before converting to SQL. This
  can reduce the length of the output.

## Value

A character string. The string has three attributes. The `trim`
attribute is `TRUE` or `FALSE`, depending on whether `maxChar` was
reached or not (and subsequent numbers dropped from the string). The
`lastIndex` attribute is the last index of `x` that was processed (i.e.,
the index of the last value in the output), and the number of values
represented by the output.

## Examples

``` r
x <- 1:5
seqToSQL(x)
#> [1] "1-5"
#> attr(,"trim")
#> [1] FALSE
#> attr(,"lastIndex")
#> [1] 5

x <- c(1:5, 7)
seqToSQL(x)
#> [1] "1-5,7"
#> attr(,"trim")
#> [1] FALSE
#> attr(,"lastIndex")
#> [1] 6

x <- c(1:5, 7, 15:16)
y <- c(1:5, 7, 15:16, 20)
seqToSQL(x)
#> [1] "1-5,7,15-16"
#> attr(,"trim")
#> [1] FALSE
#> attr(,"lastIndex")
#> [1] 8
seqToSQL(y)
#> [1] "1-5,7,15-16,20"
#> attr(,"trim")
#> [1] FALSE
#> attr(,"lastIndex")
#> [1] 9

seqToSQL(x, maxChar = 5)
#> [1] "1-5,7"
#> attr(,"trim")
#> [1] TRUE
#> attr(,"lastIndex")
#> [1] 6
seqToSQL(y, maxChar = 8)
#> [1] "1-5,7,15-16"
#> attr(,"trim")
#> [1] TRUE
#> attr(,"lastIndex")
#> [1] 8

seqToSQL(10:1, sort = FALSE)
#> [1] "10,9,8,7,6,5,4,3,2,1"
#> attr(,"trim")
#> [1] FALSE
#> attr(,"lastIndex")
#> [1] 10
seqToSQL(10:1, sort = TRUE)
#> [1] "1-10"
#> attr(,"trim")
#> [1] FALSE
#> attr(,"lastIndex")
#> [1] 10
```
