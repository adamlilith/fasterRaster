# Correlation between GRasters

Calculate the correlation, covariance, or chi-squared, Cramer's *V*, or
Kruskal-Wallis's *H* between two or more rasters. Note that cells that
are `NA` for any raster are ignored across all rasters.

## Usage

``` r
# S4 method for class 'GRaster'
layerCor(
  x,
  fun = "cor",
  cor = "Pearson",
  correct = TRUE,
  simulate = FALSE,
  nSim = 2000,
  integerAsNumeric = TRUE,
  na.rm = TRUE,
  verbose = FALSE
)
```

## Arguments

- x:

  A `GRaster` with two or more layers. Partial matching is used and
  capitalization ignored.

- fun:

  Character: Name of the statistic(s) to calculate:

  - `"cor"` (default): Pearson sample correlation (i.e., the denominator
    is `n - 1`). Appropriate for numeric-numeric raster comparisons.

  - `"cov"`: Covariance.

  - `"chisq"`: Chi-squared test and Cramer's *V*. Default for
    integer-integer, factor-factor, or integer-factor raster
    comparisons.

  - `"kw"`: Kruskal-Wallis *H* statistic. Appropriate for
    integer-numeric or factor-numeric raster comparisons.

  - `"auto"`: Automatically select the appropriate statistic based on
    the data types of the rasters and calculates the (approximate)
    effect size and (where necessary) rescale to the range from 0 to 1.
    If any two rasters are numeric, then the absolute value of the
    Pearson or Spearman correlation will be calculated (depending on
    argument `cor`). If any two rasters are integer or factor, then
    Cramer's *V* will be returned. If any two are numeric and
    factor/integer, then the square root of the proportion of variance
    explained by the Kruskal-Wallis test will be returned. This is
    `sqrt(H / (N - 1)`, where `H` is the KW test statistic and `N` the
    number of non-`NA` cells.

- cor:

  Either 'pearson' (default) or 'spearman'. Only used if `fun = "cor"`
  or `fun = "auto"` and any two rasters are numeric. Indicates the type
  of correlation statistic to calculate. Capitalization is ignored and
  partial matching is used.

- correct:

  Logical (only used if `fun = "chisq"`): If `TRUE` (default), then
  apply continuity correction when computing the test statistic for 2 by
  2 tables: one half is subtracted from all \|*O - E*\| differences;
  however, the correction will not be bigger than the differences
  themselves. No correction is performed if `simulate = TRUE`.

- simulate:

  Logical (only used if `fun = "chisq"`): If `TRUE`, then the *p*-value
  will be estimated by Monte Carlo simulation, using
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)\].
  This is recommended when there are many categories, because the
  chi-squared distribution is not a good approximation of the
  distribution of the test statistic in this case. The default is
  `FALSE` because simulating *p*-values can be time-consuming.

- nSim:

  Numeric or integer (only used if `fun = "chisq"`): Number of
  replicates for Monte Carlo simulation when `simulate` is `TRUE`. The
  default is 2000.

- integerAsNumeric:

  Logical: If `TRUE` (default), then treat integer rasters as numeric.
  This is useful for rasters that are stored as integers but are
  actually continuous variables. If `FALSE`, then treat integer rasters
  as categorical variables. Only applicable for Kruskal-Wallis test or
  when `cor = 'auto'`.

- na.rm:

  Logical: If `TRUE` (default), then remove cells with `NA` values in
  any raster. If `FALSE`, then pairwise comparisons of rasters will use
  all pairs of non-`NA` cells, even if they are `NA` in other
  `GRaster`s.

- verbose:

  Logical: If `TRUE`, then display progress. Default is `FALSE`. Ignored
  for some values of `fun`.

## Value

The output depends on the selected statistic:

- `"cor"`: A correlation `matrix`. An attribute "`n`" gives the number
  of cells used in each pairwise correlation.

- `"cov"`: A covariance `matrix`.

- `"chisq"`: A `list` with five or six elements:

  - `chisq`: A `matrix` of chi-squared values for each pairwise
    comparison;

  - `df`: A `matrix` of degrees of freedom for each pairwise comparison
    (only included if `simulate = FALSE`);

  - `p.value`: A `matrix` of *p*-values for each pairwise comparison.
    Note that most rasters have so many cells that even very small
    differences create very small *p*-values, so do not get too excited;

  - `nCats1`: A `matrix` with the number of categories/integer values
    for each raster in each pairwise comparison;

  - `nCats2`: A `matrix` with the number of categories/integer values
    for each raster in each pairwise comparison;

  - `nCells`: A `matrix` with the number of cells used in each pairwise
    comparison.

- `"kw"`: A matrix with *H* values for each comparison.

- `"auto"`: A `list` with these elements:

  - `stat`: A character vector with the name of the statistic calculated
    for each pairwise comparison.

  - `relative.effect.size`: A numeric matrix with the relative effect
    size of each comparison. Values are in the range of 0 to 1.

## See also

[`terra::layerCor()`](https://rspatial.github.io/terra/reference/layerCor.html),
[`stats::cor()`](https://rdrr.io/r/stats/cor.html),
[`stats::cov()`](https://rdrr.io/r/stats/cor.html)

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Example data
madChelsa <- fastData("madChelsa")

# Convert a SpatRaster to a GRaster:
chelsa <- fast(madChelsa)

# Correlation
layerCor(chelsa, "cor", na.rm = FALSE) # Pearson correlation
layerCor(chelsa, "cor", cor = "spearman", na.rm = FALSE) # Spearman correlation

# Covariance
layerCor(chelsa, "cov", na.rm = FALSE)

# To illustrate categorical tests, force two layers to be of type integer
chelsa[[1:2]] <- as.int(chelsa[[1:2]])

# Chi-^2 and Cramer's V (integer vs integer)
layerCor(chelsa[[1:2]], "chisq", na.rm = FALSE)

# Kruskal-Wallis test (integer vs continuous)
integerCont <- c(chelsa[[1]], chelsa[[3]])
layerCor(integerCont, "kw", na.rm = FALSE)

# automatic by data type
layerCor(chelsa, "auto", cor = "spear", na.rm = FALSE,
   verbose = TRUE, integerAsNumeric = FALSE)

}
```
