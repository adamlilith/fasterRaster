# BIOCLIM rasters

The BIOCLIM set of bioclimatic variables were created for modeling
species' geographic distributions (Booth et al. 2014). This function can
create the "standard" 19 set of variables, plus several more from an
"extended" set.

"Classic" set of BIOCLIM variables (Booth et al. 2014): The units
reported below assume that input rasters are in mm (precipitation) and
deg C (temperature), and that each raster represents a month (but other
time units are allowed, with corresponding changes to the temporal units
assumed below).

- BIO1: Mean annual temperature, calculated using monthly means (deg C)

- BIO2: Mean diurnal range across months (average of monthly difference
  between maximum and minimum temperature) (deg C)

- BIO3: Isothermality (100 \* BIO02 / BIO07; unit-less)

- BIO4: Temperature seasonality (standard deviation across months of
  average monthly temperature \* 100; deg C)

- BIO5: Maximum temperature of the warmest month (based on maximum
  temperature; deg C)

- BIO6: Minimum temperature of the coldest month (based on minimum
  temperature; deg C)

- BIO7: Range of annual temperature (BIO05 - BIO06; deg C)

- BIO8: Temperature of the wettest quarter (based on mean temperature;
  deg C)

- BIO9: Temperature of the driest quarter (based on mean temperature;
  deg C)

- BIO10: Temperature of the warmest quarter (based on mean temperature;
  deg C)

- BIO11: Temperature of the coldest quarter (based on mean temperature;
  deg C)

- BIO12: Total annual precipitation (mm)

- BIO13: Precipitation of the wettest month (mm)

- BIO14: Precipitation of the driest month (mm)

- BIO15: Precipitation seasonality (100 \* coefficient of variation;
  unit-less)

- BIO16: Precipitation of the wettest quarter (mm)

- BIO17: Precipitation of the driest quarter (mm)

- BIO18: Precipitation of the warmest quarter (based on mean
  temperature; mm)

- BIO19: Precipitation of the coldest quarter (based on mean
  temperature; mm)

"Extended" set of BIOCLIM variables (starts at 41 to avoid conflicts
with Kriticos et al. 2014):

- BIO41: Temperature of the quarter following the coldest quarter (based
  on mean temperature; deg C)

- BIO42: Temperature of the quarter following the warmest quarter (based
  on mean temperature; deg C)

- BIO43: Precipitation of the quarter following the coldest quarter
  (based on mean temperature; mm)

- BIO44: Precipitation of the quarter following the warmest quarter
  (based on mean temperature; mm)

- BIO45: Temperature of the quarter following the driest quarter (based
  on mean temperature; deg C)

- BIO46: Temperature of the quarter following the wettest quarter (based
  on mean temperature; deg C)

- BIO47: Precipitation of the quarter following the driest quarter
  (based on mean temperature; mm)

- BIO48: Precipitation of the quarter following the wettest quarter
  (based on mean temperature; mm)

- BIO49: Hottest month (based on maximum temperature)

- BIO50: Coldest month (based on minimum temperature)

- BIO51: Wettest month

- BIO52: Driest month

- BIO53: First month of the warmest quarter (based on mean temperature)

- BIO54: First month of the coldest quarter (based on mean temperature)

- BIO55: First month of the wettest quarter

- BIO56: First month of the driest quarter

- BIO57: The greatest decrease in temperature from one month to the next
  (deg C; always \>= 0)

- BIO58: The greatest increase in temperature from one month to the next
  (deg C; always \>= 0)

- BIO59: The greatest decrease in precipitation from one month to the
  next (mm; always \>= 0)

- BIO60: The greatest increase in precipitation from one month to the
  next (mm; always \>= 0)

By default, "quarter" refers to any consecutive run of three months, not
a financial quarter. A quarter can thus include
November-December-January, or December-January-February, for example.
However, the length of a quarter can be changed using the argument
`quarter`.

The variables are defined assuming that the input rasters represent
monthly values (12 rasters for min/max temperature and precipitation),
but you can also use sets of 52 rasters, representing one per week, in
which case "quarter" would be a successive run of 3 weeks. You could
also attempt 365 rasters, in which case a "quarter" would be a run of 3
successive days.

BIOCLIMs 41 through 44 are added here to capture the "shoulder" seasons
(spring and autumn) important in temperature regions. BIOCLIMs 45
through 48 are also included for consistency.

BIOCLIMs 49 through 60 are not bioclimatic variables per se, but useful
for assessing the properties of the variables that are defined based on
the "-est" month or quarter.

## Usage

``` r
# S4 method for class 'GRaster'
bioclims(
  ppt,
  tmin,
  tmax,
  tmean = NULL,
  bios = NULL,
  sample = TRUE,
  quarter = 3,
  pptDelta = 1,
  verbose = TRUE
)

# S4 method for class 'SpatRaster'
bioclims(
  ppt,
  tmin,
  tmax,
  tmean = NULL,
  bios = NULL,
  sample = TRUE,
  quarter = 3,
  pptDelta = 1,
  verbose = TRUE
)
```

## Arguments

- ppt:

  A multi-layered `GRaster` or `SpatRaster`, representing
  monthly/weekly/daily precipitation.

- tmin, tmax:

  A multi-layered `GRaster` or `SpatRaster`, representing
  monthly/weekly/daily minimum and maximum temperature.

- tmean:

  Either `NULL` (default), or a multi-layered `GRaster` or `SpatRaster`,
  representing monthly/weekly/daily average temperature. If `NULL`,
  `tmean` will be calculated internally from `tmin` and `tmax`.
  Providing these rasters thus saves time if you already have them on
  hand.

- bios:

  Any of:

  - Numeric values: Calculate these BIOCLIM variables. For example,
    `bios = c(1, 12)` calculates BIOCLIMs 1 and 12.

  - `NULL` (default): Calculate BIOCLIMs 1 through 19

  - `"*"`: Calculate all BIOCLIMs this function can calculate.

  - `"+"`: Calculate BIOCLIMs 41 onward.

  - Any combination of the above except `NULL` (e.g., `c(1, 12, "+")`).

- sample:

  Logical: If `TRUE` (default), BIO4 and 15 are calculated with the
  sample standard deviation. If `FALSE`, then the population standard
  deviation is used.

- quarter:

  Numeric: Length of a "quarter". BIOCLIM variables are typically
  calculated using monthly-averaged rasters (e.g., precipitation and
  temperature of January, February, etc.), in which case a "quarter" is
  3 months (so the default for `quarter` is 3). However, this function
  can accommodate any set of rasters representing a time series (e.g.,
  365 for daily rasters), in which case the user can decide what
  constitutes a "quarter" for calculation of the any BIOCLIMs that use
  "quarters" in their definitions.

- pptDelta:

  Numeric: Value to add to precipitation for calculation of BIO15
  (coefficient of variation of precipitation, times 100). Adding a small
  value avoids division by 0. The default is 1.

- verbose:

  Logical: If `TRUE` (default), display progress.

## Value

A `GRaster` with one or more layers.

## References

Booth, T.H., Nix, H.A., Busby, J.R., and Hutchinson, M.F. 2014. BIOCLIM:
The first species distribution modeling package, its early applications
and relevance to most current MaxEnt studies. *Diversity and
Distributions* 20:1-9
[doi:10.1111/ddi.12144](https://doi.org/10.1111/ddi.12144) .

Kriticos, D.J., Jarošik, V., and Otam N. 2014. Extending the suite of
BIOCLIM variables: A proposed registry system and case study using
principal components analysis. *Methods in Ecology and Evolution*
5:956-960
[doi:10.1111/2041-210X.12244](https://doi.org/10.1111/2041-210X.12244) .

## Examples

``` r
if (grassStarted()) {

# Setup
library(terra)

# Load rasters with precipitation and min/max temperature
madPpt <- fastData("madPpt")
madTmin <- fastData("madTmin")
madTmax <- fastData("madTmax")

### Classic and extended BIOCLIMs from SpatRasters
bcSR <- bioclims(madPpt, madTmin, madTmax, bios = "*")
bcSR

### BIOCLIMs from GRasters
ppt <- fast(madPpt)
tmin <- fast(madTmin)
tmax <- fast(madTmax)

# For small rasters, takes longer to run compared to SpatRaster version:
bc <- bioclims(ppt, tmin, tmax, bios = c(1, 5, 12))
bc
plot(bc)

}
```
