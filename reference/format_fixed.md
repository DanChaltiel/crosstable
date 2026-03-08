# Format values for display

Format numeric values for display in tables and inline text.
`format_fixed()` supports fixed or scientific notation, percentages,
dates, and small-value formatting through `epsilon` and `zero_digits`.

## Usage

``` r
format_fixed(
  x,
  digits = 1,
  ...,
  scientific = 4,
  zero_digits = 1,
  percent = FALSE,
  date_format = NULL,
  epsilon = NULL,
  is_period = FALSE,
  only_round = "deprecated"
)
```

## Arguments

- x:

  A numeric vector to format. Can also be a `Date`, `POSIXt`, or
  `Period`.

- digits:

  number of decimals places in decimal notation or number of significant
  digits in scientific notation.

- ...:

  Not used.

- scientific:

  Order of magnitude beyond which numbers are printed in scientific
  notation. For example, `scientific = 4` prints values of order `10^±4`
  or larger in scientific notation. Can also be `TRUE` (always) or
  `FALSE` (never). Can be set globally with
  `crosstable_options(format_scientific = ...)`.

- zero_digits:

  Number of significant digits to use for non-zero values that would
  otherwise round to `0`. Use `NULL` to disable this behavior. Can be
  set globally with `crosstable_options(zero_digits = ...)`.

- percent:

  If `TRUE`, values are formatted as percentages.

- date_format:

  A format string passed to
  [`format()`](https://rdrr.io/r/base/format.html) for `Date` and
  `POSIXt` vectors. See [strptime](https://rdrr.io/r/base/strptime.html)
  for formats. Can be set globally with
  `crosstable_options(date_format = ...)`

- epsilon:

  Values smaller than `epsilon` are displayed as `"< [epsilon]"`. Can be
  set globally with `crosstable_options(format_epsilon = ...)`.

- is_period:

  Whether `x` is a period (a numeric value of seconds). Mainly for
  internal use.

- only_round:

  Deprecated, use `zero_digits=NULL` instead.

## Value

A character vector of formatted numbers

## Author

Dan Chaltiel

## Examples

``` r
# Basic formatting
x = c(1, 1.2, 99.999, pi, 0.00000012)
format_fixed(x, digits = 2)
#> [1] "1.00"    "1.20"    "100.00"  "3.14"    "1.2e-07"

# Prevent small values from rounding to zero
x = c(1.1, 0.1, 0.008280661)
format_fixed(x, digits = 1, zero_digits = 1)
#> [1] "1.1"   "0.1"   "0.008"

# Control when scientific notation is used
x = c(0.11e-04, 0.001, 0.01, 0.1, 1, 10)
format_fixed(x, scientific = 2, digits = 2)
#> [1] "1.1e-05" "1.0e-03" "1.0e-02" "0.10"    "1.00"    "10.00"  

# Force scientific or fixed notation
x = c(0.5, 0.01, 1e-07)
format_fixed(x, scientific = TRUE, digits = 1)
#> [1] "5e-01" "1e-02" "1e-07"
format_fixed(x, scientific = FALSE, digits = 4)
#> [1] "0.5000"    "0.0100"    "0.0000001"

# Percent formatting
x = c(0.5, 0.1001, 0.01)
format_fixed(x, percent = TRUE, digits = 1)
#> [1] "50.0%" "10.0%" "1.0%" 

# Threshold display for very small values
x = c(0.5, 0.1, 0.01)
format_fixed(x, epsilon = 0.05)
#> [1] "0.5"   "0.1"   "<0.05"
```
