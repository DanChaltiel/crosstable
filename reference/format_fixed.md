# Format numbers with the exact same number of decimals, including trailing zeros

Format numbers with the exact same number of decimals, including
trailing zeros

## Usage

``` r
format_fixed(
  x,
  digits = 1,
  zero_digits = 1,
  date_format = NULL,
  percent = FALSE,
  is_period = FALSE,
  scientific = getOption("crosstable_scientific_log", 4),
  epsilon = getOption("crosstable_format_epsilon", NULL),
  only_round = getOption("crosstable_only_round", FALSE),
  ...
)
```

## Arguments

- x:

  a numeric vector to format

- digits:

  number of decimals

- zero_digits:

  number of significant digits for values rounded to 0 (can be set to
  NULL to keep the original 0 value)

- date_format:

  if `x` is a vector of Date or POSIXt, the format to apply (see
  [strptime](https://rdrr.io/r/base/strptime.html) for formats)

- percent:

  if TRUE, format the values as percentages

- is_period:

  whether `x` is a period (a numeric value of seconds)

- scientific:

  the power of ten above/under which numbers will be displayed as
  scientific notation.

- epsilon:

  values less than `epsilon` are formatted as `"< [epsilon]"`

- only_round:

  if TRUE, `format_fixed` simply returns the rounded value. Can be set
  globally with `options("crosstable_only_round"=TRUE)`.

- ...:

  unused

## Value

a character vector of formatted numbers

## Author

Dan Chaltiel

## Examples

``` r
x = c(1, 1.2, 12.78749, pi, 0.00000012)
format_fixed(x, digits=3) #default zero_digits=1
#> [1] "1.000e+00" "1.200e+00" "1.279e+01" "3.142e+00" "1.200e-07"
format_fixed(x, digits=3, zero_digits=2)
#> [1] "1.000e+00" "1.200e+00" "1.279e+01" "3.142e+00" "1.200e-07"
format_fixed(x, digits=3, zero_digits=NULL)
#> [1] "1.000e+00" "1.200e+00" "1.279e+01" "3.142e+00" "1.200e-07"

x_sd = sd(iris$Sepal.Length/10000, na.rm=TRUE)
format_fixed(x_sd, dig=6)
#> [1] "8.280661e-05"
format_fixed(x_sd, dig=3, zero_digits=2) #default only_round=FALSE
#> [1] "8.281e-05"
format_fixed(x_sd, dig=3, zero_digits=2, only_round=TRUE)
#> [1] "8.281e-05"
options("crosstable_only_round"=TRUE)
format_fixed(x_sd, dig=3, zero_digits=2) #override default
#> [1] "8.281e-05"
options("crosstable_only_round"=NULL)

x2 = c(0.01, 0.1001, 0.500005, 0.00000012)
format_fixed(x2, scientific=0, dig=1) #everything abs>10^0 gets scientific
#> [1] "1.0e-02" "1.0e-01" "5.0e-01" "1.2e-07"
#last would be 0 so it is scientific. Try `zero_digits=NA` or `dig=7`
format_fixed(x2, scientific=FALSE, dig=6)
#> [1] "0.010000" "0.100100" "0.500005" "1e-07"   
format_fixed(x2, scientific=FALSE, percent=TRUE, dig=0)
#> [1] "1%"     "10%"    "50%"    "1e-05%"
format_fixed(x2, scientific=FALSE, eps=0.05)
#> [1] "<0.05" "0.1"   "0.5"   "<0.05"
```
