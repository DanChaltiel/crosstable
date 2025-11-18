# Summary functions

Summary functions to use with
[`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
or anywhere else.

## Usage

``` r
meansd(x, na.rm = TRUE, dig = 2, ...)

meanCI(x, na.rm = TRUE, dig = 2, level = 0.95, format = TRUE, ...)

mediqr(x, na.rm = TRUE, dig = 2, format = TRUE, ...)

minmax(x, na.rm = TRUE, dig = 2, ...)

nna(x)
```

## Arguments

- x:

  a numeric vector

- na.rm:

  `TRUE` as default

- dig:

  number of digits

- ...:

  params to pass on to
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md):

  - `zero_digits` (default=`1`): the number of significant digits for
    values rounded to 0 (set to NULL to keep the original 0 value)

  - `only_round` (default=`FALSE`): use
    [`round()`](https://rdrr.io/r/base/Round.html) instead of
    [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)

- level:

  the confidence level required

- format:

  a sugar argument. If FALSE, the function returns a list instead of a
  formatted string

## Value

a character vector

## Functions

- `meansd()`: returns mean and std error

- `meanCI()`: returns mean and confidence interval

- `mediqr()`: returns median and IQR

- `minmax()`: returns minimum and maximum

- `nna()`: returns number of observations and number of missing values

## Fixed format

These functions use
[`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)
which allows to have trailing zeros after rounded values. In the case
when the output of rounded values is zero, the use of the `zero_digits`
argument allows to keep some significant digits for this specific case
only.

## See also

[`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)

## Author

Dan Chaltiel, David Hajage

## Examples

``` r
meansd(iris$Sepal.Length, dig=3)
#> [1] "5.843 (0.828)"
meanCI(iris$Sepal.Length)
#> [1] "5.84 [5.71;5.98]"
minmax(iris$Sepal.Length, dig=3)
#> [1] "4.300 / 7.900"
mediqr(iris$Sepal.Length, dig=3)
#> [1] "5.800 [5.100;6.400]"
nna(iris$Sepal.Length)
#> [1] "150 (0)"

#arguments for format_fixed
x = iris$Sepal.Length/10000 #closer to zero

meansd(x, dig=3)
#> [1] "0.001 (8.281e-05)"
meansd(x, dig=3, zero_digits=NULL) #or NA
#> [1] "0.001 (8.281e-05)"
meansd(x, dig=3, only_round=TRUE)
#> [1] "0.001 (8.281e-05)"
options("crosstable_only_round"=TRUE)
meansd(x, dig=3, zero_digits=2)
#> [1] "0.001 (8.281e-05)"
options("crosstable_only_round"=NULL)
meanCI(mtcars2$x_date)
#> Warning: Unknown or uninitialised column: `x_date`.
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> [1] "NA [NA;NA]"

#dates
x = as.POSIXct(mtcars$qsec*3600*24 , origin="2010-01-01")
meansd(x)
#> [1] "2010-01-18 20:22:12 (1.79 days)"
minmax(x, date_format="%d/%m/%Y")
#> [1] "15/01/2010 - 23/01/2010"
```
