# Summarize a numeric vector

Summarize a numeric vector with min, max, mean, sd, median, IQR, n and
missings.

## Usage

``` r
cross_summary(x, dig = 1, ...)
```

## Arguments

- x:

  a numeric vector

- dig:

  number of digits

- ...:

  params to pass on to
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md):
  `zero_digits` and `only_round`

## Value

a list of named functions

## Author

Dan Chaltiel, David Hajage

## Examples

``` r
cross_summary(iris$Sepal.Length)
#>       Min / Max       Med [IQR]      Mean (std)          N (NA) 
#>     "4.3 / 7.9" "5.8 [5.1;6.4]"     "5.8 (0.8)"       "150 (0)" 
cross_summary(iris$Petal.Width, dig=3)
#>             Min / Max             Med [IQR]            Mean (std) 
#>       "0.100 / 2.500" "1.300 [0.300;1.800]"       "1.199 (0.762)" 
#>                N (NA) 
#>             "150 (0)" 
cross_summary(mtcars2$hp_date)
#>                            Min / Max                            Med [IQR] 
#>            "2010-02-22 - 2010-12-02" "2010-05-04 [2010-04-06;2010-06-30]" 
#>                           Mean (std)                               N (NA) 
#>            "2010-05-27 (2.3 months)"                             "32 (0)" 
cross_summary(mtcars2$qsec_posix, date_format="%d/%m %H:%M")
#>                               Min / Max                               Med [IQR] 
#>             "15/01 12:00 - 23/01 21:36" "18/01 17:02 [17/01 20:52;19/01 21:36]" 
#>                              Mean (std)                                  N (NA) 
#>                "18/01 20:22 (1.8 days)"                                "32 (0)" 
```
