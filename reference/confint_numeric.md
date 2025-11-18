# Confidence interval of a numeric vector

Not an S3 method, which might have conflicted with
[stats::confint](https://rdrr.io/r/stats/confint.html).

## Usage

``` r
confint_numeric(object, level = 0.95, B = 0)
```

## Arguments

- object:

  a vector, numeric or equivalent (date, logical...)

- level:

  the confidence level required

- B:

  if \>0, the number of bootstraps

## Value

the vector \[conf_inf, conf_sup\]

## Author

Dan Chaltiel

## Examples

``` r
confint_numeric(iris$Sepal.Length)
#>    2.5 %   97.5 % 
#> 5.710818 5.975849 
confint_numeric(mtcars2$hp_date)
#>        2.5 %       97.5 % 
#> "2010-05-03" "2010-06-20" 
confint_numeric(mtcars2$hp_date, level=0.99)
#>        0.5 %       99.5 % 
#> "2010-04-26" "2010-06-27" 
```
