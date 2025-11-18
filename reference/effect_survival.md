# Effect measure for association between one censored variable and one categorical variable

Effect measure for association between one censored variable and one
categorical variable

## Usage

``` r
effect_survival_coxph(x, by, conf_level = 0.95)
```

## Arguments

- x:

  survival vector (made using
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html))

- by:

  categorical vector (of exactly 2 unique levels)

- conf_level:

  confidence interval level

## Value

a list with two components: p.value and method

## Author

Dan Chaltiel, David Hajage
