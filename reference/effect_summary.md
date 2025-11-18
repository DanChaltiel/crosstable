# Effect measure for association between one continuous and one categorical variable

User can either use or extend these functions to configure effect
calculation.

## Usage

``` r
diff_mean_auto(x, by, conf_level = 0.95, R = 500)

diff_mean_boot(x, by, conf_level = 0.95, R = 500)

diff_median_boot(x, by, conf_level = 0.95, R = 500)

diff_mean_student(x, by, conf_level = 0.95)
```

## Arguments

- x:

  numeric vector

- by:

  categorical vector (of exactly 2 unique levels)

- conf_level:

  confidence interval level

- R:

  number of bootstrap replication

## Value

A list with five components: effect, ci, effect.name, effect.type, and
conf_level

## Functions

- `diff_mean_auto()`: (**Default**) calculate a specific "difference in
  means" effect based on normality (Shapiro or Anderson test) and
  variance homogeneity (Bartlett test)

- `diff_mean_boot()`: calculate a "difference in means" effect with a
  bootstrapped CI using standard deviation

- `diff_median_boot()`: calculate a "difference in medians" effect with
  a bootstrapped CI using quantiles#'

- `diff_mean_student()`: calculate a "difference in means" effect using
  `t.test` confidence intervals

## See also

[`crosstable_effect_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_effect_args.md)

## Author

Dan Chaltiel, David Hajage
