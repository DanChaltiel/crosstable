# Default arguments for calculating and displaying effects in [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)

This helper function provides default parameters for defining how the
effect sizes should be computed. It belongs to the `effect_args`
argument of the
[`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
function. See
[effect_summary](https://danchaltiel.github.io/crosstable/reference/effect_summary.md),
[effect_tabular](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md),
and
[effect_survival](https://danchaltiel.github.io/crosstable/reference/effect_survival.md)
for more insight.

## Usage

``` r
crosstable_effect_args(
  effect_summarize = diff_mean_auto,
  effect_tabular = effect_odds_ratio,
  effect_survival = effect_survival_coxph,
  effect_display = display_effect,
  conf_level = 0.95,
  digits = 2
)
```

## Arguments

- effect_summarize:

  a function of three arguments (continuous variable, grouping variable
  and conf_level), used to compare continuous variable. Returns a list
  of five components: `effect` (the effect value(s)), `ci` (the matrix
  of confidence interval(s)), `effect.name` (the interpretation(s) of
  the effect value(s)), `effect.type` (the description of the measure
  used) and `conf_level` (the confidence interval level). Users can use
  [`diff_mean_auto()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md),
  [`diff_mean_student()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md),
  [`diff_mean_boot()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md),
  or
  [`diff_median()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md),
  or their custom own function.

- effect_tabular:

  a function of three arguments (two categorical variables and
  conf_level) used to measure the associations between two factors.
  Returns a list of five components: `effect` (the effect value(s)),
  `ci` (the matrix of confidence interval(s)), `effect.name` (the
  interpretation(s) of the effect value(s)), `effect.type` (the
  description of the measure used) and `conf_level` (the confidence
  interval level).Users can use
  [`effect_odds_ratio()`](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md),
  [`effect_relative_risk()`](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md),
  or
  [`effect_risk_difference()`](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md),
  or their custom own function.

- effect_survival:

  a function of two argument (a formula and conf_level), used to measure
  the association between a censored and a factor. Returns the same
  components as created by `effect_summarize`.Users can use
  [`effect_survival_coxph()`](https://danchaltiel.github.io/crosstable/reference/effect_survival.md)
  or their custom own function.

- effect_display:

  a function to format the effect. See
  [`display_effect()`](https://danchaltiel.github.io/crosstable/reference/display_effect.md).

- conf_level:

  the desired confidence interval level

- digits:

  the decimal places

## Value

A list with effect parameters

## Author

Dan Chaltiel
