# Effect measure for association between two categorical variables

User can either use or extend these functions to configure effect
calculation.

## Usage

``` r
effect_odds_ratio(x, by, conf_level = 0.95)

effect_relative_risk(x, by, conf_level = 0.95)

effect_risk_difference(x, by, conf_level = 0.95)
```

## Arguments

- x:

  categorical vector (character, factor, ...)

- by:

  categorical vector (of exactly 2 unique levels)

- conf_level:

  confidence interval level

## Value

A list with five components: effect, ci, effect.name, effect.type, and
conf_level

## Functions

- `effect_odds_ratio()`: (**Default**) calculate the odds ratio

- `effect_relative_risk()`: calculate the relative risk

- `effect_risk_difference()`: calculate the risk difference

## See also

[`crosstable_effect_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_effect_args.md)

## Author

Dan Chaltiel, David Hajage
