# Default arguments for calculating and displaying tests in [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)

This is the starting point for refining the testing algorithm used in
crosstable. Users can provide their own functions for test.~.

## Usage

``` r
crosstable_test_args(
  test_summarize = test_summarize_auto,
  test_tabular = test_tabular_auto,
  test_correlation = test_correlation_auto,
  test_survival = test_survival_logrank,
  test_display = display_test,
  plim = 4,
  show_method = TRUE
)
```

## Arguments

- test_summarize:

  a function of two arguments (continuous variable and grouping
  variable), used to compare continuous variable. Must return a list of
  two components: `p.value` and `method`. See
  [`test_summarize_auto`](https://danchaltiel.github.io/crosstable/reference/test_summarize_auto.md)
  or
  [`test_summarize_linear_contrasts`](https://danchaltiel.github.io/crosstable/reference/test_summarize_linear_contrasts.md)
  for some examples of such functions.

- test_tabular:

  a function of two arguments (two categorical variables), used to test
  association between two categorical variables. Must return a list of
  two components: `p.value` and `method`. See
  [`test_tabular_auto`](https://danchaltiel.github.io/crosstable/reference/test_tabular_auto.md)
  for example.

- test_correlation:

  a function of three arguments (two continuous variables plus the
  correlation method), used to test association between two continuous
  variables. Like `cor.test`, it must return a list of at least
  `estimate`, `p.value`, and `method`, with also `conf.int` optionally.
  See
  [`test_correlation_auto`](https://danchaltiel.github.io/crosstable/reference/test_correlation_auto.md)
  for example.

- test_survival:

  a function of one argument (the formula `surv~by`), used to compare
  survival estimations. Must return a list of two components: `p.value`
  and `method`. See
  [`test_survival_logrank`](https://danchaltiel.github.io/crosstable/reference/test_survival_logrank.md)
  for example.

- test_display:

  function used to display the test result. See
  [`display_test`](https://danchaltiel.github.io/crosstable/reference/display_test.md).

- plim:

  number of digits for the p value.

- show_method:

  whether to display the test name (logical).

## Value

A list with test parameters

## See also

[`test_summarize_auto`](https://danchaltiel.github.io/crosstable/reference/test_summarize_auto.md),
[`test_tabular_auto`](https://danchaltiel.github.io/crosstable/reference/test_tabular_auto.md),
[`test_survival_logrank`](https://danchaltiel.github.io/crosstable/reference/test_survival_logrank.md),
[`test_summarize_linear_contrasts`](https://danchaltiel.github.io/crosstable/reference/test_summarize_linear_contrasts.md),
[`display_test`](https://danchaltiel.github.io/crosstable/reference/display_test.md)

## Author

Dan Chaltiel

## Examples

``` r
library(dplyr)
my_test_args=crosstable_test_args()
my_test_args$test_summarize = test_summarize_linear_contrasts
iris %>%
  mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
  crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
#> # A tibble: 4 × 9
#>   .id          label        variable   Q1          Q2    Q3    Q4    Q5    test 
#>   <chr>        <chr>        <chr>      <chr>       <chr> <chr> <chr> <chr> <chr>
#> 1 Petal.Length Petal.Length Min / Max  1.0 / 1.9   1.3 … 3.6 … 4.5 … 4.9 … "p v…
#> 2 Petal.Length Petal.Length Med [IQR]  1.4 [1.3;1… 1.6 … 4.3 … 5.0 … 5.6 … "p v…
#> 3 Petal.Length Petal.Length Mean (std) 1.4 (0.2)   2.2 … 4.4 … 5.1 … 5.7 … "p v…
#> 4 Petal.Length Petal.Length N (NA)     30 (0)      30 (… 30 (… 30 (… 30 (… "p v…
```
