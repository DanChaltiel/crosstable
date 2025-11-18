# Test for linear trend across ordered factor with contrasts

Test for linear trend across ordered factor with contrasts

## Usage

``` r
test_summarize_linear_contrasts(x, y)
```

## Arguments

- x:

  vector

- y:

  ordered factor

## Value

a list with two components: p.value and method

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
