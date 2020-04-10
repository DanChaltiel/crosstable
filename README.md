
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crosstable

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/crosstable)](https://CRAN.R-project.org/package=crosstable)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/DanChaltiel/crosstable/branch/master/graph/badge.svg)](https://codecov.io/gh/DanChaltiel/crosstable?branch=master)
<!-- badges: end -->

Crosstable is a package centered on a single function, `crosstable`,
which easily computes descriptive statistics on datasets. It is using
the `tidyverse` syntax and is interfaced with the package `officer` to
create automatized reports.

## Installation

``` r
install.packages("devtools")
devtools::install_github("DanChaltiel/biostat2")
```

## Usage

``` r
library(crosstable)
library(dplyr)

#whole table with default parameters
crosstable(iris)
#>             .id        label   variable         value
#> 1  Sepal.Length Sepal.Length  Min / Max     4.3 / 7.9
#> 2  Sepal.Length Sepal.Length  Med [IQR] 5.8 [5.1;6.4]
#> 3  Sepal.Length Sepal.Length Mean (std)     5.8 (0.8)
#> 4  Sepal.Length Sepal.Length     N (NA)       150 (0)
#> 5   Sepal.Width  Sepal.Width  Min / Max     2.0 / 4.4
#> 6   Sepal.Width  Sepal.Width  Med [IQR] 3.0 [2.8;3.3]
#> 7   Sepal.Width  Sepal.Width Mean (std)     3.1 (0.4)
#> 8   Sepal.Width  Sepal.Width     N (NA)       150 (0)
#> 9  Petal.Length Petal.Length  Min / Max     1.0 / 6.9
#> 10 Petal.Length Petal.Length  Med [IQR] 4.3 [1.6;5.1]
#> 11 Petal.Length Petal.Length Mean (std)     3.8 (1.8)
#> 12 Petal.Length Petal.Length     N (NA)       150 (0)
#> 13  Petal.Width  Petal.Width  Min / Max     0.1 / 2.5
#> 14  Petal.Width  Petal.Width  Med [IQR] 1.3 [0.3;1.8]
#> 15  Petal.Width  Petal.Width Mean (std)     1.2 (0.8)
#> 16  Petal.Width  Petal.Width     N (NA)       150 (0)
#> 17      Species      Species     setosa   50 (33.33%)
#> 18      Species      Species versicolor   50 (33.33%)
#> 19      Species      Species  virginica   50 (33.33%)
```

``` r
#using mtcars2 for labels and cross_to_flextable for HTML formatting

#tidyselection, by, custom functions
library(tidyverse)
ct1 = crosstable(mtcars2, ends_with("t"), starts_with("c"), by=vs, 
                 funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75), digits=3)) %>% 
    cross_to_flextable
```

![crosstable1](man/figures/ct1.png)

``` r
#margin and totals
ct2 = crosstable(mtcars2, disp, vs, by=am, margin=c("row", "col"), total="both") %>%
    cross_to_flextable
```

![crosstable2](man/figures/ct2.png)

``` r
#predicate selection, correlation, testing
ct3 = crosstable(mtcars2, is.numeric, by=hp, test=TRUE)  %>%
    cross_to_flextable
```

![crosstable3](man/figures/ct3.png)

``` r
#lambda selection, effect calculation
ct4 = crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, effect=TRUE)  %>%
    cross_to_flextable
```

![crosstable4](man/figures/ct4.png)

``` r
#Survival data (using formula UI)
library(survival)
ct5 = crosstable(aml, Surv(time, status) ~ x,times=c(0,15,30,150), followup=TRUE)  %>%
    cross_to_flextable
```

![crosstable5](man/figures/ct5.png)

## Getting help

You can use the vignettes:

  - `vignette("crosstable")` for global use and parameterization  
  - `vignette("crosstable-selection")` for variable selection
  - `vignette("crosstable-report")` for reporting with `officer` or
    `Rmarkdown`

## Acknowledgement

`crosstable` is a rewrite of the awesome [`biostat2`
package](https://github.com/eusebe/biostat2) written by David Hajage.
The user interface is quite different but the concept is the same.

Thanks David\!
