
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
#> Registered S3 methods overwritten by 'expss':
#>   method                 from 
#>   [.labelled             Hmisc
#>   as.data.frame.labelled base 
#>   print.labelled         Hmisc
#> Registered S3 method overwritten by 'gdata':
#>   method         from     
#>   reorder.factor DescTools

#whole table
crosstable(iris)
#>             .id        label   variable          value
#> 1  Sepal.Length Sepal.Length  Min / Max      4.3 / 7.9
#> 2  Sepal.Length Sepal.Length  Med [IQR]  5.8 [5.1;6.4]
#> 3  Sepal.Length Sepal.Length Mean (std)    5.84 (0.83)
#> 4  Sepal.Length Sepal.Length     N (NA)        150 (0)
#> 5   Sepal.Width  Sepal.Width  Min / Max        2 / 4.4
#> 6   Sepal.Width  Sepal.Width  Med [IQR]    3 [2.8;3.3]
#> 7   Sepal.Width  Sepal.Width Mean (std)    3.06 (0.44)
#> 8   Sepal.Width  Sepal.Width     N (NA)        150 (0)
#> 9  Petal.Length Petal.Length  Min / Max        1 / 6.9
#> 10 Petal.Length Petal.Length  Med [IQR] 4.35 [1.6;5.1]
#> 11 Petal.Length Petal.Length Mean (std)    3.76 (1.77)
#> 12 Petal.Length Petal.Length     N (NA)        150 (0)
#> 13  Petal.Width  Petal.Width  Min / Max      0.1 / 2.5
#> 14  Petal.Width  Petal.Width  Med [IQR]  1.3 [0.3;1.8]
#> 15  Petal.Width  Petal.Width Mean (std)     1.2 (0.76)
#> 16  Petal.Width  Petal.Width     N (NA)        150 (0)
#> 17      Species      Species     setosa    50 (33.33%)
#> 18      Species      Species versicolor    50 (33.33%)
#> 19      Species      Species  virginica    50 (33.33%)

#tidyselection, custom functions
library(tidyverse)
#> -- Attaching packages ------------------------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.0     v purrr   0.3.3
#> v tibble  2.1.3     v stringr 1.4.0
#> v tidyr   1.0.2     v forcats 0.5.0
#> v readr   1.3.1
#> -- Conflicts ---------------------------------------------------------------- tidyverse_conflicts() --
#> x purrr::compact() masks crosstable::compact()
#> x purrr::compose() masks flextable::compose()
#> x tidyr::expand()  masks crosstable::expand()
#> x dplyr::filter()  masks stats::filter()
#> x dplyr::lag()     masks stats::lag()
crosstable(mtcars2, ends_with("t"), starts_with("c"), by=vs, 
           funs=c(mean, quantile), funs_arg = list(probs=c(.25,.75)))
#>     .id                 label     variable         straight          vshaped
#> 1  drat       Rear axle ratio         mean 3.85928571428571 3.39222222222222
#> 2  drat       Rear axle ratio quantile 25%           3.7175             3.07
#> 3  drat       Rear axle ratio quantile 75%             4.08           3.7025
#> 4    wt     Weight (1000 lbs)         mean 2.61128571428571 3.68855555555556
#> 5    wt     Weight (1000 lbs) quantile 25%          2.00125          3.23625
#> 6    wt     Weight (1000 lbs) quantile 75%          3.20875          3.84375
#> 7   cyl   Number of cylinders            4      10 (90.91%)        1 (9.09%)
#> 8   cyl   Number of cylinders            6       4 (57.14%)       3 (42.86%)
#> 9   cyl   Number of cylinders            8           0 (0%)        14 (100%)
#> 10 carb Number of carburetors         mean 1.78571428571429 3.61111111111111
#> 11 carb Number of carburetors quantile 25%                1             2.25
#> 12 carb Number of carburetors quantile 75%                2                4

#margin and totals
crosstable(mtcars2, disp, vs, by=am, 
           margin=c("row", "col"), total = "both")
#>    .id                 label   variable                 auto
#> 1 disp Displacement (cu.in.)  Min / Max          120.1 / 472
#> 2 disp Displacement (cu.in.)  Med [IQR]    275.8 [196.3;360]
#> 3 disp Displacement (cu.in.) Mean (std)      290.38 (110.17)
#> 4 disp Displacement (cu.in.)     N (NA)               19 (0)
#> 5   vs                Engine   straight     7 (50% / 36.84%)
#> 6   vs                Engine    vshaped 12 (66.67% / 63.16%)
#> 7   vs                Engine      Total          19 (59.38%)
#>                manual              Total
#> 1          71.1 / 351         71.1 / 472
#> 2      120.3 [79;160] 196.3 [120.83;326]
#> 3       143.53 (87.2)    230.72 (123.94)
#> 4              13 (0)             32 (0)
#> 5    7 (50% / 53.85%)        14 (43.75%)
#> 6 6 (33.33% / 46.15%)        18 (56.25%)
#> 7         13 (40.62%)          32 (100%)

#predicate selection, correlation, testing
crosstable(mtcars2, is.numeric, by=hp, test=TRUE)
#>    .id                 label variable            Gross horsepower
#> 1  mpg     Miles/(US) gallon  pearson -0.78 \n95%CI [-0.89;-0.59]
#> 2 disp Displacement (cu.in.)  pearson    0.79 \n95%CI [0.61;0.89]
#> 3 drat       Rear axle ratio  pearson -0.45 \n95%CI [-0.69;-0.12]
#> 4   wt     Weight (1000 lbs)  pearson     0.66 \n95%CI [0.4;0.82]
#> 5 qsec         1/4 mile time  pearson -0.71 \n95%CI [-0.85;-0.48]
#> 6 carb Number of carburetors  pearson    0.75 \n95%CI [0.54;0.87]
#>                                              test
#> 1 <0.0001\n(Pearson's product-moment correlation)
#> 2 <0.0001\n(Pearson's product-moment correlation)
#> 3  0.0100\n(Pearson's product-moment correlation)
#> 4 <0.0001\n(Pearson's product-moment correlation)
#> 5 <0.0001\n(Pearson's product-moment correlation)
#> 6 <0.0001\n(Pearson's product-moment correlation)

#lambda selection, effect calculation
crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, effect=TRUE)
#>    .id                 label   variable              straight
#> 1 disp Displacement (cu.in.)  Min / Max            71.1 / 258
#> 2 disp Displacement (cu.in.)  Med [IQR] 120.55 [83.03;162.38]
#> 3 disp Displacement (cu.in.) Mean (std)        132.46 (56.89)
#> 4 disp Displacement (cu.in.)     N (NA)                14 (0)
#> 5   hp      Gross horsepower  Min / Max              52 / 123
#> 6   hp      Gross horsepower  Med [IQR]        96 [66;109.75]
#> 7   hp      Gross horsepower Mean (std)         91.36 (24.42)
#> 8   hp      Gross horsepower     N (NA)                14 (0)
#>               vshaped
#> 1         120.3 / 472
#> 2     311 [275.8;360]
#> 3     307.15 (106.77)
#> 4              18 (0)
#> 5            91 / 335
#> 6 180 [156.25;226.25]
#> 7      189.72 (60.28)
#> 8              18 (0)
#>                                                                                        effect
#> 1 Difference in means (Welch CI) (straight minus vshaped): -174.69\nCI95%[-235.02 to -114.36]
#> 2 Difference in means (Welch CI) (straight minus vshaped): -174.69\nCI95%[-235.02 to -114.36]
#> 3 Difference in means (Welch CI) (straight minus vshaped): -174.69\nCI95%[-235.02 to -114.36]
#> 4 Difference in means (Welch CI) (straight minus vshaped): -174.69\nCI95%[-235.02 to -114.36]
#> 5   Difference in means (Welch CI) (straight minus vshaped): -98.37\nCI95%[-130.67 to -66.06]
#> 6   Difference in means (Welch CI) (straight minus vshaped): -98.37\nCI95%[-130.67 to -66.06]
#> 7   Difference in means (Welch CI) (straight minus vshaped): -98.37\nCI95%[-130.67 to -66.06]
#> 8   Difference in means (Welch CI) (straight minus vshaped): -98.37\nCI95%[-130.67 to -66.06]


#Survival data (using formula UI)
library(survival)
crosstable(aml, Surv(time, status) ~ x,times=c(0,15,30,150), followup=TRUE)
#>                  .id              label                     variable
#> 1 Surv(time, status) Surv(time, status)                          t=0
#> 2 Surv(time, status) Surv(time, status)                         t=15
#> 3 Surv(time, status) Surv(time, status)                         t=30
#> 4 Surv(time, status) Surv(time, status)                        t=150
#> 5 Surv(time, status) Surv(time, status) Median follow up [min ; max]
#> 6 Surv(time, status) Surv(time, status)              Median survival
#>       Maintained Nonmaintained
#> 1       1 (0/11)      1 (0/12)
#> 2     0.82 (2/8)    0.58 (5/7)
#> 3     0.61 (2/5)    0.29 (3/4)
#> 4     0.18 (3/1)       0 (3/0)
#> 5 103 [13 ; 161]  NA [16 ; 45]
#> 6             31            23
```

## Getting help

You can use the vignettes: + `vignette("crosstable")` for global use and
parameterization  
\+ `vignette("crosstable-selection")` for variable selection +
`vignette("crosstable-officer")` for interfacing with the package
`officer`

## Acknowledgement

`crosstable` is a rewrite of the awesome [`biostat2`
package](https://github.com/eusebe/biostat2) written by David Hajage.
The user interface is quite different but the concept is the same.

Thanks David\!
