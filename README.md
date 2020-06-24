
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crosstable

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/crosstable)](https://CRAN.R-project.org/package=crosstable)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Build
Status](https://travis-ci.org/DanChaltiel/crosstable.svg?branch=master)](https://travis-ci.org/DanChaltiel/crosstable)
[![Codecov test
coverage](https://codecov.io/gh/DanChaltiel/crosstable/branch/master/graph/badge.svg)](https://codecov.io/gh/DanChaltiel/crosstable?branch=master)
<!-- badges: end -->

Crosstable is a package centered on a single function, `crosstable`,
which easily computes descriptive statistics on datasets. It can use the
`tidyverse` syntax and is interfaced with the package `officer` to
create automatized reports.

## Installation

``` r
install.packages("devtools")
devtools::install_github("DanChaltiel/crosstable", build_vignettes=TRUE)
```

In case of any installation problem, try the solutions proposed in [this
article](file:///F:/GITHUB/crosstable/docs/articles/crosstable-install.html)
or fill an [Issue](https://github.com/DanChaltiel/crosstable/issues).

## Getting help

You can use the vignettes:

  - `vignette("crosstable")` for global use and parameterization
  - `vignette("crosstable-selection")` for variable selection
  - `vignette("crosstable-report")` for reporting with `officer` or
    `Rmarkdown`

These vignettes are reproduced on the [documentation
website](https://danchaltiel.github.io/crosstable).

## Overview

``` r
library(crosstable)
library(dplyr)
ct1 = crosstable(mtcars2, disp, vs, by=am, margin=c("row", "col"), total="both") %>%
    as_flextable()
```

<p align="center">

<img src="man/figures/ct1.png" alt="crosstable1">

</p>

With a few arguments, you can select some columns to describe (`disp,
vs`), define a grouping variable (`by=am`), set the percentage
calculation (`margin`) and ask for totals (`total`).

`mtcars2` is a dataset which has labels, so they are displayed instead
of the variable name (see
[here](https://danchaltiel.github.io/crosstable/articles/crosstable.html#dataset-modified-mtcars)
for how to add some).

`crosstable` is returning a plain R object (`data.frame`), but using
`as_flextable` allows to output a beautiful HTML table that can be
exported to Word with a few more lines of code (see
[here](https://danchaltiel.github.io/crosstable/articles/crosstable-report.html)
to learn how).

Here is another example:

``` r
ct2 = crosstable(mtcars2, ends_with("t"), starts_with("c"), by=vs, 
                 funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75), digits=3)) %>% 
    as_flextable(keep_id=TRUE)
```

<p align="center">

<img src="man/figures/ct2.png" alt="crosstable2" height="400">

</p>

Here, the variables were selected using `tidyselect` helpers and the
summary functions were specified.

## More

There are lots of other features you can learn about on the website
<https://danchaltiel.github.io/crosstable>, for instance:

  - variable selection with functions, e.g. `is.numeric`
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable-selection.html#select-with-predicate-functions))
  - variable selection with mutating, e.g. `sqrt(mpg)` or `Surv(time,
    event)`, using a formula interface
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable-selection.html#select-with-a-formula))
  - automatic computation of statistical tests
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable.html#tests))
    and of effect sizes
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable.html#effects))
  - description of correlation and of survival data
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable.html#miscellaneous-1))
  - auto-reporting with `officer`
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable-report.html#create-reports-with-officer))
    or with `Rmarkdown`
    ([link](https://danchaltiel.github.io/crosstable/articles/crosstable-report.html#create-reports-with-rmarkdown))

## Acknowledgement

`crosstable` is a rewrite of the awesome package
[`biostat2`](https://github.com/eusebe/biostat2) written by David
Hajage. The user interface is quite different but the concept is the
same.

Thanks David\!
