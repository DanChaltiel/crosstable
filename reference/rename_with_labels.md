# Rename every column of a dataframe with its label

Rename every column of a dataframe with its label

## Usage

``` r
rename_with_labels(df, cols = everything(), except = NULL)
```

## Arguments

- df:

  a data.frame

- cols:

  \<[`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)\>
  columns to be renamed.

- except:

  \<[`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)\>
  columns not be renamed. Deprecated in favor of `cols`.

## Value

A dataframe which names are copied from the label attribute

## Examples

``` r
rename_with_labels(mtcars2[,1:5], cols=1:4) %>% names()
#> [1] "Model"                 "Miles/(US) gallon"     "Number of cylinders"  
#> [4] "Displacement (cu.in.)" "hp"                   
rename_with_labels(iris2, cols=-c(Sepal.Length, Sepal.Width)) %>% names()
#> [1] "Sepal.Length"    "Sepal.Width"     "Length of Petal" "Width of Petal" 
#> [5] "Specie"         
```
