# Set the "label" attribute of an object

Set the "label" attribute of an object

Copy the label from one variable to another

## Usage

``` r
set_label(x, value, object = FALSE)

copy_label_from(x, from)
```

## Arguments

- x:

  the variable to label

- value:

  value of the label. If `x` is a list/data.frame, the labels will all
  be set recursively. If `value` is a function, it will be applied to
  the current labels of `x`.

- object:

  if `x` is a list/data.frame, `object=TRUE` will force setting the
  labels of the object instead of the children

- from:

  the variable whose label must be copied

## Value

An object of the same type as `x`, with labels

## See also

[`get_label()`](https://danchaltiel.github.io/crosstable/reference/get_label.md),
[`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md),
[`remove_label()`](https://danchaltiel.github.io/crosstable/reference/remove_labels.md)

## Author

Dan Chaltiel

## Examples

``` r
library(dplyr)
mtcars %>%
   mutate(mpg2=set_label(mpg, "Miles per gallon"),
          mpg3=mpg %>% copy_label_from(mpg2)) %>%
   crosstable(c(mpg, mpg2, mpg3))
#> # A tibble: 12 × 4
#>    .id   label            variable   value           
#>    <chr> <chr>            <chr>      <chr>           
#>  1 mpg   mpg              Min / Max  10.4 / 33.9     
#>  2 mpg   mpg              Med [IQR]  19.2 [15.4;22.8]
#>  3 mpg   mpg              Mean (std) 20.1 (6.0)      
#>  4 mpg   mpg              N (NA)     32 (0)          
#>  5 mpg2  Miles per gallon Min / Max  10.4 / 33.9     
#>  6 mpg2  Miles per gallon Med [IQR]  19.2 [15.4;22.8]
#>  7 mpg2  Miles per gallon Mean (std) 20.1 (6.0)      
#>  8 mpg2  Miles per gallon N (NA)     32 (0)          
#>  9 mpg3  Miles per gallon Min / Max  10.4 / 33.9     
#> 10 mpg3  Miles per gallon Med [IQR]  19.2 [15.4;22.8]
#> 11 mpg3  Miles per gallon Mean (std) 20.1 (6.0)      
#> 12 mpg3  Miles per gallon N (NA)     32 (0)          
mtcars %>%
   copy_label_from(mtcars2) %>%
   crosstable(c(mpg, vs))
#> # A tibble: 6 × 4
#>   .id   label             variable   value           
#>   <chr> <chr>             <chr>      <chr>           
#> 1 mpg   Miles/(US) gallon Min / Max  10.4 / 33.9     
#> 2 mpg   Miles/(US) gallon Med [IQR]  19.2 [15.4;22.8]
#> 3 mpg   Miles/(US) gallon Mean (std) 20.1 (6.0)      
#> 4 mpg   Miles/(US) gallon N (NA)     32 (0)          
#> 5 vs    Engine            0          18 (56.25%)     
#> 6 vs    Engine            1          14 (43.75%)     
mtcars2 %>% set_label(toupper) %>% get_label()
#>                     model                       mpg                       cyl 
#>                   "MODEL"       "MILES/(US) GALLON"     "NUMBER OF CYLINDERS" 
#>                      disp                        hp                      drat 
#>   "DISPLACEMENT (CU.IN.)"        "GROSS HORSEPOWER"         "REAR AXLE RATIO" 
#>                        wt                      qsec                        vs 
#>       "WEIGHT (1000 LBS)"           "1/4 MILE TIME"                  "ENGINE" 
#>                        am                      gear                      carb 
#>            "TRANSMISSION" "NUMBER OF FORWARD GEARS"   "NUMBER OF CARBURETORS" 
#>                   hp_date                qsec_posix 
#>      "SOME NONSENSE DATE"               "DATE+TIME" 
```
