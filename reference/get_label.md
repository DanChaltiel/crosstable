# Get label if wanted and available, or default (name) otherwise

Get label if wanted and available, or default (name) otherwise

## Usage

``` r
get_label(x, default = names(x), object = FALSE, simplify = TRUE)
```

## Arguments

- x:

  labelled object. If `x` is a list/data.frame, `get_label()` will
  return the labels of all children recursively

- default:

  value returned if there is no label. Default to `names(x)`.

- object:

  if `x` is a list/data.frame, `object=TRUE` will force getting the
  labels of the object instead of the children

- simplify:

  if `x` is a list and `object=FALSE`, simplify the result to a vector

## Value

A character vector if `simplify==TRUE`, a list otherwise

## See also

[`set_label()`](https://danchaltiel.github.io/crosstable/reference/set_label.md),
[`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md),
[`remove_label()`](https://danchaltiel.github.io/crosstable/reference/remove_labels.md),
[`Hmisc::label()`](https://rdrr.io/pkg/Hmisc/man/label.html),
[`expss::var_lab()`](https://rdrr.io/pkg/expss/man/var_lab.html)

## Author

Dan Chaltiel

## Examples

``` r
xx=mtcars2 %>%
  set_label("The mtcars2 dataset", object=TRUE)
xx$cyl=remove_label(xx$cyl)

#vectors
get_label(xx$mpg) #label="Miles/(US) gallon"
#> [1] "Miles/(US) gallon"
get_label(xx$cyl) #default to NULL (since names(xx$cyl)==NULL)
#> NULL
get_label(xx$cyl, default="Default value")
#> [1] "Default value"

#data.frames
get_label(xx)
#>                     model                       mpg                       cyl 
#>                   "Model"       "Miles/(US) gallon"                     "cyl" 
#>                      disp                        hp                      drat 
#>   "Displacement (cu.in.)"        "Gross horsepower"         "Rear axle ratio" 
#>                        wt                      qsec                        vs 
#>       "Weight (1000 lbs)"           "1/4 mile time"                  "Engine" 
#>                        am                      gear                      carb 
#>            "Transmission" "Number of forward gears"   "Number of carburetors" 
#>                   hp_date                qsec_posix 
#>      "Some nonsense date"               "Date+time" 
get_label(xx, object=TRUE)
#> [1] "The mtcars2 dataset"
data.frame(name=names(xx), label=get_label(xx, default=NA)) #cyl is NA
#>                  name                   label
#> model           model                   Model
#> mpg               mpg       Miles/(US) gallon
#> cyl               cyl                    <NA>
#> disp             disp   Displacement (cu.in.)
#> hp                 hp        Gross horsepower
#> drat             drat         Rear axle ratio
#> wt                 wt       Weight (1000 lbs)
#> qsec             qsec           1/4 mile time
#> vs                 vs                  Engine
#> am                 am            Transmission
#> gear             gear Number of forward gears
#> carb             carb   Number of carburetors
#> hp_date       hp_date      Some nonsense date
#> qsec_posix qsec_posix               Date+time

#lists
get_label(list(xx$cyl, xx$mpg))          #cyl is NA
#> [1] NA                  "Miles/(US) gallon"
get_label(list(foo=xx$cyl, bar=xx$mpg))  #default to names
#>                 foo                 bar 
#>               "foo" "Miles/(US) gallon" 
get_label(list(foo=xx$cyl, bar=xx$mpg), default="Default value")
#>                 foo                 bar 
#>     "Default value" "Miles/(US) gallon" 
```
