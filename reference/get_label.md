# Get variable labels, or return a default value

Returns the `"label"` attribute of `x` when available. If no label is
found, `default` is returned instead.

## Usage

``` r
get_label(
  x,
  default = names(x),
  object = FALSE,
  recursive = FALSE,
  simplify = TRUE
)
```

## Arguments

- x:

  A labelled object. If `x` is a list or data frame, labels can be
  retrieved recursively from its elements.

- default:

  Value returned when no label is found. Defaults to `names(x)`.

- object:

  Logical. If `TRUE`, and `x` is a list or data frame, return the label
  of `x` itself instead of the labels of its children.

- recursive:

  Logical. If `TRUE`, recurse into nested lists. Otherwise, labels are
  only retrieved from the first level.

- simplify:

  Logical. If `TRUE` and `object = FALSE`, simplify the result to a
  character vector when possible. Otherwise, return a list.

## Value

A character vector when `simplify = TRUE`, or a list otherwise.

## Details

If `x` is a list or a data frame, `get_label()` returns the labels of
its elements by default. Use `object = TRUE` to retrieve the label of
the object itself instead.

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
