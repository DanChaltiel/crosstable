# Remove all label attributes.

Use `remove_labels()` to remove the label from an object or to
recursively remove all the labels from a collection of objects (such as
a list or a data.frame).  
  
This can be useful with functions reacting badly to labelled objects.

## Usage

``` r
remove_labels(x)
```

## Arguments

- x:

  object to unlabel

## Value

An object of the same type as `x`, with no labels

## See also

[get_label](https://danchaltiel.github.io/crosstable/reference/get_label.md),
[set_label](https://danchaltiel.github.io/crosstable/reference/set_label.md),
[import_labels](https://danchaltiel.github.io/crosstable/reference/import_labels.md),
[expss::unlab](https://rdrr.io/pkg/expss/man/unlab.html)

## Author

Dan Chaltiel

## Examples

``` r
mtcars2 %>% remove_labels %>% crosstable(mpg) #no label
#> # A tibble: 4 × 4
#>   .id   label variable   value           
#>   <chr> <chr> <chr>      <chr>           
#> 1 mpg   mpg   Min / Max  10.4 / 33.9     
#> 2 mpg   mpg   Med [IQR]  19.2 [15.4;22.8]
#> 3 mpg   mpg   Mean (std) 20.1 (6.0)      
#> 4 mpg   mpg   N (NA)     32 (0)          
mtcars2$hp %>% remove_labels %>% get_label() #NULL
#> NULL
```
