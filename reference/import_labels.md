# Import labels

`import_labels` imports labels from a data.frame (`data_label`) to
another one (`.tbl`). Works in synergy with `save_labels()`.

`save_labels` saves the labels from a data.frame in a temporary variable
that can be retrieve by `import_labels`.

## Usage

``` r
import_labels(
  .tbl,
  data_label,
  name_from = "name",
  label_from = "label",
  warn_name = FALSE,
  warn_label = FALSE,
  verbose = deprecated()
)

save_labels(.tbl)
```

## Arguments

- .tbl:

  the data.frame to be labelled

- data_label:

  a data.frame from which to import labels. If missing, the function
  will take the labels from the last dataframe on which `save_labels()`
  was called.

- name_from:

  in `data_label`, which column to get the variable name (default to
  `name`)

- label_from:

  in `data_label`, which column to get the variable label (default to
  `label`)

- warn_name:

  if TRUE, displays a warning if a variable name is not found in
  `data_label`

- warn_label:

  if TRUE, displays a warning if a label is not found in `.tbl`

- verbose:

  deprecated

## Value

A dataframe, as `.tbl`, with labels

`.tbl` invisibly. Used only for its side effects.

## See also

[`get_label()`](https://danchaltiel.github.io/crosstable/reference/get_label.md),
[`set_label()`](https://danchaltiel.github.io/crosstable/reference/set_label.md),
[`remove_label()`](https://danchaltiel.github.io/crosstable/reference/remove_labels.md),
`save_labels()`

## Author

Dan Chaltiel

## Examples

``` r
#import the labels from a data.frame to another
iris_label = data.frame(
  name=c("Sepal.Length", "Sepal.Width",
         "Petal.Length", "Petal.Width", "Species"),
  label=c("Length of Sepals", "Width of Sepals",
          "Length of Petals", "Width of Petals", "Specie name")
)
iris %>%
  import_labels(iris_label) %>%
  crosstable
#> # A tibble: 19 × 4
#>    .id          label            variable   value        
#>    <chr>        <chr>            <chr>      <chr>        
#>  1 Sepal.Length Length of Sepals Min / Max  4.3 / 7.9    
#>  2 Sepal.Length Length of Sepals Med [IQR]  5.8 [5.1;6.4]
#>  3 Sepal.Length Length of Sepals Mean (std) 5.8 (0.8)    
#>  4 Sepal.Length Length of Sepals N (NA)     150 (0)      
#>  5 Sepal.Width  Width of Sepals  Min / Max  2.0 / 4.4    
#>  6 Sepal.Width  Width of Sepals  Med [IQR]  3.0 [2.8;3.3]
#>  7 Sepal.Width  Width of Sepals  Mean (std) 3.1 (0.4)    
#>  8 Sepal.Width  Width of Sepals  N (NA)     150 (0)      
#>  9 Petal.Length Length of Petals Min / Max  1.0 / 6.9    
#> 10 Petal.Length Length of Petals Med [IQR]  4.3 [1.6;5.1]
#> 11 Petal.Length Length of Petals Mean (std) 3.8 (1.8)    
#> 12 Petal.Length Length of Petals N (NA)     150 (0)      
#> 13 Petal.Width  Width of Petals  Min / Max  0.1 / 2.5    
#> 14 Petal.Width  Width of Petals  Med [IQR]  1.3 [0.3;1.8]
#> 15 Petal.Width  Width of Petals  Mean (std) 1.2 (0.8)    
#> 16 Petal.Width  Width of Petals  N (NA)     150 (0)      
#> 17 Species      Specie name      setosa     50 (33.33%)  
#> 18 Species      Specie name      versicolor 50 (33.33%)  
#> 19 Species      Specie name      virginica  50 (33.33%)  

#save the labels, use some dplyr label-removing function, then retrieve the labels
library(dplyr)
mtcars2 %>%
  save_labels() %>%
  transmute(disp=as.numeric(disp)+1) %>%
  import_labels(warn_label=FALSE) %>% #
  crosstable(disp)
#> # A tibble: 4 × 4
#>   .id   label                 variable   value              
#>   <chr> <chr>                 <chr>      <chr>              
#> 1 disp  Displacement (cu.in.) Min / Max  72.1 / 473.0       
#> 2 disp  Displacement (cu.in.) Med [IQR]  197.3 [121.8;327.0]
#> 3 disp  Displacement (cu.in.) Mean (std) 231.7 (123.9)      
#> 4 disp  Displacement (cu.in.) N (NA)     32 (0)             
```
