# Batch set variable labels

This function is a copycat of from expss package v0.10.7 (slightly
modified) to avoid having to depend on expss. See
[`expss::apply_labels()`](https://rdrr.io/pkg/expss/man/apply_labels.html)
for more documentation. Note that this version is not compatible with
`data.table`.

## Usage

``` r
apply_labels(data, ..., fn, warn_missing = FALSE)
```

## Arguments

- data:

  data.frame/list

- ...:

  named arguments, as `colname="label"`

- fn:

  alternatively, a function to be applied to all existing labels.

- warn_missing:

  if TRUE, throw a warning if some names are missing

## Value

An object of the same type as `data`, with labels

## Author

Dan Chaltiel

## Examples

``` r
iris %>%
  apply_labels(Sepal.Length="Length of Sepal",
               Sepal.Width="Width of Sepal") %>%
  crosstable()
#> # A tibble: 19 × 4
#>    .id          label           variable   value        
#>    <chr>        <chr>           <chr>      <chr>        
#>  1 Sepal.Length Length of Sepal Min / Max  4.3 / 7.9    
#>  2 Sepal.Length Length of Sepal Med [IQR]  5.8 [5.1;6.4]
#>  3 Sepal.Length Length of Sepal Mean (std) 5.8 (0.8)    
#>  4 Sepal.Length Length of Sepal N (NA)     150 (0)      
#>  5 Sepal.Width  Width of Sepal  Min / Max  2.0 / 4.4    
#>  6 Sepal.Width  Width of Sepal  Med [IQR]  3.0 [2.8;3.3]
#>  7 Sepal.Width  Width of Sepal  Mean (std) 3.1 (0.4)    
#>  8 Sepal.Width  Width of Sepal  N (NA)     150 (0)      
#>  9 Petal.Length Petal.Length    Min / Max  1.0 / 6.9    
#> 10 Petal.Length Petal.Length    Med [IQR]  4.3 [1.6;5.1]
#> 11 Petal.Length Petal.Length    Mean (std) 3.8 (1.8)    
#> 12 Petal.Length Petal.Length    N (NA)     150 (0)      
#> 13 Petal.Width  Petal.Width     Min / Max  0.1 / 2.5    
#> 14 Petal.Width  Petal.Width     Med [IQR]  1.3 [0.3;1.8]
#> 15 Petal.Width  Petal.Width     Mean (std) 1.2 (0.8)    
#> 16 Petal.Width  Petal.Width     N (NA)     150 (0)      
#> 17 Species      Species         setosa     50 (33.33%)  
#> 18 Species      Species         versicolor 50 (33.33%)  
#> 19 Species      Species         virginica  50 (33.33%)  
iris2 %>%
  apply_labels(fn=tolower) %>%
  crosstable()
#> # A tibble: 19 × 4
#>    .id          label           variable   value        
#>    <chr>        <chr>           <chr>      <chr>        
#>  1 Sepal.Length length of sepal Min / Max  4.3 / 7.9    
#>  2 Sepal.Length length of sepal Med [IQR]  5.8 [5.1;6.4]
#>  3 Sepal.Length length of sepal Mean (std) 5.8 (0.8)    
#>  4 Sepal.Length length of sepal N (NA)     150 (0)      
#>  5 Sepal.Width  width of sepal  Min / Max  2.0 / 4.4    
#>  6 Sepal.Width  width of sepal  Med [IQR]  3.0 [2.8;3.3]
#>  7 Sepal.Width  width of sepal  Mean (std) 3.1 (0.4)    
#>  8 Sepal.Width  width of sepal  N (NA)     150 (0)      
#>  9 Petal.Length length of petal Min / Max  1.0 / 6.9    
#> 10 Petal.Length length of petal Med [IQR]  4.3 [1.6;5.1]
#> 11 Petal.Length length of petal Mean (std) 3.8 (1.8)    
#> 12 Petal.Length length of petal N (NA)     150 (0)      
#> 13 Petal.Width  width of petal  Min / Max  0.1 / 2.5    
#> 14 Petal.Width  width of petal  Med [IQR]  1.3 [0.3;1.8]
#> 15 Petal.Width  width of petal  Mean (std) 1.2 (0.8)    
#> 16 Petal.Width  width of petal  N (NA)     150 (0)      
#> 17 Species      specie          setosa     50 (33.33%)  
#> 18 Species      specie          versicolor 50 (33.33%)  
#> 19 Species      specie          virginica  50 (33.33%)  
```
