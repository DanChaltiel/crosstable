# Generic function to compact a table (publication formatting)

Generic function to compact a table (publication formatting)

## Usage

``` r
# S3 method for class 'data.frame'
ct_compact(
  data,
  name_from,
  name_to = "variable",
  ...,
  id_from = name_from,
  keep_id = TRUE,
  collapse = NULL,
  wrap_cols = NULL,
  rtn_flextable = FALSE
)

# S3 method for class 'crosstable'
ct_compact(
  data,
  name_from = c("label", ".id"),
  name_to = "variable",
  id_from = ".id",
  label_with_id = FALSE,
  keep_id = TRUE,
  ...
)
```

## Arguments

- data:

  the object to compact

- ...:

  additional arguments (not used)

- name_from:

  name of the column to be collapsed when compacting

- name_to:

  name of the column that will receive the collapsed column. Will be
  created if it doesn't exist.

- id_from:

  name of the columns to use as cut-off. Useful when successive
  `name_from` have the same value.

- keep_id:

  whether to keep `id_from` in the output.

- collapse:

  levels to collapse into one row.

- wrap_cols:

  name of the columns to wrap

- rtn_flextable:

  whether to return a formatted
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
  object or a simple `data.frame`

- label_with_id:

  `glue` pattern to keep the column name along with the label. If
  `TRUE`, default to `"{label} ({.id})"`.

## Value

a compacted data.frame

## Author

Dan Chaltiel

## Examples

``` r
#dataframes
x=iris[c(1:5,51:55,101:105),]
ct_compact(x, name_from="Species")
#>       Species   variable Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1      setosa     setosa           NA          NA           NA          NA
#> 2      setosa                     5.1         3.5          1.4         0.2
#> 3      setosa                     4.9         3.0          1.4         0.2
#> 4      setosa                     4.7         3.2          1.3         0.2
#> 5      setosa                     4.6         3.1          1.5         0.2
#> 6      setosa                     5.0         3.6          1.4         0.2
#> 7  versicolor versicolor           NA          NA           NA          NA
#> 8  versicolor                     7.0         3.2          4.7         1.4
#> 9  versicolor                     6.4         3.2          4.5         1.5
#> 10 versicolor                     6.9         3.1          4.9         1.5
#> 11 versicolor                     5.5         2.3          4.0         1.3
#> 12 versicolor                     6.5         2.8          4.6         1.5
#> 13  virginica  virginica           NA          NA           NA          NA
#> 14  virginica                     6.3         3.3          6.0         2.5
#> 15  virginica                     5.8         2.7          5.1         1.9
#> 16  virginica                     7.1         3.0          5.9         2.1
#> 17  virginica                     6.3         2.9          5.6         1.8
#> 18  virginica                     6.5         3.0          5.8         2.2
ct_compact(x, name_from="Species", name_to="Petal.Length")
#>       Species Petal.Length Sepal.Length Sepal.Width Petal.Width
#> 1      setosa       setosa           NA          NA          NA
#> 2      setosa          1.4          5.1         3.5         0.2
#> 3      setosa          1.4          4.9         3.0         0.2
#> 4      setosa          1.3          4.7         3.2         0.2
#> 5      setosa          1.5          4.6         3.1         0.2
#> 6      setosa          1.4          5.0         3.6         0.2
#> 7  versicolor   versicolor           NA          NA          NA
#> 8  versicolor          4.7          7.0         3.2         1.4
#> 9  versicolor          4.5          6.4         3.2         1.5
#> 10 versicolor          4.9          6.9         3.1         1.5
#> 11 versicolor            4          5.5         2.3         1.3
#> 12 versicolor          4.6          6.5         2.8         1.5
#> 13  virginica    virginica           NA          NA          NA
#> 14  virginica            6          6.3         3.3         2.5
#> 15  virginica          5.1          5.8         2.7         1.9
#> 16  virginica          5.9          7.1         3.0         2.1
#> 17  virginica          5.6          6.3         2.9         1.8
#> 18  virginica          5.8          6.5         3.0         2.2
x$Species2 = substr(x$Species, 1, 1)
ct_compact(x, name_from="Species", wrap_cols="Species2")
#>       Species   variable Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1      setosa     setosa           NA          NA           NA          NA
#> 2      setosa                     5.1         3.5          1.4         0.2
#> 3      setosa                     4.9         3.0          1.4         0.2
#> 4      setosa                     4.7         3.2          1.3         0.2
#> 5      setosa                     4.6         3.1          1.5         0.2
#> 6      setosa                     5.0         3.6          1.4         0.2
#> 7  versicolor versicolor           NA          NA           NA          NA
#> 8  versicolor                     7.0         3.2          4.7         1.4
#> 9  versicolor                     6.4         3.2          4.5         1.5
#> 10 versicolor                     6.9         3.1          4.9         1.5
#> 11 versicolor                     5.5         2.3          4.0         1.3
#> 12 versicolor                     6.5         2.8          4.6         1.5
#> 13  virginica  virginica           NA          NA           NA          NA
#> 14  virginica                     6.3         3.3          6.0         2.5
#> 15  virginica                     5.8         2.7          5.1         1.9
#> 16  virginica                     7.1         3.0          5.9         2.1
#> 17  virginica                     6.3         2.9          5.6         1.8
#> 18  virginica                     6.5         3.0          5.8         2.2
#>    Species2
#> 1         s
#> 2      <NA>
#> 3      <NA>
#> 4      <NA>
#> 5      <NA>
#> 6      <NA>
#> 7         v
#> 8      <NA>
#> 9      <NA>
#> 10     <NA>
#> 11     <NA>
#> 12     <NA>
#> 13        v
#> 14     <NA>
#> 15     <NA>
#> 16     <NA>
#> 17     <NA>
#> 18     <NA>
ct_compact(x, name_from="Species", id_from="Species2") #cut on "v"
#>    Species2   variable Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1         s     setosa           NA          NA           NA          NA
#> 2         s                     5.1         3.5          1.4         0.2
#> 3         s                     4.9         3.0          1.4         0.2
#> 4         s                     4.7         3.2          1.3         0.2
#> 5         s                     4.6         3.1          1.5         0.2
#> 6         s                     5.0         3.6          1.4         0.2
#> 7         v versicolor           NA          NA           NA          NA
#> 8         v  virginica           NA          NA           NA          NA
#> 9         v                     7.0         3.2          4.7         1.4
#> 10        v                     6.4         3.2          4.5         1.5
#> 11        v                     6.9         3.1          4.9         1.5
#> 12        v                     5.5         2.3          4.0         1.3
#> 13        v                     6.5         2.8          4.6         1.5
#> 14        v                     6.3         3.3          6.0         2.5
#> 15        v                     5.8         2.7          5.1         1.9
#> 16        v                     7.1         3.0          5.9         2.1
#> 17        v                     6.3         2.9          5.6         1.8
#> 18        v                     6.5         3.0          5.8         2.2

#crosstables
x=crosstable(mtcars2, c(disp,hp,am), by=vs, test=TRUE, effect=TRUE)
ct_compact(x)
#> # A tibble: 13 × 6
#>    .id   variable              straight           vshaped           effect test 
#>  * <chr> <chr>                 <chr>              <chr>             <chr>  <chr>
#>  1 disp  Displacement (cu.in.) NA                 NA                "Diff… "p v…
#>  2 disp  Min / Max             71.1 / 258.0       120.3 / 472.0      NA     NA  
#>  3 disp  Med [IQR]             120.5 [83.0;162.4] 311.0 [275.8;360…  NA     NA  
#>  4 disp  Mean (std)            132.5 (56.9)       307.1 (106.8)      NA     NA  
#>  5 disp  N (NA)                14 (0)             18 (0)             NA     NA  
#>  6 hp    Gross horsepower      NA                 NA                "Diff… "p v…
#>  7 hp    Min / Max             52.0 / 123.0       91.0 / 335.0       NA     NA  
#>  8 hp    Med [IQR]             96.0 [66.0;109.8]  180.0 [156.2;226…  NA     NA  
#>  9 hp    Mean (std)            91.4 (24.4)        189.7 (60.3)       NA     NA  
#> 10 hp    N (NA)                14 (0)             18 (0)             NA     NA  
#> 11 am    Transmission          NA                 NA                "Odds… "p v…
#> 12 am    auto                  7 (36.84%)         12 (63.16%)        NA     NA  
#> 13 am    manual                7 (53.85%)         6 (46.15%)         NA     NA  
ct_compact(x, name_from=".id")
#> # A tibble: 13 × 7
#>    .id   variable   label                 straight          vshaped effect test 
#>  * <chr> <chr>      <chr>                 <chr>             <chr>   <chr>  <chr>
#>  1 disp  disp       NA                    NA                NA      "Diff… "p v…
#>  2 disp  Min / Max  Displacement (cu.in.) 71.1 / 258.0      120.3 …  NA     NA  
#>  3 disp  Med [IQR]  Displacement (cu.in.) 120.5 [83.0;162.… 311.0 …  NA     NA  
#>  4 disp  Mean (std) Displacement (cu.in.) 132.5 (56.9)      307.1 …  NA     NA  
#>  5 disp  N (NA)     Displacement (cu.in.) 14 (0)            18 (0)   NA     NA  
#>  6 hp    hp         NA                    NA                NA      "Diff… "p v…
#>  7 hp    Min / Max  Gross horsepower      52.0 / 123.0      91.0 /…  NA     NA  
#>  8 hp    Med [IQR]  Gross horsepower      96.0 [66.0;109.8] 180.0 …  NA     NA  
#>  9 hp    Mean (std) Gross horsepower      91.4 (24.4)       189.7 …  NA     NA  
#> 10 hp    N (NA)     Gross horsepower      14 (0)            18 (0)   NA     NA  
#> 11 am    am         NA                    NA                NA      "Odds… "p v…
#> 12 am    auto       Transmission          7 (36.84%)        12 (63…  NA     NA  
#> 13 am    manual     Transmission          7 (53.85%)        6 (46.…  NA     NA  
```
