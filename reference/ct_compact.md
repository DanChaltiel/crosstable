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
  wrap_cols = NULL,
  rtn_flextable = FALSE
)

# S3 method for class 'crosstable'
ct_compact(
  data,
  name_from = c("label", ".id"),
  name_to = "variable",
  id_from = ".id",
  keep_id = FALSE,
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

- wrap_cols:

  name of the columns to wrap

- rtn_flextable:

  whether to return a formatted
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
  object or a simple `data.frame`

- keep_id:

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
#>      variable Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1      setosa                                                  
#> 2                      5.1         3.5          1.4         0.2
#> 3                      4.9           3          1.4         0.2
#> 4                      4.7         3.2          1.3         0.2
#> 5                      4.6         3.1          1.5         0.2
#> 6                        5         3.6          1.4         0.2
#> 7  versicolor                                                  
#> 8                        7         3.2          4.7         1.4
#> 9                      6.4         3.2          4.5         1.5
#> 10                     6.9         3.1          4.9         1.5
#> 11                     5.5         2.3            4         1.3
#> 12                     6.5         2.8          4.6         1.5
#> 13  virginica                                                  
#> 14                     6.3         3.3            6         2.5
#> 15                     5.8         2.7          5.1         1.9
#> 16                     7.1           3          5.9         2.1
#> 17                     6.3         2.9          5.6         1.8
#> 18                     6.5           3          5.8         2.2
ct_compact(x, name_from="Species", name_to="Petal.Length")
#>    Petal.Length Sepal.Length Sepal.Width Petal.Width
#> 1        setosa                                     
#> 2           1.4          5.1         3.5         0.2
#> 3           1.4          4.9           3         0.2
#> 4           1.3          4.7         3.2         0.2
#> 5           1.5          4.6         3.1         0.2
#> 6           1.4            5         3.6         0.2
#> 7    versicolor                                     
#> 8           4.7            7         3.2         1.4
#> 9           4.5          6.4         3.2         1.5
#> 10          4.9          6.9         3.1         1.5
#> 11            4          5.5         2.3         1.3
#> 12          4.6          6.5         2.8         1.5
#> 13    virginica                                     
#> 14            6          6.3         3.3         2.5
#> 15          5.1          5.8         2.7         1.9
#> 16          5.9          7.1           3         2.1
#> 17          5.6          6.3         2.9         1.8
#> 18          5.8          6.5           3         2.2
x$Species2 = substr(x$Species, 1, 1)
ct_compact(x, name_from="Species", wrap_cols="Species2")
#>      variable Sepal.Length Sepal.Width Petal.Length Petal.Width Species2
#> 1      setosa                                                          s
#> 2                      5.1         3.5          1.4         0.2         
#> 3                      4.9           3          1.4         0.2         
#> 4                      4.7         3.2          1.3         0.2         
#> 5                      4.6         3.1          1.5         0.2         
#> 6                        5         3.6          1.4         0.2         
#> 7  versicolor                                                          v
#> 8                        7         3.2          4.7         1.4         
#> 9                      6.4         3.2          4.5         1.5         
#> 10                     6.9         3.1          4.9         1.5         
#> 11                     5.5         2.3            4         1.3         
#> 12                     6.5         2.8          4.6         1.5         
#> 13  virginica                                                          v
#> 14                     6.3         3.3            6         2.5         
#> 15                     5.8         2.7          5.1         1.9         
#> 16                     7.1           3          5.9         2.1         
#> 17                     6.3         2.9          5.6         1.8         
#> 18                     6.5           3          5.8         2.2         
ct_compact(x, name_from="Species", id_from="Species2") #cut on "v"
#>      variable Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1      setosa                                                  
#> 2                      5.1         3.5          1.4         0.2
#> 3                      4.9           3          1.4         0.2
#> 4                      4.7         3.2          1.3         0.2
#> 5                      4.6         3.1          1.5         0.2
#> 6                        5         3.6          1.4         0.2
#> 7  versicolor                                                  
#> 8                        7         3.2          4.7         1.4
#> 9                      6.4         3.2          4.5         1.5
#> 10                     6.9         3.1          4.9         1.5
#> 11                     5.5         2.3            4         1.3
#> 12                     6.5         2.8          4.6         1.5
#> 13                     6.3         3.3            6         2.5
#> 14                     5.8         2.7          5.1         1.9
#> 15                     7.1           3          5.9         2.1
#> 16                     6.3         2.9          5.6         1.8
#> 17                     6.5           3          5.8         2.2

#crosstables
x=crosstable(mtcars2, c(disp,hp,am), by=vs, test=TRUE, effect=TRUE)
ct_compact(x)
#> # A tibble: 13 × 5
#>    variable              straight             vshaped               effect test 
#>  * <chr>                 <chr>                <chr>                 <chr>  <chr>
#>  1 Displacement (cu.in.) ""                   ""                    "Diff… "p v…
#>  2 Min / Max             "71.1 / 258.0"       "120.3 / 472.0"       ""     ""   
#>  3 Med [IQR]             "120.5 [83.0;162.4]" "311.0 [275.8;360.0]" ""     ""   
#>  4 Mean (std)            "132.5 (56.9)"       "307.1 (106.8)"       ""     ""   
#>  5 N (NA)                "14 (0)"             "18 (0)"              ""     ""   
#>  6 Gross horsepower      ""                   ""                    "Diff… "p v…
#>  7 Min / Max             "52.0 / 123.0"       "91.0 / 335.0"        ""     ""   
#>  8 Med [IQR]             "96.0 [66.0;109.8]"  "180.0 [156.2;226.2]" ""     ""   
#>  9 Mean (std)            "91.4 (24.4)"        "189.7 (60.3)"        ""     ""   
#> 10 N (NA)                "14 (0)"             "18 (0)"              ""     ""   
#> 11 Transmission          ""                   ""                    "Odds… "p v…
#> 12 auto                  "7 (36.84%)"         "12 (63.16%)"         ""     ""   
#> 13 manual                "7 (53.85%)"         "6 (46.15%)"          ""     ""   
ct_compact(x, name_from=".id")
#> # A tibble: 13 × 6
#>    variable   label                   straight             vshaped  effect test 
#>  * <chr>      <chr>                   <chr>                <chr>    <chr>  <chr>
#>  1 disp       ""                      ""                   ""       "Diff… "p v…
#>  2 Min / Max  "Displacement (cu.in.)" "71.1 / 258.0"       "120.3 … ""     ""   
#>  3 Med [IQR]  "Displacement (cu.in.)" "120.5 [83.0;162.4]" "311.0 … ""     ""   
#>  4 Mean (std) "Displacement (cu.in.)" "132.5 (56.9)"       "307.1 … ""     ""   
#>  5 N (NA)     "Displacement (cu.in.)" "14 (0)"             "18 (0)" ""     ""   
#>  6 hp         ""                      ""                   ""       "Diff… "p v…
#>  7 Min / Max  "Gross horsepower"      "52.0 / 123.0"       "91.0 /… ""     ""   
#>  8 Med [IQR]  "Gross horsepower"      "96.0 [66.0;109.8]"  "180.0 … ""     ""   
#>  9 Mean (std) "Gross horsepower"      "91.4 (24.4)"        "189.7 … ""     ""   
#> 10 N (NA)     "Gross horsepower"      "14 (0)"             "18 (0)" ""     ""   
#> 11 am         ""                      ""                   ""       "Odds… "p v…
#> 12 auto       "Transmission"          "7 (36.84%)"         "12 (63… ""     ""   
#> 13 manual     "Transmission"          "7 (53.85%)"         "6 (46.… ""     ""   
```
