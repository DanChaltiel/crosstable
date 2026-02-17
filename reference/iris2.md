# Modified `iris` dataset

Modified `iris` dataset so:

- every column is labelled (using `label` attribute)

- `Species` column is considered as factor

See [`iris`](https://rdrr.io/r/datasets/iris.html) for more informations
on the original "Edgar Anderson's Iris Data" dataset.

## Usage

``` r
iris2
```

## Format

A data frame with 150 observations on 5 variables with labels.

## Source

    library(dplyr)
    iris2 = iris %>%
        expss::apply_labels( #I also could have used [import_labels] or even `labelled::set_variable_labels()`
            Species = "Specie",
            Sepal.Length = "Length of Sepal",
            Sepal.Width = "Width of Sepal",
            Petal.Length = "Length of Petal",
            Petal.Width = "Width of Petal"
        ) %>%
        as_tibble()

## Examples

``` r
library(crosstable)
ct=crosstable(iris2, by=Species)
ct
#> # A tibble: 16 × 6
#>    .id          label           variable   setosa        versicolor    virginica
#>    <chr>        <chr>           <chr>      <chr>         <chr>         <chr>    
#>  1 Sepal.Length Length of Sepal Min / Max  4.3 / 5.8     4.9 / 7.0     4.9 / 7.9
#>  2 Sepal.Length Length of Sepal Med [IQR]  5.0 [4.8;5.2] 5.9 [5.6;6.3] 6.5 [6.2…
#>  3 Sepal.Length Length of Sepal Mean (std) 5.0 (0.4)     5.9 (0.5)     6.6 (0.6)
#>  4 Sepal.Length Length of Sepal N (NA)     50 (0)        50 (0)        50 (0)   
#>  5 Sepal.Width  Width of Sepal  Min / Max  2.3 / 4.4     2.0 / 3.4     2.2 / 3.8
#>  6 Sepal.Width  Width of Sepal  Med [IQR]  3.4 [3.2;3.7] 2.8 [2.5;3.0] 3.0 [2.8…
#>  7 Sepal.Width  Width of Sepal  Mean (std) 3.4 (0.4)     2.8 (0.3)     3.0 (0.3)
#>  8 Sepal.Width  Width of Sepal  N (NA)     50 (0)        50 (0)        50 (0)   
#>  9 Petal.Length Length of Petal Min / Max  1.0 / 1.9     3.0 / 5.1     4.5 / 6.9
#> 10 Petal.Length Length of Petal Med [IQR]  1.5 [1.4;1.6] 4.3 [4.0;4.6] 5.5 [5.1…
#> 11 Petal.Length Length of Petal Mean (std) 1.5 (0.2)     4.3 (0.5)     5.6 (0.6)
#> 12 Petal.Length Length of Petal N (NA)     50 (0)        50 (0)        50 (0)   
#> 13 Petal.Width  Width of Petal  Min / Max  0.1 / 0.6     1.0 / 1.8     1.4 / 2.5
#> 14 Petal.Width  Width of Petal  Med [IQR]  0.2 [0.2;0.3] 1.3 [1.2;1.5] 2.0 [1.8…
#> 15 Petal.Width  Width of Petal  Mean (std) 0.2 (0.1)     1.3 (0.2)     2.0 (0.3)
#> 16 Petal.Width  Width of Petal  N (NA)     50 (0)        50 (0)        50 (0)   
as_flextable(ct)


.cl-119a64d2{table-layout:auto;}.cl-11932a1e{font-family:'DejaVu Sans';font-size:14pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-11932a32{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-119611ca{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-119611d4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-119611de{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-11963b96{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963ba0{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963baa{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963bb4{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963bb5{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963bb6{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963bbe{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963bbf{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-11963bc0{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



label
```

variable

Specie

setosa

versicolor

virginica

Length of Sepal

Min / Max

4.3 / 5.8

4.9 / 7.0

4.9 / 7.9

Med \[IQR\]

5.0 \[4.8;5.2\]

5.9 \[5.6;6.3\]

6.5 \[6.2;6.9\]

Mean (std)

5.0 (0.4)

5.9 (0.5)

6.6 (0.6)

N (NA)

50 (0)

50 (0)

50 (0)

Width of Sepal

Min / Max

2.3 / 4.4

2.0 / 3.4

2.2 / 3.8

Med \[IQR\]

3.4 \[3.2;3.7\]

2.8 \[2.5;3.0\]

3.0 \[2.8;3.2\]

Mean (std)

3.4 (0.4)

2.8 (0.3)

3.0 (0.3)

N (NA)

50 (0)

50 (0)

50 (0)

Length of Petal

Min / Max

1.0 / 1.9

3.0 / 5.1

4.5 / 6.9

Med \[IQR\]

1.5 \[1.4;1.6\]

4.3 \[4.0;4.6\]

5.5 \[5.1;5.9\]

Mean (std)

1.5 (0.2)

4.3 (0.5)

5.6 (0.6)

N (NA)

50 (0)

50 (0)

50 (0)

Width of Petal

Min / Max

0.1 / 0.6

1.0 / 1.8

1.4 / 2.5

Med \[IQR\]

0.2 \[0.2;0.3\]

1.3 \[1.2;1.5\]

2.0 \[1.8;2.3\]

Mean (std)

0.2 (0.1)

1.3 (0.2)

2.0 (0.3)

N (NA)

50 (0)

50 (0)

50 (0)
