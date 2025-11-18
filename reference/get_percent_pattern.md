# Percent pattern helper

Get a list with pre-filled values for `percent_pattern`.

## Usage

``` r
get_percent_pattern(
  margin = c("row", "column", "cell", "none", "all"),
  na = FALSE,
  warn_duplicates = TRUE
)
```

## Arguments

- margin:

  a vector giving the margins to compute.

- na:

  whether to use `NA`

- warn_duplicates:

  whether to warn if margin has duplicates

## Value

a list

## Examples

``` r
get_percent_pattern(c("cells","row","column"))
#> $body
#> {n} ({p_tot} / {p_col} / {p_row})
#> 
#> $total_row
#> [1] "{n} ({p_col})"
#> 
#> $total_col
#> [1] "{n} ({p_row})"
#> 
#> $total_all
#> [1] "{n} ({p_tot})"
#> 
get_percent_pattern(c("cells","row","column"), na=TRUE)
#> $body
#> [1] "{n} ({p_tot_na} / {p_col_na} / {p_row_na})"
#> 
#> $total_row
#> [1] "{n} ({p_col_na})"
#> 
#> $total_col
#> [1] "{n} ({p_row_na})"
#> 
#> $total_all
#> [1] "{n} ({p_tot_na})"
#> 
```
