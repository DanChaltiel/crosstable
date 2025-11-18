# Using \`percent_pattern\`

When working with categorical variables,
[`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
allows a very flexible output thanks to the `percent_pattern` argument.

This vignette will review the many things you can do using
`percent_pattern`.

## initialization

First, let’s add some missing values to the `mtcars2` dataset and tweak
some options:

``` r
library(crosstable)
mtcars3 = mtcars2
mtcars3$cyl[1:5] = NA
mtcars3$vs[5:12] = NA

crosstable_options(
  percent_digits=0
)
```

## Default behaviour

By default,
[`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
will use `percent_pattern="{n} ({p_row})"`, so it outputs the size `n`
along with the row’s percentage `p_row`:

``` r
crosstable(mtcars3, cyl, by=vs) %>% as_flextable()
```

| label               | variable | Engine   |           |     |
|---------------------|----------|----------|-----------|-----|
|                     |          | straight | vshaped   | NA  |
| Number of cylinders | 4        | 7 (88%)  | 1 (12%)   | 2   |
|                     | 6        | 0 (0%)   | 1 (100%)  | 3   |
|                     | 8        | 0 (0%)   | 11 (100%) | 2   |
|                     | NA       | 2        | 2         | 1   |

Here, we will see how we can tweak `percent_pattern` in order to display
other figures.

**NOTE**: Missing values will always be described with `n` alone. If you
want to describe them as non-missing values, you will have to mutate
them as one, most likely using
[`forcats::fct_explicit_na()`](https://forcats.tidyverse.org/reference/fct_explicit_na.html).

## Allowed variables

First, here is the list of all the internal variables you can use:

- `n`, `n_row`, `n_col`, and `n_tot`: respectively the size of the cell,
  the row, the column, and the whole table.
- `p_row`, `p_col`, and `p_tot`: respectively the proportion relative to
  the row, the column, and the whole table.
- `p_tot_inf`, `p_tot_sup`, `p_row_inf`, `p_row_sup`, `p_col_inf`,
  `p_col_sup`: the confidence interval (calculated using [Wilson
  score](https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval))
  for each of the proportions above.

Should you ever need it, note that it is also possible to use any
external variable defined outside of
[`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md).

Here is a simple example:

``` r
crosstable(mtcars3, cyl, by=vs, 
           percent_pattern="N={n}/{n_row} -> p={p_row}") %>% 
  as_flextable()
```

| label               | variable | Engine          |                    |     |
|---------------------|----------|-----------------|--------------------|-----|
|                     |          | straight        | vshaped            | NA  |
| Number of cylinders | 4        | N=7/8 -\> p=88% | N=1/8 -\> p=12%    | 2   |
|                     | 6        | N=0/1 -\> p=0%  | N=1/1 -\> p=100%   | 3   |
|                     | 8        | N=0/11 -\> p=0% | N=11/11 -\> p=100% | 2   |
|                     | NA       | 2               | 2                  | 1   |

## Missing values

As you can see, these internal variables do not account for missing
values (except for `n`, obviously).

This should make sense in most cases, but if it doesn’t, you can use the
following variables to account for NA explicitly:

- `n_row_na`, `n_col_na`, `n_tot_na`
- `p_tot_na`, `p_row_na`, `p_col_na`
- `p_tot_na_inf`, `p_tot_na_sup`, `p_row_na_inf`, `p_row_na_sup`,
  `p_col_na_inf`, `p_col_na_sup`

(See the [last section](#allowed-variables) for an example)

Note that if you use `showNA="no"`, there will be no difference between
the standard variables and the `_na` variables.

## Proportions in totals

As you may have noticed, totals are considered separately:

``` r
crosstable(mtcars3, cyl, by=vs, total=TRUE, 
           percent_pattern="N={n}, p={p_row} ({n}/{n_row})") %>% 
  as_flextable()
```

| label               | variable | Engine           |                      |     | Total     |
|---------------------|----------|------------------|----------------------|-----|-----------|
|                     |          | straight         | vshaped              | NA  |           |
| Number of cylinders | 4        | N=7, p=88% (7/8) | N=1, p=12% (1/8)     | 2   | 10 (37%)  |
|                     | 6        | N=0, p=0% (0/1)  | N=1, p=100% (1/1)    | 3   | 4 (15%)   |
|                     | 8        | N=0, p=0% (0/11) | N=11, p=100% (11/11) | 2   | 13 (48%)  |
|                     | NA       | 2                | 2                    | 1   | 5         |
|                     | Total    | 9 (38%)          | 15 (62%)             | 8   | 32 (100%) |

Indeed, you cannot have the same pattern for totals. For instance, the
proportion relative to the row would not make sense in the context of
the entire row itself.

To get control over the `percent_pattern` in totals, you have to pass a
list with names `body`, `total_row`, `total_col`, and `total_all`:

``` r
pp = list(body="N={n}, p={p_tot} ({n}/{n_tot})", 
          total_row="N={n} p=({p_col})", 
          total_col="{n}", total_all="Total={n}")
crosstable(mtcars3, cyl, by=vs, total=TRUE, 
           percent_pattern=pp) %>% 
  as_flextable()
```

| label               | variable | Engine            |                     |     | Total    |
|---------------------|----------|-------------------|---------------------|-----|----------|
|                     |          | straight          | vshaped             | NA  |          |
| Number of cylinders | 4        | N=7, p=35% (7/20) | N=1, p=5% (1/20)    | 2   | 10       |
|                     | 6        | N=0, p=0% (0/20)  | N=1, p=5% (1/20)    | 3   | 4        |
|                     | 8        | N=0, p=0% (0/20)  | N=11, p=55% (11/20) | 2   | 13       |
|                     | NA       | 2                 | 2                   | 1   | 5        |
|                     | Total    | N=9 p=(38%)       | N=15 p=(62%)        | 8   | Total=32 |

## `get_percent_pattern()`

To easily get a `percent_pattern` list, you can use the
[`get_percent_pattern()`](https://danchaltiel.github.io/crosstable/reference/get_percent_pattern.md)
helper:

``` r
get_percent_pattern("all")
#> $body
#> [1] "{n} ({p_tot} / {p_row} / {p_col})"
#> 
#> $total_row
#> [1] "{n} ({p_col})"
#> 
#> $total_col
#> [1] "{n} ({p_row})"
#> 
#> $total_all
#> [1] "{n} ({p_tot})"
get_percent_pattern("col", na=TRUE)
#> $body
#> [1] "{n} ({p_col_na})"
#> 
#> $total_row
#> [1] "{n} ({p_col_na})"
#> 
#> $total_col
#> [1] "{n} ({p_row_na})"
#> 
#> $total_all
#> [1] "{n} ({p_tot_na})"
```

You can also set the result to a variable and modify its members at
will. See
[`?get_percent_pattern`](https://danchaltiel.github.io/crosstable/reference/get_percent_pattern.md)
for more information.

## Ultimate example

Here is the ultimate example for `percent_pattern`. Give a close look to
all possible values and you will surely find the one that you need.

``` r
ULTIMATE_PATTERN=list(
  body="N={n}
        Cell: p = {p_tot} ({n}/{n_tot}) [{p_tot_inf}; {p_tot_sup}]
        Col: p = {p_col} ({n}/{n_col}) [{p_col_inf}; {p_col_sup}]
        Row: p = {p_row} ({n}/{n_row}) [{p_row_inf}; {p_row_sup}]
        
        Cell (NA): p = {p_tot_na} ({n}/{n_tot_na}) [{p_tot_na_inf}; {p_tot_na_sup}]
        Col (NA): p = {p_col_na} ({n}/{n_col_na}) [{p_col_na_inf}; {p_col_na_sup}]
        Row (NA): p = {p_row_na} ({n}/{n_row_na}) [{p_row_na_inf}; {p_row_na_sup}]",
  total_row="N={n}
             Row: p = {p_row} ({n}/{n_row}) [{p_row_inf}; {p_row_sup}]
             Row (NA): p = {p_row_na} ({n}/{n_row_na}) [{p_row_na_inf}; {p_row_na_sup}]",
  total_col="N={n}
             Col: p = {p_col} ({n}/{n_col}) [{p_col_inf}; {p_col_sup}]
             Col (NA): p = {p_col_na} ({n}/{n_col_na}) [{p_col_na_inf}; {p_col_na_sup}]",
  total_all="N={n}
             P: {p_col} [{p_col_inf}; {p_col_sup}]
             P (NA): {p_col} [{p_col_na_inf}; {p_col_na_sup}]"
)

crosstable(mtcars3, cyl, by=vs,
           percent_digits=0, total=TRUE, showNA="always",
           percent_pattern=ULTIMATE_PATTERN) %>% 
  as_flextable() %>% 
  flextable::theme_box()
```

[TABLE]
