# Easily describe datasets

Generate a descriptive table of all chosen columns, as contingency
tables for categorical variables and as calculation summaries for
numeric variables. If the `by` argument points to one or several
categorical variables, `crosstable` will output a description of all
columns for each level. Otherwise, if it points to a numeric variable,
`crosstable` will calculate correlation coefficients with all other
selected numeric columns. Finally, if it points to a `Surv` object,
`crosstable` will describe the survival at different times.  
  
Can be formatted as an HTML table using
[`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md).

## Usage

``` r
crosstable(
  data,
  cols = everything(),
  ...,
  by = NULL,
  total = c("none", "row", "column", "both"),
  percent_pattern = "{n} ({p_row})",
  percent_digits = 2,
  num_digits = 1,
  showNA = c("ifany", "always", "no"),
  label = TRUE,
  funs = c(` ` = cross_summary),
  funs_arg = list(),
  cor_method = c("pearson", "kendall", "spearman"),
  drop_levels = FALSE,
  remove_zero_percent = NULL,
  unique_numeric = 3,
  date_format = NULL,
  times = NULL,
  followup = FALSE,
  test = FALSE,
  test_args = crosstable_test_args(),
  effect = FALSE,
  effect_args = crosstable_effect_args(),
  margin = deprecated(),
  .vars = deprecated()
)
```

## Arguments

- data:

  A data.frame

- cols:

  \<[`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)\>
  Columns to describe, default to
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  See examples or
  [`vignette("crosstable-selection")`](https://danchaltiel.github.io/crosstable/articles/crosstable-selection.md)
  for more details.

- ...:

  Unused. All parameters after this one must be named.

- by:

  The variable to group on. Character or name.

- total:

  one of \["none", "row", "column" or "both"\] to indicate whether to
  add total rows and/or columns. Default to `none`.

- percent_pattern:

  Pattern used to describe proportions in categorical data. Syntax uses
  a [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  specification, see the **section** below for more details. Default to
  `"{n} ({p_col})"` if `by` is null and `"{n} ({p_row})"` if it is not.

- percent_digits:

  Number of digits for percentages.

- num_digits:

  Number of digits for numeric summaries.

- showNA:

  Whether to show NA in categorical variables (one of
  `c("ifany", "always", "no")`, like in
  [`table()`](https://rdrr.io/r/base/table.html)).

- label:

  Whether to show labels. See
  [`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md)
  or
  [`set_label()`](https://danchaltiel.github.io/crosstable/reference/set_label.md)for
  how to add labels to the dataset columns.

- funs:

  Functions to apply to numeric variables. Default to
  [`cross_summary()`](https://danchaltiel.github.io/crosstable/reference/cross_summary.md).

- funs_arg:

  Additional parameters for `funs`, e.g. `digits` (the number of decimal
  places) for the default
  [`cross_summary()`](https://danchaltiel.github.io/crosstable/reference/cross_summary.md).
  Ultimately, these arguments are passed to
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md).

- cor_method:

  One of `c("pearson", "kendall", "spearman")` to indicate which
  correlation coefficient is to be used.

- drop_levels:

  Whether to drop unused levels of factor variables. Default to `TRUE`.

- remove_zero_percent:

  Whether to remove proportions when `n==0`. Default to `FALSE`.

- unique_numeric:

  The number of non-missing different levels a variable should have to
  be considered as numeric.

- date_format:

  if `x` is a vector of Date or POSIXt, the format to apply (see
  [strptime](https://rdrr.io/r/base/strptime.html) for formats)

- times:

  When using formula with
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  objects, which times to summarize.

- followup:

  When using formula with
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  objects, whether to display follow-up time.

- test:

  Whether to perform tests.

- test_args:

  See
  [`crosstable_test_args`](https://danchaltiel.github.io/crosstable/reference/crosstable_test_args.md)
  to override default testing behaviour.

- effect:

  Whether to compute a effect measure.

- effect_args:

  See
  [`crosstable_effect_args`](https://danchaltiel.github.io/crosstable/reference/crosstable_effect_args.md)
  to override default behaviour.

- margin:

  Deprecated in favor of `percent_pattern`. One of \["row", "column",
  "cell", "none", or "all"\]. Default to `row`.

- .vars:

  Deprecated in favor of `cols`.

## Value

A `data.frame`/`tibble` of class `crosstable`

## About `percent_pattern`

The `percent_pattern` argument is very powerful but can be difficult to
understand at first :

- It is usually a single string that uses the glue syntax, where
  variables are put in curly braces (`{x}`).

- Counts are expressed as `{n}`, `{n_row}`, `{n_col}`, and `{n_tot}`,
  and proportions as `{p_row}`, `{p_col}`, and `{p_cell}`, depending on
  the margin on which they are calculated.

- For each variable, a version including missing values in the total is
  proposed as `{n_xxx_na}` or `{p_xxx_na}`.

- For each proportion, a confidence interval is also calculated using
  [Wilson
  score](https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval)
  and can be expressed as `{p_xxx_inf}` and `{p_xxx_sup}`. See examples
  for practical applications.

- Alternatively, `percent_pattern` can be a list of characters with
  names `body`, `total_row`, `total_col`, and `total_all` to also
  control the pattern in other parts of the crosstable than the body.

## See also

https://danchaltiel.github.io/crosstable/, as_flextable, import_labels

## Author

Dan Chaltiel

## Examples

``` r
#whole table
crosstable(iris)
#> # A tibble: 19 × 4
#>    .id          label        variable   value        
#>    <chr>        <chr>        <chr>      <chr>        
#>  1 Sepal.Length Sepal.Length Min / Max  4.3 / 7.9    
#>  2 Sepal.Length Sepal.Length Med [IQR]  5.8 [5.1;6.4]
#>  3 Sepal.Length Sepal.Length Mean (std) 5.8 (0.8)    
#>  4 Sepal.Length Sepal.Length N (NA)     150 (0)      
#>  5 Sepal.Width  Sepal.Width  Min / Max  2.0 / 4.4    
#>  6 Sepal.Width  Sepal.Width  Med [IQR]  3.0 [2.8;3.3]
#>  7 Sepal.Width  Sepal.Width  Mean (std) 3.1 (0.4)    
#>  8 Sepal.Width  Sepal.Width  N (NA)     150 (0)      
#>  9 Petal.Length Petal.Length Min / Max  1.0 / 6.9    
#> 10 Petal.Length Petal.Length Med [IQR]  4.3 [1.6;5.1]
#> 11 Petal.Length Petal.Length Mean (std) 3.8 (1.8)    
#> 12 Petal.Length Petal.Length N (NA)     150 (0)      
#> 13 Petal.Width  Petal.Width  Min / Max  0.1 / 2.5    
#> 14 Petal.Width  Petal.Width  Med [IQR]  1.3 [0.3;1.8]
#> 15 Petal.Width  Petal.Width  Mean (std) 1.2 (0.8)    
#> 16 Petal.Width  Petal.Width  N (NA)     150 (0)      
#> 17 Species      Species      setosa     50 (33.33%)  
#> 18 Species      Species      versicolor 50 (33.33%)  
#> 19 Species      Species      virginica  50 (33.33%)  
crosstable(mtcars)
#> # A tibble: 38 × 4
#>    .id   label variable   value              
#>    <chr> <chr> <chr>      <chr>              
#>  1 mpg   mpg   Min / Max  10.4 / 33.9        
#>  2 mpg   mpg   Med [IQR]  19.2 [15.4;22.8]   
#>  3 mpg   mpg   Mean (std) 20.1 (6.0)         
#>  4 mpg   mpg   N (NA)     32 (0)             
#>  5 cyl   cyl   4          11 (34.38%)        
#>  6 cyl   cyl   6          7 (21.88%)         
#>  7 cyl   cyl   8          14 (43.75%)        
#>  8 disp  disp  Min / Max  71.1 / 472.0       
#>  9 disp  disp  Med [IQR]  196.3 [120.8;326.0]
#> 10 disp  disp  Mean (std) 230.7 (123.9)      
#> # ℹ 28 more rows
crosstable(mtcars2)
#> # A tibble: 78 × 4
#>    .id   label variable           value    
#>    <chr> <chr> <chr>              <chr>    
#>  1 model Model AMC Javelin        1 (3.12%)
#>  2 model Model Cadillac Fleetwood 1 (3.12%)
#>  3 model Model Camaro Z28         1 (3.12%)
#>  4 model Model Chrysler Imperial  1 (3.12%)
#>  5 model Model Datsun 710         1 (3.12%)
#>  6 model Model Dodge Challenger   1 (3.12%)
#>  7 model Model Duster 360         1 (3.12%)
#>  8 model Model Ferrari Dino       1 (3.12%)
#>  9 model Model Fiat 128           1 (3.12%)
#> 10 model Model Fiat X1-9          1 (3.12%)
#> # ℹ 68 more rows

#tidyselection, custom functions
library(dplyr)
crosstable(mtcars2, c(ends_with("t"), starts_with("c")), by=vs,
           funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75)))
#> # A tibble: 12 × 5
#>    .id   label                 variable     straight    vshaped     
#>    <chr> <chr>                 <chr>        <chr>       <chr>       
#>  1 drat  Rear axle ratio       mean         3.9         3.4         
#>  2 drat  Rear axle ratio       quantile 25% 3.7         3.1         
#>  3 drat  Rear axle ratio       quantile 75% 4.1         3.7         
#>  4 wt    Weight (1000 lbs)     mean         2.6         3.7         
#>  5 wt    Weight (1000 lbs)     quantile 25% 2.0         3.2         
#>  6 wt    Weight (1000 lbs)     quantile 75% 3.2         3.8         
#>  7 cyl   Number of cylinders   4            10 (90.91%) 1 (9.09%)   
#>  8 cyl   Number of cylinders   6            4 (57.14%)  3 (42.86%)  
#>  9 cyl   Number of cylinders   8            0 (0%)      14 (100.00%)
#> 10 carb  Number of carburetors mean         1.8         3.6         
#> 11 carb  Number of carburetors quantile 25% 1.0         2.2         
#> 12 carb  Number of carburetors quantile 75% 2.0         4.0         

#margin and totals, multiple by
crosstable(mtcars2, c(disp, cyl), by=c(am, vs),
           margin=c("row", "col"), total = "both")
#> # A tibble: 8 × 8
#>   .id   label             variable am=auto & vs=straigh…¹ am=manual & vs=strai…²
#>   <chr> <chr>             <chr>    <chr>                  <chr>                 
#> 1 disp  Displacement (cu… Min / M… 120.1 / 258.0          71.1 / 121.0          
#> 2 disp  Displacement (cu… Med [IQ… 167.6 [143.8;196.3]    79.0 [77.2;101.5]     
#> 3 disp  Displacement (cu… Mean (s… 175.1 (49.1)           89.8 (18.8)           
#> 4 disp  Displacement (cu… N (NA)   7 (0)                  7 (0)                 
#> 5 cyl   Number of cylind… 4        3 (42.86% / 27.27%)    7 (100.00% / 63.64%)  
#> 6 cyl   Number of cylind… 6        4 (57.14% / 57.14%)    0 (0% / 0%)           
#> 7 cyl   Number of cylind… 8        0 (0% / 0%)            0 (0% / 0%)           
#> 8 cyl   Number of cylind… Total    7 (21.88%)             7 (21.88%)            
#> # ℹ abbreviated names: ¹​`am=auto & vs=straight`, ²​`am=manual & vs=straight`
#> # ℹ 3 more variables: `am=auto & vs=vshaped` <chr>,
#> #   `am=manual & vs=vshaped` <chr>, Total <chr>

#predicate selection, correlation, effect calculation
crosstable(mtcars2, where(is.numeric), by=hp, effect=TRUE)
#> # A tibble: 6 × 4
#>   .id   label                 variable `Gross horsepower`           
#>   <chr> <chr>                 <chr>    <chr>                        
#> 1 mpg   Miles/(US) gallon     pearson  "-0.78 \n95%CI [-0.89;-0.59]"
#> 2 disp  Displacement (cu.in.) pearson  "0.79 \n95%CI [0.61;0.89]"   
#> 3 drat  Rear axle ratio       pearson  "-0.45 \n95%CI [-0.69;-0.12]"
#> 4 wt    Weight (1000 lbs)     pearson  "0.66 \n95%CI [0.4;0.82]"    
#> 5 qsec  1/4 mile time         pearson  "-0.71 \n95%CI [-0.85;-0.48]"
#> 6 carb  Number of carburetors pearson  "0.75 \n95%CI [0.54;0.87]"   

#lambda selection & statistical tests
crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, test=TRUE)
#> # A tibble: 8 × 6
#>   .id   label                 variable   straight           vshaped        test 
#>   <chr> <chr>                 <chr>      <chr>              <chr>          <chr>
#> 1 disp  Displacement (cu.in.) Min / Max  71.1 / 258.0       120.3 / 472.0  "p v…
#> 2 disp  Displacement (cu.in.) Med [IQR]  120.5 [83.0;162.4] 311.0 [275.8;… "p v…
#> 3 disp  Displacement (cu.in.) Mean (std) 132.5 (56.9)       307.1 (106.8)  "p v…
#> 4 disp  Displacement (cu.in.) N (NA)     14 (0)             18 (0)         "p v…
#> 5 hp    Gross horsepower      Min / Max  52.0 / 123.0       91.0 / 335.0   "p v…
#> 6 hp    Gross horsepower      Med [IQR]  96.0 [66.0;109.8]  180.0 [156.2;… "p v…
#> 7 hp    Gross horsepower      Mean (std) 91.4 (24.4)        189.7 (60.3)   "p v…
#> 8 hp    Gross horsepower      N (NA)     14 (0)             18 (0)         "p v…

#Dates
mtcars2$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
crosstable(mtcars2, my_date, by=vs, date_format="%d/%m/%Y")
#> # A tibble: 4 × 5
#>   .id     label              variable   straight                         vshaped
#>   <chr>   <chr>              <chr>      <chr>                            <chr>  
#> 1 my_date Some nonsense date Min / Max  22/02/2010 - 04/05/2010          02/04/…
#> 2 my_date Some nonsense date Med [IQR]  07/04/2010 [08/03/2010;21/04/20… 30/06/…
#> 3 my_date Some nonsense date Mean (std) 02/04/2010 (24.4 days)           09/07/…
#> 4 my_date Some nonsense date N (NA)     14 (0)                           18 (0) 

#Survival data (using formula syntax)
library(survival)
crosstable(aml, Surv(time, status) ~ x, times=c(0,15,30,150), followup=TRUE)
#> # A tibble: 6 × 5
#>   .id                label              variable        Maintained Nonmaintained
#>   <chr>              <chr>              <chr>           <chr>      <chr>        
#> 1 Surv(time, status) Surv(time, status) t=0             1.00 (0/1… 1.00 (0/12)  
#> 2 Surv(time, status) Surv(time, status) t=15            0.82 (2/8) 0.58 (5/7)   
#> 3 Surv(time, status) Surv(time, status) t=30            0.61 (2/5) 0.29 (3/4)   
#> 4 Surv(time, status) Surv(time, status) t=150           0.18 (3/1) 0 (3/0)      
#> 5 Surv(time, status) Surv(time, status) Median follow … 103 [13 ;… NA [16 ; 45] 
#> 6 Surv(time, status) Surv(time, status) Median survival 31         23           

#Patterns
crosstable(mtcars2, vs, by=am, percent_digits=0,
           percent_pattern="{n} ({p_col} / {p_row})")
#> # A tibble: 2 × 5
#>   .id   label  variable auto           manual       
#>   <chr> <chr>  <chr>    <chr>          <chr>        
#> 1 vs    Engine straight 7 (37% / 50%)  7 (54% / 50%)
#> 2 vs    Engine vshaped  12 (63% / 67%) 6 (46% / 33%)
crosstable(mtcars2, vs, by=am, percent_digits=0,
           percent_pattern="N={n} \np[95%CI] = {p_col} [{p_col_inf}; {p_col_sup}]")
#> # A tibble: 2 × 5
#>   .id   label  variable auto                               manual               
#>   <chr> <chr>  <chr>    <chr>                              <chr>                
#> 1 vs    Engine straight "N=7 \np[95%CI] = 37% [19%; 59%]"  "N=7 \np[95%CI] = 54…
#> 2 vs    Engine vshaped  "N=12 \np[95%CI] = 63% [41%; 81%]" "N=6 \np[95%CI] = 46…
str_high="n>5"; str_lo="n<=5"
crosstable(mtcars2, vs, by=am, percent_digits=0,
           percent_pattern="col={p_col}, row={p_row} ({ifelse(n<5, str_lo, str_high)})")
#> Error in map(.x, .f, ..., .progress = .progress): ℹ In index: 1.
#> ℹ With name: body.
#> Caused by error in `crosstable()`:
#> ! Could not resolve a variable used in `percent_pattern`.
#> ℹ Authorized variables are `n`, `n_tot`, `n_row`, `n_col`, `p_tot`, `p_row`,
#>   and `p_col`, along with `p_xxx_inf` and `p_xxx_sup` for proportions.
#> ℹ Provided `percent_pattern`: `col={p_col}, row={p_row} ({ifelse(n<5, str_lo,
#>   str_high)})`
#> ✖ Error: "Failed to evaluate glue component {ifelse(n<5, str_lo, str_high)}"
```
