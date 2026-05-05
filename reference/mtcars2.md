# Modified `mtcars` dataset

Modified `mtcars` dataset so:

- every column is labelled (using `label` attribute)

- rownames are a character column named `model`

- `gear` and `cyl` columns are considered as numerical factors

- `vs` and `am` columns are considered as character vector

See [`mtcars`](https://rdrr.io/r/datasets/mtcars.html) for more
informations on the original "Motor Trend Car Road Tests" dataset.

## Usage

``` r
mtcars2
```

## Format

A data frame with 32 observations on 11 variables with labels.

## Source


    library(dplyr)
    mtcars2 = mtcars %>%
        mutate(
           model=rownames(mtcars),
           vs=ifelse(vs==0, "vshaped", "straight"),
           am=ifelse(am==0, "auto", "manual"),
           across(c("cyl", "gear"), factor),
           .before=1
        ) %>%
        expss::apply_labels( #I also could have used [import_labels] or even `labelled::set_variable_labels()`
            mpg="Miles/(US) gallon",
            cyl="Number of cylinders",
            disp="Displacement (cu.in.)",
            hp="Gross horsepower",
            drat="Rear axle ratio",
            wt="Weight (1000 lbs)",
            qsec="1/4 mile time",
            vs="Engine",
            am="Transmission",
            gear="Number of forward gears",
           carb="Number of carburetors"
        )

## Examples

``` r
library(crosstable)
ct=crosstable(mtcars2, by=vs)
ct
#> # A tibble: 76 × 5
#>    .id   label variable           straight vshaped 
#>    <chr> <chr> <chr>              <chr>    <chr>   
#>  1 model Model AMC Javelin        0 (0%)   1 (100%)
#>  2 model Model Cadillac Fleetwood 0 (0%)   1 (100%)
#>  3 model Model Camaro Z28         0 (0%)   1 (100%)
#>  4 model Model Chrysler Imperial  0 (0%)   1 (100%)
#>  5 model Model Datsun 710         1 (100%) 0 (0%)  
#>  6 model Model Dodge Challenger   0 (0%)   1 (100%)
#>  7 model Model Duster 360         0 (0%)   1 (100%)
#>  8 model Model Ferrari Dino       0 (0%)   1 (100%)
#>  9 model Model Fiat 128           1 (100%) 0 (0%)  
#> 10 model Model Fiat X1-9          1 (100%) 0 (0%)  
#> # ℹ 66 more rows
as_flextable(ct)


.cl-7df69204{table-layout:auto;}.cl-7dee47fc{font-family:'DejaVu Sans';font-size:14pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7dee4810{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7df18156{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-7df1816a{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-7df18174{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-7df1a406{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a410{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a41a{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a41b{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a41c{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a424{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a42e{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a42f{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7df1a430{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



label
```
