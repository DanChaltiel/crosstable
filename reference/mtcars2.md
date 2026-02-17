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
#>    .id   label variable           straight    vshaped    
#>    <chr> <chr> <chr>              <chr>       <chr>      
#>  1 model Model AMC Javelin        0 (0%)      1 (100.00%)
#>  2 model Model Cadillac Fleetwood 0 (0%)      1 (100.00%)
#>  3 model Model Camaro Z28         0 (0%)      1 (100.00%)
#>  4 model Model Chrysler Imperial  0 (0%)      1 (100.00%)
#>  5 model Model Datsun 710         1 (100.00%) 0 (0%)     
#>  6 model Model Dodge Challenger   0 (0%)      1 (100.00%)
#>  7 model Model Duster 360         0 (0%)      1 (100.00%)
#>  8 model Model Ferrari Dino       0 (0%)      1 (100.00%)
#>  9 model Model Fiat 128           1 (100.00%) 0 (0%)     
#> 10 model Model Fiat X1-9          1 (100.00%) 0 (0%)     
#> # ℹ 66 more rows
as_flextable(ct)


.cl-126a692a{table-layout:auto;}.cl-126235e8{font-family:'DejaVu Sans';font-size:14pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-126235f2{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-126558fe{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-12655908{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-12655912{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-12657ba4{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bb8{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bb9{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bc2{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bcc{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bcd{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bd6{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657bd7{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-12657be0{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



label
```

variable

Engine

straight

vshaped

Model

AMC Javelin

0 (0%)

1 (100.00%)

Cadillac Fleetwood

0 (0%)

1 (100.00%)

Camaro Z28

0 (0%)

1 (100.00%)

Chrysler Imperial

0 (0%)

1 (100.00%)

Datsun 710

1 (100.00%)

0 (0%)

Dodge Challenger

0 (0%)

1 (100.00%)

Duster 360

0 (0%)

1 (100.00%)

Ferrari Dino

0 (0%)

1 (100.00%)

Fiat 128

1 (100.00%)

0 (0%)

Fiat X1-9

1 (100.00%)

0 (0%)

Ford Pantera L

0 (0%)

1 (100.00%)

Honda Civic

1 (100.00%)

0 (0%)

Hornet 4 Drive

1 (100.00%)

0 (0%)

Hornet Sportabout

0 (0%)

1 (100.00%)

Lincoln Continental

0 (0%)

1 (100.00%)

Lotus Europa

1 (100.00%)

0 (0%)

Maserati Bora

0 (0%)

1 (100.00%)

Mazda RX4

0 (0%)

1 (100.00%)

Mazda RX4 Wag

0 (0%)

1 (100.00%)

Merc 230

1 (100.00%)

0 (0%)

Merc 240D

1 (100.00%)

0 (0%)

Merc 280

1 (100.00%)

0 (0%)

Merc 280C

1 (100.00%)

0 (0%)

Merc 450SE

0 (0%)

1 (100.00%)

Merc 450SL

0 (0%)

1 (100.00%)

Merc 450SLC

0 (0%)

1 (100.00%)

Pontiac Firebird

0 (0%)

1 (100.00%)

Porsche 914-2

0 (0%)

1 (100.00%)

Toyota Corolla

1 (100.00%)

0 (0%)

Toyota Corona

1 (100.00%)

0 (0%)

Valiant

1 (100.00%)

0 (0%)

Volvo 142E

1 (100.00%)

0 (0%)

Miles/(US) gallon

Min / Max

17.8 / 33.9

10.4 / 26.0

Med \[IQR\]

22.8 \[21.4;29.6\]

15.7 \[14.8;19.1\]

Mean (std)

24.6 (5.4)

16.6 (3.9)

N (NA)

14 (0)

18 (0)

Number of cylinders

4

10 (90.91%)

1 (9.09%)

6

4 (57.14%)

3 (42.86%)

8

0 (0%)

14 (100.00%)

Displacement (cu.in.)

Min / Max

71.1 / 258.0

120.3 / 472.0

Med \[IQR\]

120.5 \[83.0;162.4\]

311.0 \[275.8;360.0\]

Mean (std)

132.5 (56.9)

307.1 (106.8)

N (NA)

14 (0)

18 (0)

Gross horsepower

Min / Max

52.0 / 123.0

91.0 / 335.0

Med \[IQR\]

96.0 \[66.0;109.8\]

180.0 \[156.2;226.2\]

Mean (std)

91.4 (24.4)

189.7 (60.3)

N (NA)

14 (0)

18 (0)

Rear axle ratio

Min / Max

2.8 / 4.9

2.8 / 4.4

Med \[IQR\]

3.9 \[3.7;4.1\]

3.2 \[3.1;3.7\]

Mean (std)

3.9 (0.5)

3.4 (0.5)

N (NA)

14 (0)

18 (0)

Weight (1000 lbs)

Min / Max

1.5 / 3.5

2.1 / 5.4

Med \[IQR\]

2.6 \[2.0;3.2\]

3.6 \[3.2;3.8\]

Mean (std)

2.6 (0.7)

3.7 (0.9)

N (NA)

14 (0)

18 (0)

1/4 mile time

Min / Max

16.9 / 22.9

14.5 / 18.0

Med \[IQR\]

19.2 \[18.6;20.0\]

17.0 \[16.0;17.4\]

Mean (std)

19.3 (1.4)

16.7 (1.1)

N (NA)

14 (0)

18 (0)

Transmission

auto

7 (36.84%)

12 (63.16%)

manual

7 (53.85%)

6 (46.15%)

Number of forward gears

3

3 (20.00%)

12 (80.00%)

4

10 (83.33%)

2 (16.67%)

5

1 (20.00%)

4 (80.00%)

Number of carburetors

Min / Max

1.0 / 4.0

2.0 / 8.0

Med \[IQR\]

1.5 \[1.0;2.0\]

4.0 \[2.2;4.0\]

Mean (std)

1.8 (1.1)

3.6 (1.5)

N (NA)

14 (0)

18 (0)

Some nonsense date

Min / Max

2010-02-22 - 2010-05-04

2010-04-02 - 2010-12-02

Med \[IQR\]

2010-04-07 \[2010-03-08;2010-04-21\]

2010-06-30 \[2010-05-31;2010-08-19\]

Mean (std)

2010-04-02 (24.4 days)

2010-07-09 (2.0 months)

N (NA)

14 (0)

18 (0)

Date+time

Min / Max

2010-01-17 21:36:00 - 2010-01-23 21:36:00

2010-01-15 12:00:00 - 2010-01-19

Med \[IQR\]

2010-01-20 04:04:48 \[2010-01-19 14:24:00;2010-01-21 00:00:00\]

2010-01-18 00:28:48 \[2010-01-16 20:09:36;2010-01-18 10:04:48\]

Mean (std)

2010-01-20 08:00:20 (1.4 days)

2010-01-17 16:39:12 (1.1 days)

N (NA)

14 (0)

18 (0)
