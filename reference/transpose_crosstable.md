# Transpose a crosstable

Pivot a crosstable so the `label` column is swapped with the `by` row.
This requires the `variable` column to be the same for every data
column, like when all columns are numeric of when all columns are
factors with the same levels

## Usage

``` r
transpose_crosstable(x)

# S3 method for class 'crosstable'
t(x)
```

## Arguments

- x:

  a crosstable

## Value

a tibble of class `transposed_crosstable`

## Examples

``` r
ct = crosstable(mtcars2, c(mpg, drat, wt, qsec), by=am)
ct %>% t() %>% as_flextable()


.cl-cf1f7bf0{table-layout:auto;}.cl-cf18d94e{font-family:'DejaVu Sans';font-size:14pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-cf18d962{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-cf1ba214{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-cf1ba21e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-cf1ba21f{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-cf1bc398{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3a2{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3a3{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3ac{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3ad{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3ae{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3b6{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cf1bc3b7{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



am
```

variable

Columns

Miles/(US) gallon

Rear axle ratio

Weight (1000 lbs)

1/4 mile time

auto

Min / Max

10.4 / 24.4

2.8 / 3.9

2.5 / 5.4

15.4 / 22.9

Med \[IQR\]

17.3 \[14.9;19.2\]

3.1 \[3.1;3.7\]

3.5 \[3.4;3.8\]

17.8 \[17.2;19.2\]

Mean (std)

17.1 (3.8)

3.3 (0.4)

3.8 (0.8)

18.2 (1.8)

N (NA)

19 (0)

19 (0)

19 (0)

19 (0)

manual

Min / Max

15.0 / 33.9

3.5 / 4.9

1.5 / 3.6

14.5 / 19.9

Med \[IQR\]

22.8 \[21.0;30.4\]

4.1 \[3.9;4.2\]

2.3 \[1.9;2.8\]

17.0 \[16.5;18.6\]

Mean (std)

24.4 (6.2)

4.0 (0.4)

2.4 (0.6)

17.4 (1.8)

N (NA)

13 (0)

13 (0)

13 (0)

13 (0)

ct2 =
[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(mtcars2,
[c](https://rdrr.io/r/base/c.html)(mpg, drat, wt, qsec),
by=[c](https://rdrr.io/r/base/c.html)(am, vs)) ct2
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[t](https://rdrr.io/r/base/t.html)()
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[as_flextable](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)()

| label                   | variable    | Columns            |                 |                   |                    |
|-------------------------|-------------|--------------------|-----------------|-------------------|--------------------|
|                         |             | Miles/(US) gallon  | Rear axle ratio | Weight (1000 lbs) | 1/4 mile time      |
| am=auto & vs=straight   | Min / Max   | 17.8 / 24.4        | 2.8 / 3.9       | 2.5 / 3.5         | 18.3 / 22.9        |
| am=auto & vs=straight   | Med \[IQR\] | 21.4 \[18.6;22.1\] | 3.7 \[3.4;3.9\] | 3.2 \[3.2;3.4\]   | 20.0 \[19.2;20.1\] |
| am=auto & vs=straight   | Mean (std)  | 20.7 (2.5)         | 3.6 (0.5)       | 3.2 (0.3)         | 20.0 (1.5)         |
| am=auto & vs=straight   | N (NA)      | 7 (0)              | 7 (0)           | 7 (0)             | 7 (0)              |
| am=manual & vs=straight | Min / Max   | 21.4 / 33.9        | 3.8 / 4.9       | 1.5 / 2.8         | 16.9 / 19.9        |
| am=manual & vs=straight | Med \[IQR\] | 30.4 \[25.1;31.4\] | 4.1 \[4.0;4.2\] | 1.9 \[1.7;2.3\]   | 18.6 \[18.6;19.2\] |
| am=manual & vs=straight | Mean (std)  | 28.4 (4.8)         | 4.1 (0.4)       | 2.0 (0.4)         | 18.7 (0.9)         |
| am=manual & vs=straight | N (NA)      | 7 (0)              | 7 (0)           | 7 (0)             | 7 (0)              |
| am=auto & vs=vshaped    | Min / Max   | 10.4 / 19.2        | 2.8 / 3.7       | 3.4 / 5.4         | 15.4 / 18.0        |
| am=auto & vs=vshaped    | Med \[IQR\] | 15.2 \[14.1;16.6\] | 3.1 \[3.1;3.2\] | 3.8 \[3.6;4.4\]   | 17.4 \[17.0;17.7\] |
| am=auto & vs=vshaped    | Mean (std)  | 15.1 (2.8)         | 3.1 (0.2)       | 4.1 (0.8)         | 17.1 (0.8)         |
| am=auto & vs=vshaped    | N (NA)      | 12 (0)             | 12 (0)          | 12 (0)            | 12 (0)             |
| am=manual & vs=vshaped  | Min / Max   | 15.0 / 26.0        | 3.5 / 4.4       | 2.1 / 3.6         | 14.5 / 17.0        |
| am=manual & vs=vshaped  | Med \[IQR\] | 20.4 \[16.8;21.0\] | 3.9 \[3.7;4.1\] | 2.8 \[2.7;3.1\]   | 16.0 \[14.8;16.6\] |
| am=manual & vs=vshaped  | Mean (std)  | 19.8 (4.0)         | 3.9 (0.3)       | 2.9 (0.5)         | 15.8 (1.1)         |
| am=manual & vs=vshaped  | N (NA)      | 6 (0)              | 6 (0)           | 6 (0)             | 6 (0)              |
