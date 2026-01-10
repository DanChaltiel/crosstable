# Turns a `crosstable` object into a formatted `flextable`

Turns a `crosstable` object into a formatted `flextable`

## Usage

``` r
# S3 method for class 'crosstable'
as_flextable(
  x,
  keep_id = FALSE,
  ...,
  by_header = NULL,
  autofit = TRUE,
  compact = FALSE,
  collapse = NULL,
  show_test_name = TRUE,
  allow_breaks = FALSE,
  fontsizes = list(body = 11, subheaders = 11, header = 11),
  padding_v = NULL,
  remove_header_keys = TRUE,
  header_show_n = FALSE,
  header_show_n_pattern = "{.col} (N={.n})",
  generic_labels = list(id = ".id", variable = "variable", value = "value", total =
    "Total", label = "label", test = "test", effect = "effect")
)

as_flextable(x, ...)
```

## Arguments

- x:

  the result of
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md).

- keep_id:

  whether to keep the `.id` column.

- ...:

  unused.

- by_header:

  a string to override the header if `x` has only one `by` stratum.

- autofit:

  whether to automatically adjust the table. Can also be a function.

- compact:

  whether to compact the table. If `TRUE`, see
  [`ct_compact.crosstable()`](https://danchaltiel.github.io/crosstable/reference/ct_compact.md)
  to see how to use `keep_id`.

- collapse:

  levels to collapse when in compact format. Ignored if `compact!=TRUE`.
  See examples.

- show_test_name:

  in the `test` column, show the test name.

- allow_breaks:

  whether to allow the table to break on .

- fontsizes:

  font sizes as a list of keys. Default to
  `list(body=11, subheaders=11, header=11)`. If set through arguments
  instead of options, all 3 names should be specified.

- padding_v:

  vertical padding (body).

- remove_header_keys:

  if `TRUE` and `x` has several `by` strata, header will only display
  values.

- header_show_n:

  numeric vector telling on which depth the group size should be
  indicated in the header. You can control the pattern using option
  `crosstable_options`. See
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  for details about it. See example for use case.

- header_show_n_pattern:

  glue pattern used when `header_show_n==TRUE`. `.col` is the name of
  the column and `.n` the size of the group. Default to
  `{.col} (N={.n})`; you can also use `{.col_key}` and `{.col_val}` when
  `by` has multiple stratum. To control the "Total" column, enter this
  as a `list` with names "cell" and "total".

- generic_labels:

  names of the crosstable default columns. Useful for translation for
  instance.

## Value

a flextable.

## Methods (by class)

- `as_flextable(crosstable)`: Turns a `crosstable` object into a
  formatted `flextable`.

## See also

[`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md),
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html),
[`as_gt.crosstable()`](https://danchaltiel.github.io/crosstable/reference/as_gt.md)

## Author

Dan Chaltiel

## Examples

``` r
crosstable_options(crosstable_fontsize_header=14,
                   crosstable_fontsize_subheaders=10,
                   crosstable_fontsize_body=8)
crosstable(iris) %>% as_flextable()


.cl-e42c33bc{table-layout:auto;}.cl-e423e748{font-family:'DejaVu Sans';font-size:14pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e423e75c{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e42749d8{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e42749ec{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e42749ed{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e4277c96{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e4277ca0{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e4277caa{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e4277cab{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e4277cb4{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e4277cb5{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e4277cbe{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


label
```

variable

value

Sepal.Length

Min / Max

4.3 / 7.9

Med \[IQR\]

5.8 \[5.1;6.4\]

Mean (std)

5.8 (0.8)

N (NA)

150 (0)

Sepal.Width

Min / Max

2.0 / 4.4

Med \[IQR\]

3.0 \[2.8;3.3\]

Mean (std)

3.1 (0.4)

N (NA)

150 (0)

Petal.Length

Min / Max

1.0 / 6.9

Med \[IQR\]

4.3 \[1.6;5.1\]

Mean (std)

3.8 (1.8)

N (NA)

150 (0)

Petal.Width

Min / Max

0.1 / 2.5

Med \[IQR\]

1.3 \[0.3;1.8\]

Mean (std)

1.2 (0.8)

N (NA)

150 (0)

Species

setosa

50 (33.33%)

versicolor

50 (33.33%)

virginica

50 (33.33%)

[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(mtcars2,
-model, by=[c](https://rdrr.io/r/base/c.html)(am, vs))
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
as_flextable(header_show_n=1)

| Engine                  |             | straight                                               |                                                                 | vshaped                                                         |                                                                 |
|-------------------------|-------------|--------------------------------------------------------|-----------------------------------------------------------------|-----------------------------------------------------------------|-----------------------------------------------------------------|
| Transmission            |             | auto (N=7)                                             | manual (N=7)                                                    | auto (N=12)                                                     | manual (N=6)                                                    |
| Miles/(US) gallon       | Min / Max   | 17.8 / 24.4                                            | 21.4 / 33.9                                                     | 10.4 / 19.2                                                     | 15.0 / 26.0                                                     |
|                         | Med \[IQR\] | 21.4 \[18.6;22.1\]                                     | 30.4 \[25.1;31.4\]                                              | 15.2 \[14.1;16.6\]                                              | 20.4 \[16.8;21.0\]                                              |
|                         | Mean (std)  | 20.7 (2.5)                                             | 28.4 (4.8)                                                      | 15.1 (2.8)                                                      | 19.8 (4.0)                                                      |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Number of cylinders     | 4           | 3 (27.27%)                                             | 7 (63.64%)                                                      | 0 (0%)                                                          | 1 (9.09%)                                                       |
|                         | 6           | 4 (57.14%)                                             | 0 (0%)                                                          | 0 (0%)                                                          | 3 (42.86%)                                                      |
|                         | 8           | 0 (0%)                                                 | 0 (0%)                                                          | 12 (85.71%)                                                     | 2 (14.29%)                                                      |
| Displacement (cu.in.)   | Min / Max   | 120.1 / 258.0                                          | 71.1 / 121.0                                                    | 275.8 / 472.0                                                   | 120.3 / 351.0                                                   |
|                         | Med \[IQR\] | 167.6 \[143.8;196.3\]                                  | 79.0 \[77.2;101.5\]                                             | 355.0 \[296.9;410.0\]                                           | 160.0 \[148.8;265.8\]                                           |
|                         | Mean (std)  | 175.1 (49.1)                                           | 89.8 (18.8)                                                     | 357.6 (71.8)                                                    | 206.2 (95.2)                                                    |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Gross horsepower        | Min / Max   | 62.0 / 123.0                                           | 52.0 / 113.0                                                    | 150.0 / 245.0                                                   | 91.0 / 335.0                                                    |
|                         | Med \[IQR\] | 105.0 \[96.0;116.5\]                                   | 66.0 \[65.5;101.0\]                                             | 180.0 \[175.0;218.8\]                                           | 142.5 \[110.0;241.8\]                                           |
|                         | Mean (std)  | 102.1 (20.9)                                           | 80.6 (24.1)                                                     | 194.2 (33.4)                                                    | 180.8 (98.8)                                                    |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Rear axle ratio         | Min / Max   | 2.8 / 3.9                                              | 3.8 / 4.9                                                       | 2.8 / 3.7                                                       | 3.5 / 4.4                                                       |
|                         | Med \[IQR\] | 3.7 \[3.4;3.9\]                                        | 4.1 \[4.0;4.2\]                                                 | 3.1 \[3.1;3.2\]                                                 | 3.9 \[3.7;4.1\]                                                 |
|                         | Mean (std)  | 3.6 (0.5)                                              | 4.1 (0.4)                                                       | 3.1 (0.2)                                                       | 3.9 (0.3)                                                       |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Weight (1000 lbs)       | Min / Max   | 2.5 / 3.5                                              | 1.5 / 2.8                                                       | 3.4 / 5.4                                                       | 2.1 / 3.6                                                       |
|                         | Med \[IQR\] | 3.2 \[3.2;3.4\]                                        | 1.9 \[1.7;2.3\]                                                 | 3.8 \[3.6;4.4\]                                                 | 2.8 \[2.7;3.1\]                                                 |
|                         | Mean (std)  | 3.2 (0.3)                                              | 2.0 (0.4)                                                       | 4.1 (0.8)                                                       | 2.9 (0.5)                                                       |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| 1/4 mile time           | Min / Max   | 18.3 / 22.9                                            | 16.9 / 19.9                                                     | 15.4 / 18.0                                                     | 14.5 / 17.0                                                     |
|                         | Med \[IQR\] | 20.0 \[19.2;20.1\]                                     | 18.6 \[18.6;19.2\]                                              | 17.4 \[17.0;17.7\]                                              | 16.0 \[14.8;16.6\]                                              |
|                         | Mean (std)  | 20.0 (1.5)                                             | 18.7 (0.9)                                                      | 17.1 (0.8)                                                      | 15.8 (1.1)                                                      |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Number of forward gears | 3           | 3 (20.00%)                                             | 0 (0%)                                                          | 12 (80.00%)                                                     | 0 (0%)                                                          |
|                         | 4           | 4 (33.33%)                                             | 6 (50.00%)                                                      | 0 (0%)                                                          | 2 (16.67%)                                                      |
|                         | 5           | 0 (0%)                                                 | 1 (20.00%)                                                      | 0 (0%)                                                          | 4 (80.00%)                                                      |
| Number of carburetors   | Min / Max   | 1.0 / 4.0                                              | 1.0 / 2.0                                                       | 2.0 / 4.0                                                       | 2.0 / 8.0                                                       |
|                         | Med \[IQR\] | 2.0 \[1.0;3.0\]                                        | 1.0 \[1.0;2.0\]                                                 | 3.0 \[2.0;4.0\]                                                 | 4.0 \[4.0;5.5\]                                                 |
|                         | Mean (std)  | 2.1 (1.3)                                              | 1.4 (0.5)                                                       | 3.1 (0.9)                                                       | 4.7 (2.1)                                                       |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Some nonsense date      | Min / Max   | 2010-03-04 - 2010-05-04                                | 2010-02-22 - 2010-04-24                                         | 2010-05-31 - 2010-09-03                                         | 2010-04-02 - 2010-12-02                                         |
|                         | Med \[IQR\] | 2010-04-16 \[2010-04-06;2010-05-04\]                   | 2010-03-08 \[2010-03-07;2010-04-20\]                            | 2010-06-30 \[2010-06-25;2010-08-04\]                            | 2010-05-23 \[2010-04-21;2010-09-22\]                            |
|                         | Mean (std)  | 2010-04-13 (20.9 days)                                 | 2010-03-22 (24.1 days)                                          | 2010-07-14 (1.1 months)                                         | 2010-06-30 (3.2 months)                                         |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |
| Date+time               | Min / Max   | 2010-01-19 07:12:00 - 2010-01-23 21:36:00              | 2010-01-17 21:36:00 - 2010-01-20 21:36:00                       | 2010-01-16 09:50:24 - 2010-01-19                                | 2010-01-15 12:00:00 - 2010-01-18 00:28:48                       |
|                         | Med \[IQR\] | 2010-01-21 \[2010-01-19 21:36:00;2010-01-21 05:16:48\] | 2010-01-19 14:38:24 \[2010-01-19 12:28:48;2010-01-20 11:16:48\] | 2010-01-18 08:24:00 \[2010-01-17 20:52:48;2010-01-18 14:24:00\] | 2010-01-16 23:31:12 \[2010-01-15 14:24:00;2010-01-17 16:48:00\] |
|                         | Mean (std)  | 2010-01-20 23:12:41 (1.5 days)                         | 2010-01-19 16:48:00 (22.7 hours)                                | 2010-01-18 03:25:12 (19.2 hours)                                | 2010-01-16 19:07:12 (1.1 days)                                  |
|                         | N (NA)      | 7 (0)                                                  | 7 (0)                                                           | 12 (0)                                                          | 6 (0)                                                           |

[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(mtcars2,
cols=[c](https://rdrr.io/r/base/c.html)(mpg, cyl), by=am, effect=TRUE)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
as_flextable(keep_id=TRUE, autofit=FALSE)

[TABLE]

[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(mtcars2,
cols=[c](https://rdrr.io/r/base/c.html)(mpg, cyl), by=am, effect=TRUE,
total=TRUE) [%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
as_flextable(compact=TRUE, header_show_n=TRUE,
header_show_n_pattern=[list](https://rdrr.io/r/base/list.html)(cell="{.col}
(N={.n})", total="Total\n(N={.n})")) \#\> Error in mutate(.,
across(-any_of(c(id_from, name_to)), ~if_else(.data\[\[id_from\]\] %in%
collapse_grp, .x\[.data\[\[name_to\]\] == collapse\] %0% NA, .x))): ℹ In
argument: \`across(...)\`. \#\> Caused by error in \`across()\`: \#\> !
Can't compute column \`auto\`. \#\> Caused by error in \`if_else()\`:
\#\> ! could not find function "if_else" \#collapse levels mtcars2
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html) mutate(am_man
= fct_recode(am, "Yes"="manual", "No"="auto"))
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[apply_labels](https://danchaltiel.github.io/crosstable/reference/apply_labels.md)(am_man="Manual
transmission")
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)([c](https://rdrr.io/r/base/c.html)(mpg,
am_man, hp), by=vs, test=T, effect=T)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
as_flextable(compact=TRUE, collapse="Yes") \#\> Error in mutate(.,
am_man = fct_recode(am, Yes = "manual", No = "auto")): could not find
function "mutate" \#Renaming (because why not?)
[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(mtcars2,
am, by=vs, total="both", test=TRUE, effect=TRUE)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
dplyr::[rename](https://dplyr.tidyverse.org/reference/rename.html)(ID=.id,
math=variable, Tot=Total, lab=label, pval=test, fx=effect)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
as_flextable(by_header = "Engine shape",
generic_labels=[list](https://rdrr.io/r/base/list.html)(id = "ID",
variable = "math", total="Tot", label = "lab", test = "pval",
effect="fx")) \#\> Warning: Be aware that automatic global testing
should only be done in an exploratory \#\> context, as it would cause
extensive alpha inflation otherwise. \#\> This warning is displayed once
every 8 hours.

[TABLE]
