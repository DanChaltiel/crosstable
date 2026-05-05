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


.cl-15734a1c{table-layout:auto;}.cl-156a8544{font-family:'DejaVu Sans';font-size:14pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-156a856c{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-156e200a{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-156e2014{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-156e201e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-156e56ce{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(0, 0, 0, 1.00);border-top: 1.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-156e56d8{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-156e56d9{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-156e56da{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-156e56e2{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-156e56e3{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-156e56e4{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


label
```
