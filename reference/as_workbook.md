# Converts a `crosstable` object into a formatted, savable `openxlsx` workbook.

Converts a `crosstable` object into a formatted, savable `openxlsx`
workbook.

## Usage

``` r
as_workbook(
  x,
  show_test_name = TRUE,
  by_header = NULL,
  keep_id = FALSE,
  generic_labels = list(id = ".id", variable = "variable", value = "value", total =
    "Total", label = "label", test = "test", effect = "effect"),
  ...
)
```

## Arguments

- x:

  the result of
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  or a list of crosstables

- show_test_name:

  in the `test` column, show the test name

- by_header:

  a string to override the `by` header

- keep_id:

  whether to keep the `.id` column

- generic_labels:

  names of the crosstable default columns

- ...:

  unused

## Value

an `openxlsx` workbook containing the crosstable(s)

## Author

Dan Chaltiel

## Examples

``` r
library(openxlsx)
target = tempfile(fileext=".xlsx")

x=crosstable(mtcars2, c(mpg, vs, gear), total=TRUE, test=TRUE)
as_workbook(x, keep_id=TRUE) %>%
    saveWorkbook(file=target)
if(interactive()) browseURL(target)

target = tempfile(fileext=".xlsx")
x2=list(iris=crosstable(iris2), crosstable(mtcars2))
as_workbook(x2, keep_id=TRUE) %>%
    saveWorkbook(file=target)
if(interactive()) browseURL(target)
```
