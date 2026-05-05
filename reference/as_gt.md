# Converts a `crosstable` object into a formatted `gt` table.

Converts a `crosstable` object into a formatted `gt` table.

Method to convert an object to a `gt` table

Default method to convert an object to a `gt` table

## Usage

``` r
# S3 method for class 'crosstable'
as_gt(
  x,
  show_test_name = TRUE,
  by_header = NULL,
  keep_id = FALSE,
  generic_labels = list(id = ".id", variable = "variable", value = "value", total =
    "Total", label = "label", test = "test", effect = "effect"),
  ...
)

as_gt(x, ...)

# Default S3 method
as_gt(x, ...)
```

## Arguments

- x:

  object to be converted

- show_test_name:

  in the `test` column, show the test name

- by_header:

  a string to override the `by` header

- keep_id:

  whether to keep the `.id` column

- generic_labels:

  names of the crosstable default columns

- ...:

  arguments for custom methods

## Value

a formatted `gt` table

## Methods (by class)

- `as_gt(crosstable)`: For crosstables

- `as_gt(default)`: default function

## See also

[`as_flextable.crosstable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)

[`gt::gt()`](https://gt.rstudio.com/reference/gt.html)

## Author

Dan Chaltiel

## Examples

``` r
xx = mtcars2 %>% dplyr::select(2:10)
crosstable(xx) %>% as_gt


  
```
