# Open a `crosstable` in a temporary document

This eases copy-pasting

## Usage

``` r
peek(x, docx = getOption("crosstable_peek_docx", TRUE), ...)
```

## Arguments

- x:

  a crosstable

- docx:

  if true, peek as a `docx`, else, peek as `xlsx`

- ...:

  passed on to
  [`as_flextable.crosstable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  or to
  [`as_workbook()`](https://danchaltiel.github.io/crosstable/reference/as_workbook.md)

## Value

Nothing, called for its side effects

## Author

Dan Chaltiel
