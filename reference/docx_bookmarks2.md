# List Word bookmarks, including the ones in header and footer

This is a correction of
[`officer::docx_bookmarks()`](https://davidgohel.github.io/officer/reference/docx_bookmarks.html).
See [this PR](https://github.com/davidgohel/officer/pull/313).

## Usage

``` r
docx_bookmarks2(
  x,
  return_vector = FALSE,
  target = c("all", "header", "body", "footer")
)
```

## Arguments

- x:

  an `rdocx` object

- return_vector:

  use `TRUE` for compatibility with
  [`officer::docx_bookmarks()`](https://davidgohel.github.io/officer/reference/docx_bookmarks.html)

- target:

  one of c("all", "header", "body", "footer")

## Value

a list with all bookmarks

## Author

Dan Chaltiel
