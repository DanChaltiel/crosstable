# Alternative to [`flextable::body_add_flextable()`](https://davidgohel.github.io/flextable/reference/body_add_flextable.html)

Extends `body_add_flextable()` by adding:

- a legend (if `x` has a `"legend"` attribute), via
  [`body_add_table_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)

- an optional empty line after the table

## Usage

``` r
body_add_flextable2(doc, x, bookmark = NULL, append_line = TRUE, ...)
```

## Arguments

- doc:

  An `rdocx` object from **officer**.

- x:

  A `flextable` object. If it has a `"legend"` attribute, it is added.

- bookmark:

  Optional. Word bookmark name for the legend.

- append_line:

  Whether to add an empty line after the table.

- ...:

  Passed to
  [`flextable::body_add_flextable()`](https://davidgohel.github.io/flextable/reference/body_add_flextable.html).

## Value

The docx object `doc`

## Examples

``` r
library(officer)
library(ggplot2)
ft = flextable::flextable(head(iris)) %>%
  structure(legend="The iris dataset")

doc = read_docx() %>%
 body_add_normal("Text before") %>%
 body_add_flextable2(ft) %>%
 body_add_normal("Text after")
write_and_open(doc)
```
