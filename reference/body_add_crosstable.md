# Add a crosstable to an `officer` document

`body_add_crosstable()` adds such a `flextable` an `officer` document.

## Usage

``` r
body_add_crosstable(
  doc,
  x,
  body_fontsize = NULL,
  header_fontsize = ceiling(body_fontsize * 1.2),
  padding_v = NULL,
  allow_break = TRUE,
  max_cols = 25,
  ...
)
```

## Arguments

- doc:

  a `rdocx` object, created by
  [`officer::read_docx()`](https://davidgohel.github.io/officer/reference/read_docx.html)

- x:

  a `crosstable` object

- body_fontsize:

  fontsize of the body

- header_fontsize:

  fontsize of the header. Defaults to `1.2*body_fontsize`.

- padding_v:

  vertical padding of all table rows

- allow_break:

  allow crosstable rows to break across pages

- max_cols:

  max number of columns for `x`

- ...:

  further arguments passed to
  [`as_flextable.crosstable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)

## Value

The docx object `doc`

## Author

Dan Chaltiel

## Examples

``` r
#Officer
library(officer)
mytable = crosstable(mtcars2)
doc = read_docx() %>%
    body_add_crosstable(mytable) %>%
    body_add_break %>%
    body_add_crosstable(mytable, compact=TRUE)
#> Error in mutate(., across(-any_of(c(id_from, name_to)), ~if_else(.data[[id_from]] %in%     collapse_grp, .x[.data[[name_to]] == collapse] %0% NA, .x))): ℹ In argument: `across(...)`.
#> Caused by error in `across()`:
#> ! Can't compute column `value`.
#> Caused by error in `if_else()`:
#> ! could not find function "if_else"

dfile = tempfile(fileext=".docx")
print(doc, target = dfile)
#> Error: object 'doc' not found
if(interactive()) browseURL(dfile)
```
