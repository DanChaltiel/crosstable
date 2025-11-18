# Add a title to an `officer` document

Add a title to an `officer` document

## Usage

``` r
body_add_title(
  doc,
  value,
  level = 1,
  squish = TRUE,
  envir = parent.frame(),
  style = getOption("crosstable_style_heading", "heading")
)
```

## Arguments

- doc:

  the doc object (created with the `read_docx` function of `officer`
  package)

- value:

  a character string. See Section below for markdown support.

- level:

  the level of the title. See `styles_info(doc)` to know the
  possibilities.

- squish:

  Whether to squish the result (remove trailing and repeated spaces).
  Default to `TRUE`.

- envir:

  Environment to evaluate each expression in `glue()`.

- style:

  the name of the title style. See `styles_info(doc)` to know the
  possibilities.

## Value

The docx object `doc`

## Markdown support

In all `crosstable` helpers for `officer`, you can use the following
Markdown syntax to format your text:

- *bold*: `"**text in bold**"`

- \*italics: `"*text in italics*"`

- *subscript*: `"Text in ~subscript~"`

- *superscript*: `"Text in ^superscript^"`

- *newline*: `Before <br> After`

- *color*: `"<color:red>red text</color>"`

- *shade*: `"<shade:yellow>yellow text</shade>"` (background color)

- *font family*: `"<ff:symbol>symbol</ff>"` (

Note that the font name depends on your system language. For instant, in
French, it would be `Symbol` with an uppercase first letter.

See the last example of
[`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
for a practical case.

## Author

Dan Chaltiel

## Examples

``` r
library(officer)
library(crosstable)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
doc = read_docx() %>%
   body_add_title("La table iris (nrow={nrow(iris)})", 1) %>%
   body_add_title("Description", 2) %>%
   body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#write_and_open(doc)
```
