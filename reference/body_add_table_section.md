# Add a section with a table and its legend

Add a section with a table and its legend

## Usage

``` r
body_add_table_section(
  doc,
  x,
  legend,
  ...,
  bookmark = NULL,
  title = getOption("crosstable_section_title", TRUE),
  title_lvl = getOption("crosstable_section_title_level", 3),
  sentence = getOption("crosstable_section_sentence", FALSE)
)
```

## Arguments

- doc:

  a `rdocx` object

- x:

  a table: `crosstable`, `flextable`, or plain old `dataframe`

- legend:

  the legend to use

- ...:

  passed on to
  [`flextable::body_add_flextable()`](https://davidgohel.github.io/flextable/reference/body_add_flextable.html)
  or
  [`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)

- bookmark:

  the bookmark to use. Defaults to the cleaned variable name of `x`

- title:

  the title to add for the section. Can also be `FALSE` (no title) or
  `TRUE` (the title defaults to `legend`)

- title_lvl:

  the title level if applicable

- sentence:

  a sentence to add between the title (if applicable) and the table. If
  `TRUE`, defaults to
  `"Information about {tolower(title)} is described in Table @ref({bookmark})"`.

## Value

The `docx` object `doc`

## Examples

``` r
library(officer)
read_docx() %>%
  body_add_title("Description", 1) %>%
  body_add_title("Population A", 2) %>%
  body_add_table_section(head(iris), "The iris dataset", sentence=TRUE) %>%
  body_add_table_section(crosstable(iris), "A crosstable of the iris dataset",
                         title=FALSE, sentence=TRUE, body_fontsize=8) %>%
  write_and_open()
```
