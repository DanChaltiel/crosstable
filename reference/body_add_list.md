# Add a list to an `officer` document

Add a list to an `officer` document

## Usage

``` r
body_add_list(doc, value, ordered = FALSE, style = NULL, ...)

body_add_list_item(doc, value, ordered = FALSE, style = NULL, ...)
```

## Arguments

- doc:

  a docx object

- value:

  a character vector (`body_add_list()`) or scalar
  (`body_add_list_item`). See Section below for markdown support.

- ordered:

  if `TRUE`, adds an ordered list, if `FALSE` (default), adds a bullet
  list

- style:

  specify the style manually, overriding `ordered`. A better way is to
  set options `crosstable_style_list_ordered` and
  `crosstable_style_list_unordered` globally.

- ...:

  passed on to
  [`officer::body_add_par()`](https://davidgohel.github.io/officer/reference/body_add_par.html)

## Value

The docx object `doc`

## Details

Ordered lists and bullet lists are not supported by the default officer
template (see [https://github.com/davidgohel/officer/issues/262](#262)).
You have to manually set custom styles matching those list in a custom
Word template file. Then, you can use either the `style` argument or
crosstable options. See examples for more details.

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
if (FALSE) { # \dontrun{
#For this example to work, `my_template.docx` should include styles named
#`ordered_list` and `unordered_list`

library(officer)
library(crosstable)
options(crosstable_style_list_ordered="ordered_list")
options(crosstable_style_list_unordered="unordered_list")

read_docx("my_template.docx") %>%
 body_add_list(c("Numbered item 1", "Numbered item 2"), ordered = TRUE) %>%
 body_add_list_item("Numbered item 3", ordered = TRUE) %>%
 body_add_list(c("Bullet item 1", "Bullet item 2"), ordered = FALSE) %>%
 body_add_list_item("Bullet item 3", ordered = FALSE) %>%
 write_and_open()
} # }
```
