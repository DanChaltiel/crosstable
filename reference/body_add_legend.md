# Add a legend to a table or a figure

Add a legend to a table or a figure in an `officer` document. Legends
can be referred to using the `@ref` syntax in
[`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
(see examples for some use cases). Table legends should be inserted
before the table while figure legends should be inserted after the
figure.

## Usage

``` r
body_add_table_legend(
  doc,
  legend,
  ...,
  bookmark = NULL,
  legend_style = getOption("crosstable_style_legend", doc$default_styles$paragraph),
  style = deprecated(),
  legend_prefix = NULL,
  name_format = NULL,
  legend_name = "Table",
  seqfield = "SEQ Table \\* Arabic",
  par_before = FALSE,
  envir = parent.frame(),
  legacy = FALSE
)

body_add_figure_legend(
  doc,
  legend,
  ...,
  bookmark = NULL,
  legend_style = getOption("crosstable_style_legend", doc$default_styles$paragraph),
  style = deprecated(),
  legend_prefix = NULL,
  name_format = NULL,
  legend_name = "Figure",
  seqfield = "SEQ Figure \\* Arabic",
  par_after = FALSE,
  envir = parent.frame(),
  legacy = FALSE
)
```

## Arguments

- doc:

  a docx object

- legend:

  the table legend. Supports `glue` syntax and markdown syntax (see
  Section below).

- ...:

  unused

- bookmark:

  the id of the bookmark. This is the id that should then be called in
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  using the `"\\@ref(id)"` syntax. Forbidden characters will be removed.

- legend_style:

  style of of the whole legend. May depend on the docx template.
  However, if `name_format` is provided with a specific `font.size`,
  this size will apply to the whole legend for consistency.

- style:

  deprecated in favor of `name_format`.

- legend_prefix:

  a prefix that comes before the legend, after the numbering

- name_format:

  format of the legend's LHS (legend_name + numbering) using
  [`officer::fp_text_lite()`](https://davidgohel.github.io/officer/reference/fp_text.html)
  or
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html).
  Default to `fp_text_lite(bold=TRUE)` in addition to the format defined
  in `legend_style`. Note that the reference to the bookmark will have
  the same specific format in the text.

- legend_name:

  name before the numbering. Default to either "Table" or "Figure".

- seqfield:

  Keep default. Otherwise, you may figure it out doing this: in a docx
  file, insert a table legend, right click on the inserted number and
  select "Toggle Field Codes". This argument should be the value of the
  field, with extra escaping.

- par_before, par_after:

  should an empty paragraph be inserted before/after the legend?

- envir:

  Environment to evaluate each expression in `glue()`.

- legacy:

  use the old version of this function, if you cannot update `{officer}`
  to v0.4+

## Value

The docx object `doc`

## Warning

Be aware that you unfortunately cannot reference a bookmark more than
once using this method. Writing:  
`body_add_normal("Table \\@ref(iris_col1) is about flowers. I really like Table \\@ref(iris_col1).")`  
will prevent the numbering from applying.

## What to do if there is still no numbering?

During the opening of the document, MS Word might ask you to "update the
fields", to which you should answer "Yes".  
If it is not asked or if you answer "No", the legends added with
`body_add_table_legend()` or `body_add_figure_legend()` might have no
actual numbers displayed.  
In this case, you have to manually update the references in MS Word:
select all (Ctrl+A), then update (F9), sometimes twice. More info on
<https://ardata-fr.github.io/officeverse/faq.html#update-fields>.

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
library(ggplot2)
p = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
fp_italic = fp_text_lite(italic=TRUE, font.size=10)
x = read_docx() %>%
    body_add_normal("There is Table \\@ref(iris_col1) and Table \\@ref(iris_col2). ",
                    "The `iris` dataset is about flowers.") %>%
    body_add_normal() %>%
    body_add_table_legend("Iris dataset, column 1 (mean={round(mean(iris[[1]]), 2)})",
                           bookmark="iris_col1") %>%
    body_add_crosstable(crosstable(iris[1])) %>%
    body_add_normal() %>%
    body_add_table_legend("Iris dataset, column 2 (mean={round(mean(iris[[2]]), 2)})",
                          bookmark="iris_col2",
                          name_format=fp_italic, legend_style="Balloon Text") %>%
    body_add_crosstable(crosstable(iris[2])) %>%
    body_add_normal() %>%
    body_add_normal("There is also the figure \\@ref(iris_fig)") %>%
    body_add_gg(p) %>%
    body_add_figure_legend("Iris plot", bookmark="iris_fig")
write_and_open(x)
#If asked to update fields, press "Yes". Otherwise press Ctrl+A then F9 twice for the references
#to appear.
```
