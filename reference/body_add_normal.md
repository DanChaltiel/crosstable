# Add a new paragraph with default style

Add a new paragraph in an `officer` document with default style.  
Variables can be inserted in the text as multiple strings
([`paste()`](https://rdrr.io/r/base/paste.html) style) or enclosed by
braces (`glue()` style).  
Basic markdown syntax is available: `**bold**`, `*italic*`, and
`_underlined_`.  
References to any bookmark can be inserted using the syntax
`@ref(bookmark)` and newlines can be inserted using the token `<br>`.

## Usage

``` r
body_add_normal(
  doc,
  ...,
  .sep = "",
  style = NULL,
  squish = TRUE,
  font_size = NA,
  envir = parent.frame(),
  parse = c("ref", "format", "code")
)
```

## Arguments

- doc:

  the doc object (created with the `read_docx` function of `officer`
  package)

- ...:

  one or several character strings, pasted using `.sep`. As with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html),
  expressions enclosed by braces will be evaluated as R code. If more
  than one variable is passed, all should be of length 1.

- .sep:

  Separator used to separate elements.

- style:

  Style for normal text. Best set with
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md).

- squish:

  Whether to squish the result (remove trailing and repeated spaces).
  Default to `TRUE`. Allows to add multiline paragraph without breaking
  the string.

- font_size:

  Font size.

- envir:

  Environment to evaluate each expression in `glue()`.

- parse:

  which format to parse. Default to all formats
  (`c("ref", "format", "code")`).

## Value

a new doc object

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

See the last example of `body_add_normal()` for a practical case.

## Author

Dan Chaltiel

## Examples

``` r
library(officer)
library(crosstable)

info_rows = c("Also, table iris has {nrow(iris)} rows.",
              "And table mtcars has {nrow(mtcars)} rows.")
doc = read_docx()  %>%
    body_add_normal("Table iris has", ncol(iris), "columns.", .sep=" ") %>% #paste style
    body_add_normal("However, table mtcars has {ncol(mtcars)} columns") %>% #glue style
    body_add_normal(info_rows)                                          %>% #vector style
    body_add_normal("")
doc = doc %>%
    body_add_normal("You can write text in *italic1*, _underlined1_, **bold1**, and `code`,
                    and you can also add * **references** *, for instance a ref to Table
                    @ref(my_table). Multiple spaces are ignored (squished) so that you
                    can enter multiline text.") %>%
    body_add_normal() %>%
    body_add_normal("Here I should use `body_add_crosstable()` to add a table before the
                     legend.") %>%
    body_add_table_legend("My pretty table", bookmark="my_table")
write_and_open(doc)

#Markdown support
read_docx() %>%
  body_add_normal("This is **bold and *italic* (see Table @ref(my_bkm)). ** <br> This is
                   **bold `console \\*CODE\\*` and *bold _and_ italic* **") %>%
  body_add_normal("This is <color:red>red **bold** text</color>, this is ~subscript *italic*~,
                   and this is ^superscript with <shade:yellow>yellow</shade>^") %>%
  body_add_normal("This is <ff:Alibi>a fancy font</ff> and this `is code`!!") %>%
              #you might need to change "Alibi" to "alibi" here
  body_add_normal() %>%
  body_add_table_legend("Some table legend", bookmark="my_bkm") %>%
  write_and_open()
```
