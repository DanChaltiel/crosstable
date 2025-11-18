# Add a list of tables

Add a list of tables in an officer document. `crosstables` will be added
using
[`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)
and `flextables` will be added using
[`flextable::body_add_flextable()`](https://davidgohel.github.io/flextable/reference/body_add_flextable.html).
Plain dataframes will be converted to flextables.

## Usage

``` r
body_add_table_list(
  doc,
  l,
  fun_before = "title2",
  fun_after = NULL,
  fun = fun_before,
  ...
)

body_add_flextable_list(...)

body_add_crosstable_list(...)
```

## Arguments

- doc:

  a `rdocx` object, created by
  [`officer::read_docx()`](https://davidgohel.github.io/officer/reference/read_docx.html)

- l:

  a named list of tables (of class `crosstable`, `flextable`, or
  `data.frame`).

- fun_before:

  a function to be used before each table

- fun_after:

  a function to be used after each table.

- fun:

  Deprecated

- ...:

  arguments passed on to
  [`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)
  or
  [`flextable::body_add_flextable()`](https://davidgohel.github.io/flextable/reference/body_add_flextable.html)

## Value

The docx object `doc`

## `fun_before` and `fun_after`

These should be function of the form `function(doc, .name)` where
`.name` is the name of the current table of the list. You can also pass
`"title2"` to add the name as a title of level 2 between each table
(works for levels 3 and 4 as well), `"newline"` to simply add a new
line, or even `NULL` to not separate them (beware that the tables might
merge then). `fun_before` is designed to add a title while `fun_after`
is designed to add a table legend (cf. examples).

## Examples

``` r
library(officer)
ctl = list(iris2=crosstable(iris2, 1),
           "Just a flextable"=flextable::flextable(mtcars2[1:5,1:5]),
           "Just a dataframe"=iris2[1:5,1:5])

fun1 = function(doc, .name){
    doc %>%
        body_add_title(" This is table '{.name}' as a flex/crosstable", level=2) %>%
        body_add_normal("Here is the table:")
}
fun2 = function(doc, .name){
  doc %>% body_add_table_legend("{.name}", bookmark=.name)
}
read_docx() %>%
  body_add_title("Separated by subtitle", 1) %>%
  body_add_table_list(ctl, fun_before="title2") %>%
  body_add_break() %>%
  body_add_title("Separated using a custom function", 1) %>%
  body_add_normal("You can therefore use bookmarks, for instance here are
                   tables \\@ref(iris2), \\@ref(just_a_flextable)
                   and \\@ref(just_a_dataframe).") %>%
  body_add_table_list(ctl, fun_before=fun1, fun_after=fun2, body_fontsize=8) %>%
  write_and_open()
```
