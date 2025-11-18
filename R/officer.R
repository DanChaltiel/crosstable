
# Crosstable --------------------------------------------------------------


#' Add a crosstable to an `officer` document
#' @description [body_add_crosstable()] adds such a `flextable` an `officer` document.
#'
#' @param doc a `rdocx` object, created by [officer::read_docx()]
#' @param x a `crosstable` object
#' @param body_fontsize fontsize of the body
#' @param header_fontsize fontsize of the header. Defaults to `1.2*body_fontsize`.
#' @param padding_v vertical padding of all table rows
#' @param allow_break allow crosstable rows to break across pages
#' @param max_cols max number of columns for `x`
#' @param ... further arguments passed to [as_flextable.crosstable()]
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_class vname
#' @importFrom cli cli_abort
#' @importFrom flextable as_flextable body_add_flextable fontsize padding
#'
#' @return The docx object `doc`
#'
#' @examples
#' #Officer
#' library(officer)
#' mytable = crosstable(mtcars2)
#' doc = read_docx() %>%
#'     body_add_crosstable(mytable) %>%
#'     body_add_break %>%
#'     body_add_crosstable(mytable, compact=TRUE)
#'
#' dfile = tempfile(fileext=".docx")
#' print(doc, target = dfile)
#' if(interactive()) browseURL(dfile)
body_add_crosstable = function (doc, x, body_fontsize=NULL,
                                header_fontsize=ceiling(body_fontsize*1.2),
                                padding_v=NULL,
                                allow_break=TRUE,
                                max_cols=25, ...) {
  if(is.null(x)){
    cli_abort("{caller_arg(x)} is NULL")
  }
  assert_class(x, "crosstable", .var.name=vname(x))

  if(missing(padding_v)) padding_v = getOption("crosstable_padding_v", NULL)
  if(missing(body_fontsize)) body_fontsize = getOption("crosstable_fontsize_body", NULL)
  if(missing(header_fontsize)) header_fontsize = getOption("crosstable_fontsize_header", ceiling(body_fontsize*1.2))
  if(missing(allow_break)) allow_break = getOption("crosstable_allow_break", TRUE)
  if(missing(max_cols)) max_cols = getOption("crosstable_add_max_cols", 25)

  if(ncol(x)>max_cols){
    cli_abort(c("The crosstable {.var x} has {ncol(x)} columns, which is higher than {.arg max_cols} ({max_cols}). The resulting document will probably be totally unreadable.",
                i="To override this error, use {.code body_add_crosstable(max_cols={ncol(x)})} or {.code options(crosstable_add_max_cols={ncol(x)})}"),
              class="crosstable_body_add_large_error")
  }
  ft = as_flextable(x, ...)
  if(length(body_fontsize)>0)
    ft = fontsize(ft, size = body_fontsize, part = "body")
  if(length(header_fontsize)>0)
    ft = fontsize(ft, size = header_fontsize, part = "header")
  if(length(padding_v)>0)
    ft = padding(ft, padding.top=padding_v, padding.bottom=padding_v, part = "body")

  body_add_flextable(doc, ft, keepnext=!allow_break)
}



# Officer add_xxx ---------------------------------------------------------



#' Add a new paragraph with default style
#'
#' Add a new paragraph in an `officer` document with default style.\cr
#' Variables can be inserted in the text as multiple strings (`paste()` style) or enclosed by braces (`glue()` style).  \cr
#' Basic markdown syntax is available: `**bold**`, `*italic*`, and `_underlined_`. \cr
#' References to any bookmark can be inserted using the syntax `@ref(bookmark)` and newlines can be inserted using the token `<br>`.
#'
#' @param doc the doc object (created with the `read_docx` function of `officer` package)
#' @param ... one or several character strings, pasted using `.sep`. As with `glue::glue()`, expressions enclosed by braces will be evaluated as R code. If more than one variable is passed, all should be of length 1.
#' @param style Style for normal text. Best set with [crosstable_options()].
#' @param font_size Font size.
#' @param .sep Separator used to separate elements.
#' @param squish Whether to squish the result (remove trailing and repeated spaces). Default to `TRUE`. Allows to add multiline paragraph without breaking the string.
#' @param parse which format to parse. Default to all formats (`c("ref", "format", "code")`).
#' @param envir Environment to evaluate each expression in `glue()`.
#'
#' @section Markdown support:
#' In all `crosstable` helpers for `officer`, you can use the following Markdown syntax to format your text:
#'
#'  - *bold*: `"**text in bold**"`
#'  - *italics: `"*text in italics*"`
#'  - *subscript*: `"Text in ~subscript~"`
#'  - *superscript*: `"Text in ^superscript^"`
#'  - *newline*: `Before <br> After`
#'  - *color*: `"<color:red>red text</color>"`
#'  - *shade*: `"<shade:yellow>yellow text</shade>"` (background color)
#'  - *font family*: `"<ff:symbol>symbol</ff>"` (
#'
#' Note that the font name depends on your system language. For instant, in French, it would be `Symbol` with an uppercase first letter.
#'
#' See the last example of [body_add_normal()] for a practical case.
#'
#'
#' @return a new doc object
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom cli cli_abort
#' @importFrom stringr str_squish
#'
#' @return The docx object `doc`
#'
#' @examples
#' library(officer)
#' library(crosstable)
#'
#' info_rows = c("Also, table iris has {nrow(iris)} rows.",
#'               "And table mtcars has {nrow(mtcars)} rows.")
#' doc = read_docx()  %>%
#'     body_add_normal("Table iris has", ncol(iris), "columns.", .sep=" ") %>% #paste style
#'     body_add_normal("However, table mtcars has {ncol(mtcars)} columns") %>% #glue style
#'     body_add_normal(info_rows)                                          %>% #vector style
#'     body_add_normal("")
#' doc = doc %>%
#'     body_add_normal("You can write text in *italic1*, _underlined1_, **bold1**, and `code`,
#'                     and you can also add * **references** *, for instance a ref to Table
#'                     @ref(my_table). Multiple spaces are ignored (squished) so that you
#'                     can enter multiline text.") %>%
#'     body_add_normal() %>%
#'     body_add_normal("Here I should use `body_add_crosstable()` to add a table before the
#'                      legend.") %>%
#'     body_add_table_legend("My pretty table", bookmark="my_table")
#' write_and_open(doc)
#'
#' #Markdown support
#' read_docx() %>%
#'   body_add_normal("This is **bold and *italic* (see Table @ref(my_bkm)). ** <br> This is
#'                    **bold `console \\*CODE\\*` and *bold _and_ italic* **") %>%
#'   body_add_normal("This is <color:red>red **bold** text</color>, this is ~subscript *italic*~,
#'                    and this is ^superscript with <shade:yellow>yellow</shade>^") %>%
#'   body_add_normal("This is <ff:Alibi>a fancy font</ff> and this `is code`!!") %>%
#'               #you might need to change "Alibi" to "alibi" here
#'   body_add_normal() %>%
#'   body_add_table_legend("Some table legend", bookmark="my_bkm") %>%
#'   write_and_open()
body_add_normal = function(doc, ..., .sep="", style=NULL, squish=TRUE, font_size=NA,
                           envir=parent.frame(),
                           parse=c("ref", "format", "code")) {
  if(missing(squish)) squish = getOption("crosstable_normal_squish", TRUE)
  if(missing(font_size)) font_size = getOption("crosstable_normal_font_size", NA)
  dots = list(...)
  if(is.null(style)){
    style = getOption("crosstable_style_normal", doc$default_styles$paragraph)
  }
  if(length(dots)==0) dots=""
  dots_lengths = lengths(dots)

  if(all(dots_lengths==1)){ #one or several vectors of length 1
    value = do.call(glue, c(dots, .sep=.sep, .envir=envir))
    if(squish) value = str_squish(value)
    parse_ref = "ref" %in% parse
    parse_format = "format" %in% parse
    parse_code = "code" %in% parse
    doc = body_add_parsed(doc, value, style=style, parse_ref=parse_ref,
                          parse_format=parse_format, parse_code=parse_code,
                          font_size=font_size)
  } else if(length(dots)==1) { #one vector (of 1 or more) -> recursive call
    for(i in dots[[1]]){
      doc = body_add_normal(doc, i, .sep=.sep, style=style, squish=squish,
                            font_size=font_size, parse=parse)
    }
  } else { #several vectors of which at least one is length 2+
    cli_abort(c("{.fun body_add_normal} only accepts either one vector of any length or several vectors of length 1",
                i="Length of vectors passed: {.val {dots_lengths}}"),
              class="crosstable_officer_wrong_vector_error")
  }

  doc
}




#' Add a title to an `officer` document
#'
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param value a character string. See Section below for markdown support.
#' @param level the level of the title. See \code{styles_info(doc)} to know the possibilities.
#' @param squish Whether to squish the result (remove trailing and repeated spaces). Default to `TRUE`.
#' @param style the name of the title style. See \code{styles_info(doc)} to know the possibilities.
#' @param envir Environment to evaluate each expression in `glue()`.
#'
#' @return The docx object `doc`
#'
#' @inheritSection body_add_normal Markdown support
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_integerish
#' @importFrom glue glue
#' @importFrom stringr str_squish
#' @examples
#' library(officer)
#' library(crosstable)
#' library(dplyr)
#' doc = read_docx() %>%
#'    body_add_title("La table iris (nrow={nrow(iris)})", 1) %>%
#'    body_add_title("Description", 2) %>%
#'    body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#' #write_and_open(doc)
body_add_title = function(doc, value, level=1, squish=TRUE,
                          envir=parent.frame(),
                          style=getOption("crosstable_style_heading", "heading")) {
  assert_integerish(level)
  if(missing(squish)) squish = getOption("crosstable_title_squish", TRUE)
  value = glue(value, .envir = envir)
  if(squish) value = str_squish(value)
  style = paste(style, level)
  body_add_parsed(doc, value, style=style)
}


#' Add a list to an `officer` document
#'
#' @param doc a docx object
#' @param value a character vector (`body_add_list()`) or scalar (`body_add_list_item`). See Section below for markdown support.
#' @param ordered if `TRUE`, adds an ordered list, if `FALSE` (default), adds a bullet list
#' @param style specify the style manually, overriding `ordered`. A better way is to set options `crosstable_style_list_ordered` and `crosstable_style_list_unordered` globally.
#' @param ... passed on to [officer::body_add_par()]
#'
#' @return The docx object `doc`
#'
#' @details Ordered lists and bullet lists are not supported by the default officer template (see [https://github.com/davidgohel/officer/issues/262](#262)). You have to manually set custom styles matching those list in a custom Word template file. Then, you can use either the `style` argument or crosstable options. See examples for more details.
#'
#' @inheritSection body_add_normal Markdown support
#' @export
#'
#' @examples
#' \dontrun{
#' #For this example to work, `my_template.docx` should include styles named
#' #`ordered_list` and `unordered_list`
#'
#' library(officer)
#' library(crosstable)
#' options(crosstable_style_list_ordered="ordered_list")
#' options(crosstable_style_list_unordered="unordered_list")
#'
#' read_docx("my_template.docx") %>%
#'  body_add_list(c("Numbered item 1", "Numbered item 2"), ordered = TRUE) %>%
#'  body_add_list_item("Numbered item 3", ordered = TRUE) %>%
#'  body_add_list(c("Bullet item 1", "Bullet item 2"), ordered = FALSE) %>%
#'  body_add_list_item("Bullet item 3", ordered = FALSE) %>%
#'  write_and_open()
#' }
body_add_list = function(doc, value, ordered=FALSE, style=NULL, ...){
  for(i in value){
    doc = body_add_list_item(doc, i, ordered=ordered, style=style, ...)
  }
  doc
}

#' @rdname body_add_list
#' @author Dan Chaltiel
#' @export
#' @importFrom cli cli_abort
body_add_list_item = function(doc, value, ordered=FALSE, style=NULL, ...){
  if(is.null(style)){
    if(ordered){
      style = getOption("crosstable_style_list_ordered", NULL)
    } else {
      style = getOption("crosstable_style_list_unordered", NULL)
    }
    if(is.null(style)){
      cli_abort("Ordered lists and bullet lists are not supported by the default officer template. You have to set them in a custom template and use either the `style` argument or crosstable options. See `?body_add_list` examples for more details.",
                class="crosstable_officer_lists_style_error") #nocov
    }
  }
  body_add_parsed(doc, value, style=style, ...)
}



#' Add a list of tables
#'
#' Add a list of tables in an officer document. `crosstables` will be added using [body_add_crosstable()] and `flextables` will be added using [flextable::body_add_flextable()]. Plain dataframes will be converted to flextables.
#'
#' @param doc a `rdocx` object, created by [officer::read_docx()]
#' @param l a named list of tables (of class `crosstable`, `flextable`, or `data.frame`).
#' @param fun_before a function to be used before each table
#' @param fun_after a function to be used after each table.
#' @param fun Deprecated
#' @param ... arguments passed on to [body_add_crosstable()] or [flextable::body_add_flextable()]
#'
#' @section `fun_before` and `fun_after`:
#' These should be function of the form `function(doc, .name)` where `.name` is the name of the current table of the list.
#' You can also pass `"title2"` to add the name as a title of level 2 between each table (works for levels 3 and 4 as well), `"newline"` to simply add a new line, or even `NULL` to not separate them (beware that the tables might merge then).
#' `fun_before` is designed to add a title while `fun_after` is designed to add a table legend (cf. examples).
#'
#' @importFrom checkmate assert_class assert_list
#' @importFrom cli cli_abort
#' @importFrom dplyr intersect
#' @importFrom flextable flextable
#' @importFrom lifecycle deprecate_warn
#' @importFrom purrr keep map
#' @importFrom rlang is_named is_string
#'
#' @return The docx object `doc`
#' @export
#'
#' @examples
#' library(officer)
#' ctl = list(iris2=crosstable(iris2, 1),
#'            "Just a flextable"=flextable::flextable(mtcars2[1:5,1:5]),
#'            "Just a dataframe"=iris2[1:5,1:5])
#'
#' fun1 = function(doc, .name){
#'     doc %>%
#'         body_add_title(" This is table '{.name}' as a flex/crosstable", level=2) %>%
#'         body_add_normal("Here is the table:")
#' }
#' fun2 = function(doc, .name){
#'   doc %>% body_add_table_legend("{.name}", bookmark=.name)
#' }
#' read_docx() %>%
#'   body_add_title("Separated by subtitle", 1) %>%
#'   body_add_table_list(ctl, fun_before="title2") %>%
#'   body_add_break() %>%
#'   body_add_title("Separated using a custom function", 1) %>%
#'   body_add_normal("You can therefore use bookmarks, for instance here are
#'                    tables \\@ref(iris2), \\@ref(just_a_flextable)
#'                    and \\@ref(just_a_dataframe).") %>%
#'   body_add_table_list(ctl, fun_before=fun1, fun_after=fun2, body_fontsize=8) %>%
#'   write_and_open()
body_add_table_list = function(doc, l, fun_before="title2", fun_after=NULL,
                               fun=fun_before, ...){
  assert_list(l)
  if(!missing(fun)){
    deprecate_warn("5.1.0", "body_add_table_list(fun)", "body_add_table_list(fun_before)")
  }
  if(!is_named(l)){
    cli_abort(c("List {.var l} must have names for all members.",
                i="Current names: {names(l)}"),
              class="body_add_table_list_named")
  }
  l = map(l, ~{
    if(!is.crosstable(.x) && is.data.frame(.x)) flextable(.x) else .x
  })

  wrong_classes = l %>% keep(~!is.crosstable(.x) && !inherits(.x, "flextable"))
  if(length(wrong_classes)>0){
    cli_abort(c("{.fun body_add_table_list} only accepts {.cls crosstable}, {.cls flextable}, and {.cls data.frame}.",
                x="Wrong class{?es} in the list: {.cls {map_chr(wrong_classes, ~class(.x)[1])}}"),
              class="body_add_table_list_class")
  }

  fun0 = function(doc, .name) doc
  if(is.null(fun_after)) fun_after = fun0
  if(is_string(fun)){
    if(fun=="title2") fun=function(doc, .name) body_add_title(doc, .name, 2)
    else if(fun=="title3") fun=function(doc, .name) body_add_title(doc, .name, 3)
    else if(fun=="title4") fun=function(doc, .name) body_add_title(doc, .name, 4)
    else if(fun=="newline") fun=function(doc, .name) body_add_normal(doc, "")
    else {
      cli_abort(c('{.arg fun} should be either a function or one of
                {.val {c("title2", "title3", "title4", "newline")}}',
                i="Current value: {.val {fun}}"),
                class="body_add_table_list_fun_name")
    }
  } else if(is.null(fun)) fun=fun0
  assert_class(fun, "function")

  if(!identical(formal_args(fun), c("doc", ".name"))){
    cli_abort(c('`fun` should be of the form `function(doc, .name)`',
                i="Current arg names: {formal_args(fun)}"),
              class="body_add_table_list_fun_args")
  }

  argnames = names(list(...))
  for(i in names(l)){
    x=l[[i]]
    doc = doc %>% fun(.name=i)
    if(!inherits(doc, "rdocx")){
      cli_abort('fun(doc, .name)` shoud return a {.cls rdocx} value.',
                class="body_add_table_list_return")
    }
    if(is.crosstable(x)) {
      args = intersect(argnames, names(as.list(args(body_add_crosstable))))
      doc = do.call(body_add_crosstable, c(list(doc=doc, x=x), list(...)[args]))
    } else if(inherits(x, "flextable")) {
      args = intersect(argnames, names(as.list(args(body_add_flextable))))
      doc = do.call(body_add_flextable, c(list(x=doc, value=x), list(...)[args]))
    }

    doc = doc %>% fun_after(.name=i)
    if(!inherits(doc, "rdocx")){
      cli_abort('fun_after(doc, .name)` shoud return a {.cls rdocx} value.',
                class="body_add_table_list_return2")
    }
  }

  doc
}

#' @rdname body_add_table_list
#' @export
#' @importFrom lifecycle deprecate_warn
body_add_flextable_list = function(...){
  deprecate_warn("0.5.0", "body_add_table_list(=")
  body_add_table_list(...)
}
#' @rdname body_add_table_list
#' @export
#' @importFrom lifecycle deprecate_warn
body_add_crosstable_list = function(...){
  deprecate_warn("0.5.0", "body_add_table_list(=")
  body_add_table_list(...)
}

#' Add a section with a table and its legend
#'
#' @param doc a `rdocx` object
#' @param x a table: `crosstable`, `flextable`, or plain old `dataframe`
#' @param legend the legend to use
#' @param bookmark the bookmark to use. Defaults to the cleaned variable name of `x`
#' @param title the title to add for the section. Can also be `FALSE` (no title) or `TRUE` (the title defaults to `legend`)
#' @param title_lvl the title level if applicable
#' @param sentence a sentence to add between the title (if applicable) and the table. If `TRUE`, defaults to `"Information about {tolower(title)} is described in Table @ref({bookmark})"`.
#' @param ... passed on to [flextable::body_add_flextable()] or [body_add_crosstable()]
#'
#' @return The `docx` object `doc`
#' @importFrom flextable body_add_flextable qflextable
#' @export
#'
#' @examples
#' library(officer)
#' read_docx() %>%
#'   body_add_title("Description", 1) %>%
#'   body_add_title("Population A", 2) %>%
#'   body_add_table_section(head(iris), "The iris dataset", sentence=TRUE) %>%
#'   body_add_table_section(crosstable(iris), "A crosstable of the iris dataset",
#'                          title=FALSE, sentence=TRUE, body_fontsize=8) %>%
#'   write_and_open()
body_add_table_section = function(doc, x, legend, ..., bookmark=NULL,
                                  title=getOption("crosstable_section_title", TRUE),
                                  title_lvl=getOption("crosstable_section_title_level", 3),
                                  sentence=getOption("crosstable_section_sentence", FALSE)){
  ctname = rlang::caller_arg(x)
  if(is.null(x)) cli_abort("{.arg x} ({.val {ctname}}) should not be NULL.")
  ok_classes = c("data.frame", "flextable", "crosstable")
  if(!inherits(x, ok_classes)) cli_abort("{.arg x} ({.val {ctname}}) should not of class {.cls {ok_classes}}.")
  if(is.null(bookmark)) bookmark = crosstable_clean_names(ctname)

  if(!is.null(title) && !isFALSE(title)){
    if(isTRUE(title)) title = legend
    doc = body_add_title(doc, title, title_lvl)
  }
  if(isTRUE(sentence)){
    if(isTRUE(title) || isFALSE(title)) title = legend
    doc = body_add_normal(doc, "Information about {tolower(title)} is described in Table @ref({bookmark}).")
  } else if(!is.null(sentence) & !isFALSE(sentence)) {
    doc = body_add_normal(doc, sentence)
  }
  doc = body_add_table_legend(doc, legend=legend, bookmark=bookmark)
  if(inherits(x, "crosstable")){
    doc = body_add_crosstable(doc, x, ...)
  } else {
    if(!inherits(x, "flextable")) x = qflextable(x)
    doc = body_add_flextable(doc, x, ...)
  }
  doc
}


#' Add a legend to a table or a figure
#'
#' Add a legend to a table or a figure in an `officer` document. Legends can be referred to using the `@ref` syntax in [body_add_normal()] (see examples for some use cases). Table legends should be inserted before the table while figure legends should be inserted after the figure.
#'
#' @param doc a docx object
#' @param legend the table legend. Supports `glue` syntax and markdown syntax (see Section below).
#' @param bookmark the id of the bookmark. This is the id that should then be called in [body_add_normal()] using the `"\\@ref(id)"` syntax. Forbidden characters will be removed.
#' @param legend_prefix a prefix that comes before the legend, after the numbering
#' @param legend_style style of of the whole legend. May depend on the docx template. However, if `name_format` is provided with a specific `font.size`, this size will apply to the whole legend for consistency.
#' @param name_format format of the legend's LHS (legend_name + numbering) using [officer::fp_text_lite()] or [officer::fp_text()]. Default to `fp_text_lite(bold=TRUE)` in addition to the format defined in `legend_style`. Note that the reference to the bookmark will have the same specific format in the text.
#' @param legend_name name before the numbering. Default to either "Table" or "Figure".
#' @param style deprecated in favor of `name_format`.
#' @param seqfield Keep default. Otherwise, you may figure it out doing this: in a docx file, insert a table legend, right click on the inserted number and select "Toggle Field Codes". This argument should be the value of the field, with extra escaping.
#' @param par_before,par_after should an empty paragraph be inserted before/after the legend?
#' @param legacy use the old version of this function, if you cannot update `{officer}` to v0.4+
#' @param ... unused
#' @param envir Environment to evaluate each expression in `glue()`.
#'
#' @return The docx object `doc`
#'
#' @inheritSection body_add_normal Markdown support
#'
#' @section Warning:
#' Be aware that you unfortunately cannot reference a bookmark more than once using this method. Writing: \cr `body_add_normal("Table \\@ref(iris_col1) is about flowers. I really like Table \\@ref(iris_col1).")`\cr
#' will prevent the numbering from applying.
#' @section What to do if there is still no numbering?:
#' During the opening of the document, MS Word might ask you to "update the fields", to which you should answer "Yes".  \cr
#' If it is not asked or if you answer "No", the legends added with [body_add_table_legend()] or [body_add_figure_legend()] might have no actual numbers displayed. \cr
#' In this case, you have to manually update the references in MS Word: select all (\kbd{Ctrl}+\kbd{A}), then update (\kbd{F9}), sometimes twice. More info on [https://ardata-fr.github.io/officeverse/faq.html#update-fields](https://ardata-fr.github.io/officeverse/faq.html#update-fields).
#'
#'
#' @rdname body_add_legend
#' @name body_add_legend
#' @author Dan Chaltiel
#' @importFrom lifecycle deprecated
#' @importFrom rlang check_dots_empty
#' @export
#'
#' @examples
#' library(officer)
#' library(ggplot2)
#' p = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
#' fp_italic = fp_text_lite(italic=TRUE, font.size=10)
#' x = read_docx() %>%
#'     body_add_normal("There is Table \\@ref(iris_col1) and Table \\@ref(iris_col2). ",
#'                     "The `iris` dataset is about flowers.") %>%
#'     body_add_normal() %>%
#'     body_add_table_legend("Iris dataset, column 1 (mean={round(mean(iris[[1]]), 2)})",
#'                            bookmark="iris_col1") %>%
#'     body_add_crosstable(crosstable(iris[1])) %>%
#'     body_add_normal() %>%
#'     body_add_table_legend("Iris dataset, column 2 (mean={round(mean(iris[[2]]), 2)})",
#'                           bookmark="iris_col2",
#'                           name_format=fp_italic, legend_style="Balloon Text") %>%
#'     body_add_crosstable(crosstable(iris[2])) %>%
#'     body_add_normal() %>%
#'     body_add_normal("There is also the figure \\@ref(iris_fig)") %>%
#'     body_add_gg(p) %>%
#'     body_add_figure_legend("Iris plot", bookmark="iris_fig")
#' write_and_open(x)
#' #If asked to update fields, press "Yes". Otherwise press Ctrl+A then F9 twice for the references
#' #to appear.
body_add_table_legend = function(doc, legend, ..., bookmark=NULL,
                                 legend_style=getOption("crosstable_style_legend",
                                                        doc$default_styles$paragraph),
                                 style=deprecated(),
                                 legend_prefix=NULL,
                                 name_format=NULL,
                                 legend_name="Table",
                                 seqfield="SEQ Table \\* Arabic",
                                 par_before=FALSE,
                                 envir=parent.frame(),
                                 legacy=FALSE){
  check_dots_empty()
  if(missing(par_before)) par_before = getOption("crosstable_table_legend_par_before", FALSE)
  if(missing(legend_prefix)) legend_prefix = getOption("crosstable_table_legend_prefix", NULL)
  if(par_before){
    doc=body_add_normal(doc, "")
  }
  body_add_legend(doc=doc, legend=legend, legend_name=legend_name,
                  bookmark=bookmark, legend_prefix=legend_prefix, legend_style=legend_style,
                  name_format=name_format, seqfield=seqfield,
                  style=style, legacy=legacy, envir=envir)
}

#' @rdname body_add_legend
#' @export
#' @importFrom lifecycle deprecated
#' @importFrom rlang check_dots_empty
body_add_figure_legend = function(doc, legend, ..., bookmark=NULL,
                                  legend_style=getOption("crosstable_style_legend",
                                                         doc$default_styles$paragraph),
                                  style=deprecated(),
                                  legend_prefix=NULL,
                                  name_format=NULL,
                                  legend_name="Figure",
                                  seqfield="SEQ Figure \\* Arabic",
                                  par_after=FALSE,
                                  envir=parent.frame(),
                                  legacy=FALSE){
  check_dots_empty()
  # ellipsis::check_dots_empty()
  if(missing(par_after)) par_after = getOption("crosstable_figure_legend_par_after", FALSE)
  if(missing(legend_prefix)) legend_prefix = getOption("crosstable_figure_legend_prefix", NULL)
  doc = body_add_legend(doc=doc, legend=legend, legend_name=legend_name,
                        bookmark=bookmark, legend_prefix=legend_prefix, legend_style=legend_style,
                        name_format=name_format, seqfield=seqfield,
                        style=style, legacy=legacy, envir=envir)
  if(par_after){
    doc=body_add_normal(doc, "")
  }
  doc
}


#' @importFrom glue glue
#' @importFrom lifecycle deprecate_warn is_present
#' @importFrom officer body_add_fpar fp_text_lite fpar ftext run_bookmark run_word_field
#' @keywords internal
#' @noRd
body_add_legend = function(doc, legend, legend_name, bookmark,
                           legend_prefix, legend_style, name_format, seqfield,
                           style, legacy, envir){

  # nocov start
  if(is_present(style)){
    deprecate_warn("0.2.2", "body_add_X_legend(style)",
                   "body_add_X_legend(name_format)",
                   details="Therefore, its value has been ignored. Use `legacy=TRUE` to override.")
  }
  # nocov end

  legend = paste0(legend_prefix, legend)
  if(is.null(name_format)){
    name_format = getOption("crosstable_format_legend_name", fp_text_lite(bold=TRUE))
  }
  fp_size = fp_text_lite(font.size=name_format$font.size)

  legend = glue(legend, .envir=envir)
  legend_name = paste0(legend_name, " ")

  bkm = run_word_field(seqfield, prop=name_format)
  if(!is.null(bookmark)){
    bookmark = crosstable_clean_names(bookmark)
    bkm = run_bookmark(bookmark, bkm)
  }

  legend_fpar = do.call(fpar, args=c(
    list(ftext(legend_name, name_format),
         bkm,
         ftext(": ", name_format)),
    parse_md(legend, return_list=TRUE)
  ))

  body_add_fpar(doc, legend_fpar, style=legend_style)
}

#' Alternative to [officer::body_add_img()] which adds a `units` choice
#'
#' @param doc an `rdocx` object
#' @param src image filename, the basename of the file must not contain any blank.
#' @param width,height width and height. Can be abbreviated to w and h.
#' @param style paragraph style
#' @param units units for width and height
#' @param ... other arguments to be passed to [officer::body_add_img()]
#'
#' @return The docx object `doc`
#'
#' @seealso [body_add_gg2()]
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom officer body_add_img
#' @examples
#' img.file = file.path( R.home("doc"), "html", "logo.jpg" )
#' if(file.exists(img.file)){
#'     library(officer)
#'     options(crosstable_units="cm")
#'     doc = read_docx() %>%
#'         body_add_normal("This is the R logo.") %>%
#'         body_add_img2(img.file, h=7.6, w=10, style="centered") #or units="cm" without options
#'     #write_and_open(doc)
#' }
body_add_img2 = function(doc, src, width, height,
                         units = getOption("crosstable_units", "in"),
                         style = getOption("crosstable_style_image", doc$default_styles$paragraph),
                         ...){
  units = match.arg(units, c("in", "cm", "mm"))
  to_units = function(x) x/c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
  if(!file.exists(src)){
    cli_abort("File {.file {src}} does not exist", call=parent.frame())
  }
  body_add_img(x=doc, src=src, width=to_units(width), height=to_units(height), style=style, ...)
}



#' Alternative to [officer::body_add_gg()] which uses `ggplot` syntax
#'
#' @param doc An `rdocx` object from **officer**.
#' @param value ggplot object
#' @param width,height width and height. Can be abbreviated to w and h.
#' @param style paragraph style
#' @param res resolution of the png image in ppi (passed to the argument `dpi` of [ggplot2::ggsave()])
#' @param units units for width and height
#' @param add_legend add a legend if the ggplot has a legend attribute (see example)
#' @param bookmark the bookmark of the legend, if applicable
#' @param ... other arguments to be passed to [ggplot2::ggsave()]
#'
#' @return The docx object `doc`
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_class
#' @importFrom rlang caller_arg check_installed
#' @examples
#' library(officer)
#' library(ggplot2)
#' p = ggplot(data=iris, aes(Sepal.Length, Petal.Length)) + geom_point()
#' attr(p, "legend") = "Sepal length by Petal length"
#' crosstable_options(
#'   units="cm",
#'   style_image="centered"
#' )
#' doc = read_docx() %>%
#'  body_add_normal("Text before") %>%
#'  body_add_gg2(p, w=14, h=10, scale=1.5) %>% #or units="cm" instead of using options
#'  body_add_normal("Text after")
#' write_and_open(doc)
body_add_gg2 = function(doc, value,
                        width = getOption("crosstable_gg_width", 6),
                        height = getOption("crosstable_gg_height", 5),
                        units = getOption("crosstable_units", "in"),
                        style = getOption("crosstable_style_image", doc$default_styles$paragraph),
                        add_legend=TRUE, bookmark=NULL,
                        res = 300, ... ){
  check_installed("ggplot2", reason="for function `body_add_gg2()` to work.")
  if(is.null(value)){
    cli_abort("{caller_arg(value)} is NULL")
  }
  assert_class(value, "ggplot")
  units = match.arg(units, c("in", "cm", "mm"))
  file = tempfile(fileext=".png")
  ggplot2::ggsave(file, value, width=width, height=height, units=units, dpi=res, ...)
  on.exit(unlink(file))
  legend = attr(value, "legend")
  if(isTRUE(add_legend) && !is.null(legend)){
    doc = body_add_figure_legend(doc, legend=legend, bookmark=bookmark)
  }
  body_add_img2(doc, src=file, style=style, width=width, height=height, units=units)
}

#' Alternative to [flextable::body_add_flextable()]
#'
#' Extends `body_add_flextable()` by adding:
#' - a legend (if `x` has a `"legend"` attribute), via [body_add_table_legend()]
#' - an optional empty line after the table
#'
#' @param doc An `rdocx` object from **officer**.
#' @param x A `flextable` object. If it has a `"legend"` attribute, it is added.
#' @param bookmark Optional. Word bookmark name for the legend.
#' @param append_line Whether to add an empty line after the table.
#' @param ... Passed to [flextable::body_add_flextable()].
#'
#' @return The docx object `doc`
#' @export
#'
#' @examples
#' library(officer)
#' library(ggplot2)
#' ft = flextable::flextable(head(iris)) %>%
#'   structure(legend="The iris dataset")
#'
#' doc = read_docx() %>%
#'  body_add_normal("Text before") %>%
#'  body_add_flextable2(ft) %>%
#'  body_add_normal("Text after")
#' write_and_open(doc)
body_add_flextable2 = function(doc, x, bookmark=NULL, append_line=TRUE, ...){
  legend = attr(x, "legend")
  if(is.null(x)){
    cli_abort("{caller_arg(x)} is NULL")
  }
  if(!is.null(legend)){
    doc = body_add_table_legend(doc, legend=legend, bookmark=bookmark)
  }
  doc = body_add_flextable(doc, x, ...)
  if(isTRUE(append_line)){
    doc = body_add_normal(doc)
  }
  doc
}

#' Replace text on several bookmarks at once
#'
#' @param doc a `rdocx` object
#' @param ... named
#' @param envir Environment to evaluate each expression in `glue()`.
#'
#' @importFrom glue glue
#' @importFrom purrr iwalk safely
#' @return The docx object `doc`
#' @author Dan Chaltiel
#' @export
body_replace_text_at_bkms = function(doc, ...,
                                     envir=parent.frame()){
  l=list(...)
  #TODO tester qu'il y a bien un nom à chaque élément!
  iwalk(l, ~{
    .x = glue(.x, .envir=envir)
    x = safely(body_replace_text_at_bkm)(doc, .y, .x)
    if(is.null(x$result)) warning(x$error$message, call.=FALSE)
    else doc = x$result
  })
  doc
}


#' Adds a standard footnote explaining the abbreviations used in a crosstable
#'
#' Use it below [body_add_crosstable()].
#' Footnote: Med: median, IQR: interquartile range, Std: standard deviation. Percentages are expressed in column.
#'
#' @param doc a `rdocx` object
#'
#' @return The docx object `doc`
#'
#' @author Dan Chaltiel
#' @export
body_add_crosstable_footnote = function(doc){
  body_add_normal(doc, "Med: median, IQR: interquartile range, Std: standard deviation. Percentages are expressed in column.")
}




# Officer helpers ---------------------------------------------------------

crosstable_luafilters = function(){
  x = system.file(package="crosstable", "rmarkdown/page-break.lua")
  paste0("--lua-filter=", x)
}

#' List Word bookmarks, including the ones in header and footer
#'
#' This is a correction of [officer::docx_bookmarks()]. See [this PR](https://github.com/davidgohel/officer/pull/313).
#'
#' @param x an `rdocx` object
#' @param return_vector use `TRUE` for compatibility with [officer::docx_bookmarks()]
#' @param target one of c("all", "header", "body", "footer")
#'
#' @return a list with all bookmarks
#'
#' @importFrom checkmate assert_class
#' @importFrom rlang check_installed
#' @author Dan Chaltiel
#' @export
# nocov start
docx_bookmarks2 = function(x, return_vector=FALSE,
                           target=c("all", "header", "body", "footer")) {
  #cannot test nor add examples as there is officer::body_bookmark() but no officer::head_bookmark()

  assert_class(x, "rdocx")
  check_installed("xml2", reason="for function `docx_bookmarks2()` to work.")
  target = match.arg(target)
  doc_ = xml2::xml_find_all(x$doc_obj$get(), "//w:bookmarkStart[@w:name]")
  doc_ = setdiff(xml2::xml_attr(doc_, "name"), "_GoBack")
  head_ = sapply(x$headers, function(h) {
    tmp = xml2::xml_find_all(h$get(), "//w:bookmarkStart[@w:name]")
    setdiff(xml2::xml_attr(tmp, "name"), "_GoBack")
  })
  foot_ = sapply(x$footers, function(f) {
    tmp = xml2::xml_find_all(f$get(), "//w:bookmarkStart[@w:name]")
    setdiff(xml2::xml_attr(tmp, "name"), "_GoBack")
  })

  rtn = list(header=unname(unlist(head_)), body=unname(unlist(doc_)), footer=unname(unlist(foot_)))
  if(target!="all"){
    rtn = rtn[target]
  }
  if(return_vector) return(unname(unlist(rtn)))
  rtn
}
# nocov end


#' Alternative to default `officer` print() function. Write the file and try to open it right away.
#'
#' As it tests if the file is writable, this function also prevents `officer:::print.rdocx()` to abort the RStudio session.
#'
#' @param doc the docx object
#' @param docx.file the name of the target file. If missing or NULL, the doc will open in a temporary file.
#'
#' @return Nothing, called for its side effects
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom cli cli_abort
#' @importFrom stringr str_detect
#' @importFrom utils browseURL
#'
#' @examples
#' library(officer)
#' library(crosstable)
#' mytable = crosstable(mtcars2)
#' doc = read_docx() %>%
#'     body_add_crosstable(mytable)
#'
#' write_and_open(doc)
#' \dontrun{
#' write_and_open(doc, "example.docx")
#' }
# nocov start
write_and_open = function(doc, docx.file){

  #checking if the file is already open... by removing it
  tryCatch({
    if(missing(docx.file) || is.null(docx.file)){
      docx.file = tempfile(fileext=".docx")
    } else if(file.exists(docx.file)) {
      file.remove(docx.file)
    }
  }, warning=function(w) {
    message(w)
    if(str_detect(w$message, "Permission denied")){
      cli_abort(c("Permission denied. Is the file already open?",
                  i="File: {docx.file}"),
                class="crosstable_permission_denied")
    }
  })

  tryCatch({
    print(doc, target=docx.file)
    if(interactive()) browseURL(normalizePath(docx.file))
  }, error=function(e) {
    if(str_detect(e$message, "Permission denied")){
      cli_abort(c("Permission denied. Is the file already open?",
                  i="File: {docx.file}"),
                class="crosstable_permission_denied")
    }
    stop(e)
  }, warning=function(w) {
    warning(w)
  }, finally={}
  )
}
# nocov end



# External utils ---------------------------------------------------------

# nocov start
#' Generate a macro file for autofitting
#'
#' Autofitting using existing tools in flextable should be enough for most cases. For the others, here is a VBA macro which autofits all tables from inside MS Word.
#' This function generates a file that can be imported into MS Word in order to use this macro. The macro file should be imported only once per computer.
#'
#' @section Installation:
#'  * In the `R` console, run `generate_autofit_macro()` to generate the file `crosstable_autofit.bas` in your working directory.
#'  * In MS Word, press Alt+F11 to open the VB Editor.
#'  * In the Editor, go to `File` > `Import` or press `Ctrl+M` to open the import dialog, and import `crosstable_autofit.bas`. There should now be a "CrosstableMacros" module in the "Normal" project.
#'  * Run the macro, either from the VB Editor or from `View` > `Macros` > `View Macros` > `Run`.
#'
#'  This process will make the macro accessible from any Word file on this computer. Note that, in the Editor, you can also drag the module to your document project to make the macro accessible only from this file. The file will have to be named with the `docm` extension though.
#'
#' @return Nothing, called for its side effects
#' @author Dan Chaltiel
#' @export
generate_autofit_macro = function(){
  fileConn = file("crosstable_autofit.bas")
  writeLines(c(
    'Attribute VB_Name = "CrosstableMacros"',
    'Sub CrosstableAutofitAll()',
    '\tDim t As Table',
    '\tFor Each t In ActiveDocument.Tables',
    '\t\tt.AutoFitBehavior wdAutoFitContent',
    '\t\tt.AutoFitBehavior wdAutoFitWindow',
    '\tNext t',
    'End Sub'
  ), fileConn)
  close(fileConn)
  invisible(NULL)
}

# nocov end

# Internal utils ---------------------------------------------------------


#' Parse value for multiple regexp to unravel formats (bold, italic and underline) and reference calls.
#'
#' @importFrom officer body_add_par body_add_fpar
#'
#' @keywords internal
#' @noRd
body_add_parsed = function(doc, value, style, parse_ref=TRUE, parse_format=TRUE,
                           parse_code=TRUE, parse_newline=TRUE, font_size=NA, ...){
  p = parse_md(value, parse_ref=parse_ref, parse_format=parse_format, parse_code=parse_code,
               parse_newline=parse_newline, font_size=font_size, return_list=FALSE)
  body_add_fpar(doc, p, style, ...)
}

utils::globalVariables(c("do", "end", "start"))

#' Compile Markdown to `officer` formatted paragraph
#' @return a `fpar`
#' @importFrom dplyr arrange bind_rows case_when everything group_split lag lead mutate mutate_all select
#' @importFrom glue glue
#' @importFrom officer fp_text_lite ftext run_linebreak run_word_field
#' @importFrom purrr accumulate
#' @importFrom stringr fixed str_extract str_locate_all str_replace_all
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr fill replace_na unpack
#' @importFrom utils head
#' @keywords internal
#' @noRd
parse_md = function(x, parse_ref=TRUE, parse_format=TRUE, parse_code=TRUE, parse_newline=TRUE,
                    return_list=FALSE, font_size=NA){
  if(nchar(x)==0) return(fpar(x))

  x = str_replace_all(x, fixed("**"), fixed("%%")) #better separates bold from italic

  f = function(x, k, f) str_locate_all(x, k)[[1]] %>% as_tibble() %>% mutate(format=f)
  bolds = f(x, "(?<!\\\\)%%", "bold")
  italics = f(x, "(?<!\\\\)\\*", "italic")
  underlined = f(x, "(?<!\\\\)_", "underlined")
  code = f(x, "(?<!\\\\)`", "code")
  subscript = f(x, "(?<!\\\\)~", "subscript")
  superscript = f(x, "(?<!\\\\)\\^", "superscript")
  ref = f(x, "\\\\?\\\\?@ref\\((.*?)\\)", "ref")
  newline = f(x, "<br> *", "newline")
  #TODO warning si TOKEN non refermé ?

  color = f(x, "<color:\\S+?>|</color>", "color")
  font = f(x, "<ff:\\S+?>|</ff>", "font")
  shade = f(x, "<shade:\\S+?>|</shade>", "shade")

  rtn = tibble()
  if(parse_ref)     rtn = bind_rows(rtn, ref)
  if(parse_format)  rtn = bind_rows(rtn, bolds, italics, underlined, color,
                                    shade, font, subscript, superscript)
  if(parse_code)    rtn = bind_rows(rtn, code)
  if(parse_newline) rtn = bind_rows(rtn, newline)
  if(nrow(rtn)==0) return(fpar(x))

  #remove tokens inside refs
  reflines = rtn %>% filter(format=="ref") %>% select(-format) %>%
    group_split(row_number(), .keep = FALSE)
  for(i in reflines){
    rtn = rtn %>% filter(! (start>i$start & end<i$end))
  }


  state0 = tibble(bold=0, italic=0, underlined=0, code=0, ref=0, newline=0,
                  color=0, subscript=0, superscript=0, font=0, shade=0)
  get_state = function(state, x){
    if(is.na(x)) return(NA)
    state[[x]] = 1-state[[x]]
    state
  }
  rtn = rtn %>%
    arrange(start) %>%
    mutate(
      do = purrr::accumulate(lead(format), .init=get_state(state0, format[1]),
                             ~get_state(.x, .y)) %>%
        head(-1) %>%
        mutate_all(~{
          lag_x = lag(.x, default=0)
          case_when(
            lag_x==0 & .x==1 ~ TRUE,
            lag_x==1 & .x==0 ~ FALSE,
            .default=NA
          )
        }) %>%
        mutate(color2 = 1,)
    ) %>%
    unpack(do) %>%
    mutate(
      txt = substring(x, start, end),
      color=ifelse(color, str_extract(txt, "<color:(\\S+?)>", group=1), "no"),
      shade=ifelse(shade, str_extract(txt, "<shade:(\\S+?)>", group=1), "no"),
      font =ifelse(font,  str_extract(txt, "<ff:(\\S+?)>", group=1), "no"),
    ) %>%
    fill(everything()) %>%
    mutate(
      across(-c(color, shade, font), ~replace_na(.x, FALSE)),
      across(c(color, shade, font), ~na_if(as.character(.x), "no")),
      font = ifelse(code, getOption("crosstable_font_code", "Consolas"), font),
      valign = case_when(subscript ~ "subscript", superscript ~ "superscript",
                         .default="baseline")
    ) %>%
    select(-ref)

  p = list()
  fmt = fp_text_lite(font.size=font_size)
  p[[1]] = ftext(substring(x, 1, rtn$start[1]-1), fmt)
  for(i in seq(nrow(rtn))){
    d = as.list(rtn[i, ])
    if(!is.na(d$shade)){
      #bug in fp_text_lite(), change when https://github.com/davidgohel/officer/issues/538 is fixed
      if(!is.na(d$color)) cli_warn("Shade can only be added to default black text.")
      d$color = "black"
    }

    fmt = fp_text_lite(bold=d$bold, italic=d$italic, underlined=d$underlined, color=d$color,
                       shading.color=d$shade, font.family=d$font, vertical.align=d$valign,
                       font.size=font_size)
    next_start = rtn[i+1, ][["start"]]-1

    if(d$format=="ref"){
      bkm = substring(x, d$start, d$end) %>%
        str_extract("\\\\?\\\\?@ref\\((.*?)\\)", group=1)
      p[[length(p)+1]] = run_word_field(glue(' REF {bkm} \\h '), prop=fmt)
    } else if(d$format=="newline"){
      p[[length(p)+1]] = run_linebreak()
    }

    if(is.na(next_start)) next_start=nchar(x)
    txt = substring(x, d$end+1, next_start)
    txt = txt %>% str_replace_all("\\\\([*`%])", "\\1") #delete escapings
    p[[length(p)+1]] = ftext(txt, prop=fmt)
  }

  if(return_list) return(p)
  do.call(fpar, args=p)
}


# Deprecated --------------------------------------------------------------


#' @usage NULL
#' @importFrom lifecycle deprecate_warn
#' @rdname body_add_normal
#' @export
body_add_glued = function(...){
  deprecate_warn("0.2.0", "body_add_glued()", "body_add_normal()")# nocov
  body_add_normal(...)# nocov
}
