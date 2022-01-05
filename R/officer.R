
# Crosstable --------------------------------------------------------------


#' Add a crosstable to an `officer` document
#' @description [body_add_crosstable()] adds such a `flextable` an `officer` document.
#'
#' @param doc a `rdocx` object, created by [officer::read_docx()]
#' @param x a `crosstable` object
#' @param body_fontsize fontsize of the body
#' @param header_fontsize fontsize of the header
#' @param padding_v vertical padding of all table rows
#' @param ... further arguments passed to [as_flextable.crosstable()]
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom flextable body_add_flextable fontsize padding
#' @importFrom checkmate assert_class vname
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
                                padding_v=NULL, ...) {
    assert_class(x, "crosstable", .var.name=vname(x))
    
    if(missing(padding_v)) padding_v = getOption("crosstable_padding_v", NULL)
    if(missing(body_fontsize)) body_fontsize = getOption("crosstable_fontsize_body", NULL)
    if(missing(header_fontsize)) header_fontsize = getOption("crosstable_fontsize_header", NULL)
    ft = as_flextable(x, ...)
    if(length(body_fontsize)!=0)
        ft = fontsize(ft, size = body_fontsize, part = "body")
    if(length(header_fontsize)!=0)
        ft = fontsize(ft, size = header_fontsize, part = "header")
    if(length(padding_v)!=0)
        ft = padding(ft, padding.top=padding_v, padding.bottom=padding_v, part = "body")
    
    body_add_flextable(doc, ft)
}



# Officer add_xxx ---------------------------------------------------------



#' Add a new paragraph with default style 
#' 
#' Add a new paragraph in an `officer` document with default style.\cr
#' Variables can be inserted in the text as multiple strings (`paste()` style) or enclosed by braces (`glue()` style).  \cr
#' Basic markdown syntax is available: `**bold**`, `*italic*`, and `_underlined_`. \cr
#' References to any bookmark can be inserted using the syntax `\\@ref(bookmark)`.
#'
#' @param doc the doc object (created with the `read_docx` function of `officer` package)
#' @param ... one or several character strings, pasted using `.sep`. As with `glue::glue()`, expressions enclosed by braces will be evaluated as R code. If more than one variable is passed, all should be of length 1.
#' @param style Style for normal text. Best set with [crosstable_options()].
#' @param .sep Separator used to separate elements.
#' @param squish Whether to squish the result (remove trailing and repeated spaces). Default to `TRUE`. Allows to add multiline paragraph without breaking the string.
#' @param parse which format to parse. Default to all formats (`c("ref", "format", "code")`).
#'
#' @return a new doc object
#' 
#' @author Dan Chaltiel
#' @export
#' @importFrom glue glue glue_collapse
#' @importFrom officer body_add_par
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
#'                     \\@ref(my_table). Multiple spaces are ignored (squished) so that you 
#'                     can enter multiline text.") %>% 
#'     body_add_normal() %>% 
#'     body_add_normal("Here I should use `body_add_crosstable()` to add a table before the 
#'                      legend.") %>% 
#'     body_add_table_legend("My pretty table", bookmark="my_table")
#' write_and_open(doc)
body_add_normal = function(doc, ..., .sep="", style=NULL, squish=TRUE, parse=c("ref", "format", "code")) {
    if(missing(squish)) squish = getOption("crosstable_normal_squish", TRUE)
    dots = list(...)
    if(is.null(style)){
        style = getOption('crosstable_style_normal', doc$default_styles$paragraph)
    }
    if(length(dots)==0) dots=""
    dots_lengths = lengths(dots)
    
    if(all(dots_lengths==1)){ #one or several vectors of length 1
        value = do.call(glue, c(dots, .sep=.sep, .envir=parent.frame()))
        if(squish) value = str_squish(value)
        parse_ref = "ref" %in% parse
        parse_format = "format" %in% parse
        parse_code = "code" %in% parse
        doc = body_add_parsed(doc, value, style, parse_ref, parse_format, parse_code)
    } else if(length(dots)==1) { #one vector (of 1 or more) -> recursive call
        for(i in dots[[1]]){
            doc = body_add_normal(doc, i, .sep=.sep, squish=squish)
        }
    } else { #several vectors of which at least one is length 2+
        abort(c("body_add_normal() only accepts either one vector of any length or several vectors of length 1", 
                i=glue("Length of vectors passed: {glue_collapse(dots_lengths, ', ')}")),
              class="officer_wrong_vector_error")
    }
    
    doc
}




#' Add a title to an `officer` document
#'
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param value a character string
#' @param level the level of the title. See \code{styles_info(doc)} to know the possibilities.
#' @param squish Whether to squish the result (remove trailing and repeated spaces). Default to `TRUE`.
#' @param style the name of the title style. See \code{styles_info(doc)} to know the possibilities.
#' 
#' @return The docx object `doc`
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom officer body_add_par
#' @importFrom stringr str_squish
#' @importFrom glue glue
#' @examples
#' library(officer)
#' library(crosstable)
#' library(dplyr)
#' doc = read_docx() %>% 
#'    body_add_title("La table iris (nrow={nrow(iris)})", 1) %>% 
#'    body_add_title("Description", 2) %>% 
#'    body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#' #write_and_open(doc)
body_add_title = function(doc, value, level = 1, squish=TRUE, 
                          style = getOption('crosstable_style_heading', "heading")) {
    if(missing(squish)) squish = getOption("crosstable_title_squish", TRUE)
    value = glue(value, .envir = parent.frame())
    if(squish) value = str_squish(value)
    style = paste(style, level)
    # body_add_par(doc, value, style = style)
    body_add_parsed(doc, value, style = style)
}



#' Add a list to an `officer` document
#'
#' @param doc a docx object
#' @param value a character (`body_add_list()`) or a string (`body_add_list_item`) 
#' @param ordered if TRUE, adds an ordered list, if FALSE, adds a bullet list
#' @param style specify the style manually, overriding `ordered`. A better way is to set options `crosstable_style_list_ordered` and `crosstable_style_list_unordered` globally. 
#' @param ... passed on to [officer::body_add_par()]
#' 
#' @return The docx object `doc`
#'
#' @details Ordered lists and bullet lists are not supported by the default officer template (see [https://github.com/davidgohel/officer/issues/262](#262)). You have to manually set custom styles matching those list in a custom Word template file. Then, you can use either the `style` argument or crosstable options. See examples for more details.
#'
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
body_add_list_item = function(doc, value, ordered=FALSE, style=NULL, ...){
    if(is.null(style)){
        if(ordered){
            style = getOption('crosstable_style_list_ordered', NULL)
        } else {
            style = getOption('crosstable_style_list_unordered', NULL)
        }
        if(is.null(style)){
            abort("Ordered lists and bullet lists are not supported by the default officer template. You have to set them in a custom template and use either the `style` argument or crosstable options. See `?body_add_list` examples for more details.",
                  class="officer_lists_style_error") #nocov
        }
    }
    body_add_parsed(doc, value, style=style, ...)
}



#' Add a list of crosstables
#' 
#' Add a list of crosstables in an officer document
#'
#' @param doc a `rdocx` object, created by [officer::read_docx()]
#' @param l a named list of tables. Plain dataframes will be converted to flextables.
#' @param fun a function to be used before each table, most likely to add some kind of title. Should be of the form `function(doc, .name)` where `.name` is the name of the current crosstable of the list. You can also pass `"title2"` to add the name as a title of level 2 between each table, `"newline"` to simply add a new line, or even NULL to not separate them (beware that the table might merge then).
#' @param ... arguments passed on to [body_add_crosstable()] or [body_add_flextable()]
#'
#' @importFrom checkmate assert_list assert_named assert_class assert_multi_class
#' @importFrom rlang abort
#' @importFrom glue glue
#' 
#' @return The docx object `doc`
#' @export
#' 
#' @examples 
#' library(officer)
#' ctl = list(iris2=crosstable(iris2, 1),
#'            mtcars2=crosstable(mtcars2, 1),
#'            "just a flextable"=flextable::flextable(mtcars2[1:5,1:5]))
#' 
#' myfun = function(doc, .name){
#'     doc %>% 
#'         body_add_title(" This is table '{.name}' as a flex/crosstable", level=2) %>%
#'         body_add_normal("Here is the table:")
#' }
#' 
#' read_docx() %>% 
#'     body_add_title("Separated by subtitle", 1) %>% 
#'     body_add_crosstable_list(ctl, fun="title2") %>% 
#'     body_add_title("Separated by new line", 1) %>% 
#'     body_add_crosstable_list(ctl, fun="newline") %>% 
#'     body_add_title("Separated using a custom function", 1) %>% 
#'     body_add_crosstable_list(ctl, fun=myfun, body_fontsize=8) %>% 
#'     write_and_open()
body_add_crosstable_list = function(doc, l, fun="title2", ...){
    assert_list(l)
    assert_named(l)
    l = map(l, ~{
        if(!is.crosstable(.x) && is.data.frame(.x)) .x = flextable(.x)
        assert_multi_class(.x, c("flextable", "crosstable"))
        .x
    })
    
    if(is_string(fun)){
        if(fun=="title2") fun=function(doc, .name) body_add_title(doc, .name, 2)
        else if(fun=="title3") fun=function(doc, .name) body_add_title(doc, .name, 3)
        else if(fun=="title4") fun=function(doc, .name) body_add_title(doc, .name, 4)
        else if(fun=="newline") fun=function(doc, .name) body_add_normal(doc, "")
        else {
            abort(c('`fun` should be either a function or a member of c("title2", "title3", "title4", "newline")', 
                    i=glue("Current value: '{fun}'")), 
                  class="body_add_crosstable_list_fun_name")
        }
    } else if(is.null(fun)) fun=function(doc, .name) doc
    assert_class(fun, "function")
    
    if(!identical(formalArgs(fun), c("doc", ".name"))){
        abort(c('`fun` should be of the form `function(doc, .name)`', 
                i=paste0("Current arg names: ", paste0(formalArgs(fun), collapse=", "))), 
              class="body_add_crosstable_list_fun_args")
    }
    
    argnames = names(list(...))
    for(i in names(l)){
        x=l[[i]]
        doc = doc %>% fun(.name=i)
        if(is.crosstable(x)) {
            args = intersect(argnames, names(as.list(args(body_add_crosstable))))
            doc = do.call(body_add_crosstable, c(list(doc=doc, x=x), list(...)[args]))
        } else {       
            args = intersect(argnames, names(as.list(args(body_add_flextable))))
            doc = do.call(body_add_flextable, c(list(x=doc, value=x), list(...)[args]))
        }
    }
    
    doc
}

#' @rdname body_add_crosstable_list
#' @export
body_add_flextable_list = body_add_crosstable_list


#' Add a legend to a table or a figure 
#' 
#' Add a legend to a table or a figure in an `officer` document. Legends can be referred to using the `@ref` syntax in [body_add_normal()] (see examples for some use cases). Table legends should be inserted before the table while figure legends should be inserted after the figure.
#'
#' @param doc a docx object
#' @param legend the table legend. As with [glue::glue()], expressions enclosed by braces will be evaluated as R code.
#' @param bookmark the id of the bookmark. This is the id that should then be called in [body_add_normal()] using the `"\\@ref(id)"` syntax.
#' @param legend_prefix a prefix that comes before the legend, after the numbering
#' @param legend_style style of of the whole legend. May depend on the docx template. However, if `name_format` is provided with a specific `font.size`, this size will apply to the whole legend for consistency.
#' @param name_format format of the legend's LHS (legend_name + numbering) using [officer::fp_text_lite()] or [officer::fp_text()]. Default to `fp_text_lite(bold=TRUE)` in addition to the format defined in `legend_style`. Note that the reference to the bookmark will have the same specific format in the text.
#' @param legend_name name before the numbering. Default to either "Table" or "Figure".
#' @param style deprecated in favor of `name_format`.
#' @param seqfield Keep default. Otherwise, you may figure it out doing this: in a docx file, insert a table legend, right click on the inserted number and select "Toggle Field Codes". This argument should be the value of the field, with extra escaping.
#' @param par_before,par_after should an empty paragraph be inserted before/after the legend?
#' @param legacy use the old version of this function, if you cannot update `{officer}` to v0.4+
#' 
#' @return The docx object `doc`
#'
#' @section Warning:
#' Be aware that you unfortunately cannot reference a bookmark more than once using this method. Writing: \cr `body_add_normal("Table \\@ref(iris_col1) is about flowers. I like this Table \\@ref(iris_col1).")`\cr
#' will prevent the numbering from applying.
#' @section What to do if there is still no numbering?:
#' During the opening of the document, MS Word might ask you to "update the fields", to which you should answer "Yes".  \cr
#' If it is not asked or if you answer "No", the legends added with [body_add_table_legend()] or [body_add_figure_legend()] might have no actual numbers displayed. \cr
#' In this case, you have to manually update the references in MS Word: select all (\kbd{Ctrl}+\kbd{A}), then update (\kbd{F9}), sometimes twice. More info on [https://ardata-fr.github.io/officeverse/faq.html#update-fields](https://ardata-fr.github.io/officeverse/faq.html#update-fields).
#' @rdname body_add_legend
#' @name body_add_legend
#' @author Dan Chaltiel
#' @importFrom utils packageVersion
#' @importFrom rlang is_missing warn
#' @importFrom lifecycle is_present deprecate_warn
#' @export
#' 
#' @examples 
#' library(officer)
#' p=ggplot2::quickplot(x=Sepal.Length, y=Sepal.Width, color=Species, data=iris)
#' fp_italic = fp_text_lite(italic=TRUE, font.size=10)
#' x=read_docx() %>%
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
body_add_table_legend = function(doc, legend, bookmark=NULL, 
                                 legend_style=getOption('crosstable_style_legend', 
                                                        doc$default_styles$paragraph), 
                                 style=deprecated(), 
                                 legend_prefix=NULL,
                                 name_format=NULL,
                                 legend_name="Table", 
                                 seqfield="SEQ Table \\* Arabic", 
                                 par_before=FALSE,
                                 legacy=FALSE){
    if(missing(par_before)) par_before = getOption("crosstable_table_legend_par_before", FALSE)
    if(missing(legend_prefix)) legend_prefix = getOption("crosstable_table_legend_prefix", NULL)
    if(par_before){
        doc=body_add_normal(doc, "")
    }
    body_add_legend(doc=doc, legend=legend, legend_name=legend_name,
                    bookmark=bookmark, legend_prefix=legend_prefix, legend_style=legend_style,
                    name_format=name_format, seqfield=seqfield, 
                    style=style, legacy=legacy)
}

#' @rdname body_add_legend
#' @export
body_add_figure_legend = function(doc, legend, bookmark=NULL, 
                                  legend_style=getOption('crosstable_style_legend', 
                                                         doc$default_styles$paragraph), 
                                  style=deprecated(), 
                                  legend_prefix=NULL,
                                  name_format=NULL,
                                  legend_name="Figure", 
                                  seqfield="SEQ Figure \\* Arabic", 
                                  par_after=FALSE,
                                  legacy=FALSE){
    if(missing(par_after)) par_after = getOption("crosstable_figure_legend_par_after", FALSE)
    if(missing(legend_prefix)) legend_prefix = getOption("crosstable_figure_legend_prefix", NULL)
    doc = body_add_legend(doc=doc, legend=legend, legend_name=legend_name,
                    bookmark=bookmark, legend_prefix=legend_prefix, legend_style=legend_style,
                    name_format=name_format, seqfield=seqfield, 
                    style=style, legacy=legacy)
    if(par_after){
        doc=body_add_normal(doc, "")
    }
    doc
}


#' @importFrom glue glue
#' @importFrom officer ftext fpar run_bookmark run_word_field body_add_fpar
#' @keywords internal
#' @noRd
body_add_legend = function(doc, legend, legend_name, bookmark, 
                           legend_prefix, legend_style, name_format, seqfield, 
                           style, legacy){
    
    # nocov start
    if(packageVersion("officer")<"0.4" || legacy){
        if(!legacy){
            warn("You might want to update officer to v0.4+ in order to get the best of crosstable::body_add_xxx_legend().", 
                 .frequency="once", 
                 .frequency_id="body_add_xxx_legend_officer_version")
        }
        if(is_missing(style)){
            style = getOption('crosstable_style_strong', "strong")
        }
        
        
        rtn = body_add_legend_legacy(doc=doc, legend=legend, legend_name=legend_name,
                               bookmark=bookmark, legend_style=legend_style, style=style, seqfield=seqfield)
        return(rtn)
    } 
    
    if(is_present(style)){
        deprecate_warn("0.2.2", "body_add_X_legend(style)", 
                       "body_add_X_legend(name_format)", 
                       details="Therefore, its value has been ignored. Use `legacy=TRUE` to override.")
    }
    # nocov end
    
    legend = paste0(legend_prefix, legend)
    fp_text2 = officer::fp_text_lite #v0.4+
    if(is.null(name_format)){
        name_format = getOption('crosstable_format_legend_name', fp_text2(bold=TRUE))
    }
    fp_size = fp_text2(font.size=name_format$font.size)
    
    legend = glue(legend, .envir = parent.frame())
    legend_name = paste0(legend_name, " ")
    
    bkm = run_word_field(seqfield, prop=name_format)
    if(!is.null(bookmark)){
        bkm = run_bookmark(bookmark, bkm)
    }
    
    legend_fpar = fpar(
        ftext(legend_name, name_format), 
        bkm,
        ftext(": ", name_format), 
        ftext(legend, fp_size)
    )
    
    body_add_fpar(doc, legend_fpar, style=legend_style)
}


# nocov start
#' @importFrom glue glue
#' @importFrom officer body_add_par slip_in_text slip_in_seqfield body_bookmark
#' @keywords internal
#' @noRd
body_add_legend_legacy = function(doc, legend, legend_name, bookmark, 
                                  legend_style, style, seqfield){
    legend = glue(legend, .envir = parent.frame())
    rtn = doc %>% 
        body_add_par(value=legend, style=legend_style) %>% 
        slip_in_text(str=": ", style=style, pos="before") %>% 
        slip_in_seqfield(str=seqfield, style=style, pos="before")
    if(!is.null(bookmark)){
        rtn = body_bookmark(rtn, bookmark)
    }
    slip_in_text(rtn, str=glue("{legend_name} "), style=style, pos="before")
}
# nocov end



#' Alternative to [officer::body_add_img()] which adds a `units` choice
#' 
#' @param doc an `rdocx` object
#' @param src image filename, the basename of the file must not contain any blank.
#' @param width,height width and height. Can be abbreviated to w and h.
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
                         ...){
    units = match.arg(units, c("in", "cm", "mm"))
    to_units = function(x) x/c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units] 
    body_add_img(x=doc, src=src, width=to_units(width), height=to_units(height), ...)
}



#' Alternative to [officer::body_add_gg()] which uses `ggplot` syntax
#'  
#' @param doc an `rdocx` object
#' @param value ggplot object
#' @param width,height width and height. Can be abbreviated to w and h.
#' @param style paragraph style
#' @param res resolution of the png image in ppi (passed to the argument `dpi` of [ggplot2::ggsave()])
#' @param units units for width and height
#' @param ... other arguments to be passed to [ggplot2::ggsave()]
#' 
#' @return The docx object `doc`
#' 
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_class
#' @examples
#' if(require("ggplot2") && capabilities(what = "png")){
#'   library(officer)
#'   p = ggplot(data = iris ) +
#'     geom_point(mapping = aes(Sepal.Length, Petal.Length))
#'   
#'   options(crosstable_units="cm")
#'   options(crosstable_style_image="centered")
#'   doc = read_docx() %>%
#'     body_add_normal("Text before") %>% 
#'     body_add_gg2(p, w=14, h=10, scale=1.5) %>% #or units="cm" instead of using options
#'     body_add_normal("Text after")
#'   #write_and_open(doc)
#' }
body_add_gg2 = function(doc, value, width = 6, height = 5, 
                        units = getOption("crosstable_units", "in"), 
                        style = getOption("crosstable_style_image", doc$default_styles$paragraph), 
                        res = 300, ... ){
    assert_is_installed("ggplot2", "body_add_gg2()")
    assert_class(value, "ggplot")
    units = match.arg(units, c("in", "cm", "mm"))
    file = tempfile(fileext=".png")
    ggplot2::ggsave(file, value, width=width, height=height, units=units, dpi=res, ...)
    on.exit(unlink(file))
    body_add_img2(doc, src=file, style=style, width=width, height=height, units=units)
}



#' Replace text on several bookmarks at once
#'
#' @param doc a `rdocx` object
#' @param ... named
#' 
#' @importFrom officer body_replace_text_at_bkm
#' @importFrom purrr iwalk safely
#' @return The docx object `doc`
#' @author Dan Chaltiel
#' @export
body_replace_text_at_bkms = function(doc, ...){
    l=list(...)
    #TODO tester qu'il y a bien un nom à chaque élément!
    iwalk(l, ~{
        .x = glue(.x, .envir=parent.frame())
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
#' @author Dan Chaltiel
#' @export
docx_bookmarks2 = function(x, return_vector=FALSE, 
                           target=c("all", "header", "body", "footer")) {#nocov start
    #cannot test nor add examples as there is officer::body_bookmark() but no officer::head_bookmark()
    
    assert_class(x, "rdocx")  
    assert_is_installed("xml2", "docx_bookmarks2()")
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
}#nocov end


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
#' @importFrom utils browseURL
#' @importFrom rlang abort
#' @importFrom stringr str_detect
#' @importFrom glue glue
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
            abort(c("Permission denied. Is the file already open?", glue("File: {docx.file}")),
                  class="permission_denied")
        }
    })
    
    tryCatch({
        print(doc, target=docx.file)
        if(interactive()) browseURL(docx.file)
    }, error=function(e) {
        if(str_detect(e$message, "Permission denied")){
            abort(c("Permission denied. Is the file already open?", glue("File: {docx.file}")),
                  class="permission_denied")
        }
        stop(e)
    }, warning=function(w) {
        warning(w)
    }, finally={}
    )
    
}    # nocov end



# External utils ---------------------------------------------------------

# nocov start
#' Generate a macro file for autofitting
#'
#' This function generates a file that can be imported into MS Word in order to use a macro for autofitting all tables in a document at once. This macro file should be imported only once per computer.
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
    fileConn<-file("crosstable_autofit.bas")
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
#' @importFrom stringr str_split str_detect str_match str_extract_all
#' @importFrom glue glue
#' @importFrom utils packageVersion
#' @importFrom purrr map map_lgl discard set_names
#' @importFrom rlang warn
#' @importFrom officer run_word_field ftext body_add_fpar fp_text_lite
#' 
#' @keywords internal
#' @noRd
body_add_parsed = function(doc, value, style, parse_ref=TRUE, parse_format=TRUE, parse_code=TRUE){
    if(packageVersion("officer")<"0.4"){
        warn("This function needs package {officer} v0.4+ to work. You won't be able to add formatted text or references until you update this package.")
        return(doc)
    }
    if(isFALSE(parse_ref) && isFALSE(parse_format)){
        body_add_par(doc, value, style)
    }
    reg_r = list(
        ref = "\\\\@ref\\(.*?\\)"
    )
    reg_f = list(
        bold = "\\*\\*(.+?)\\*\\*",
        underlined = "_(.+?)_",
        italic = "(?<!\\*)\\*(?!\\*)(.+?)(?<!\\*)\\*(?!\\*)"
    )
    reg_c = list(
        code = "`(.+?)`"
    )
    if(isFALSE(parse_ref)) reg_r = list()
    if(isFALSE(parse_format)) reg_f = list()
    if(isFALSE(parse_code)) reg_c = list()
    regex = c(reg_f, reg_r, reg_c)
    rex_all = paste(regex, collapse="|")
    
    par_not_format = str_split(value, rex_all)[[1]]
    par_format = str_extract_all(value, rex_all)[[1]]
    
    # #altern: https://stackoverflow.com/a/43876294/3888000
    altern = c(par_not_format, par_format)[order(c(seq_along(par_not_format)*2 - 1, 
                                                   seq_along(par_format)*2))]
    par_list = map(altern, ~{
        .format = map_lgl(regex, function(pat) str_detect(.x, pattern=pat)) %>% 
            discard(isFALSE) %>% names()
        
        if(length(.format)==0) return(ftext(.x))
        
        if(any(.format=="ref")){
            bkm = str_match(.x, "\\\\@ref\\((.*?)\\)")[,2]
            return(run_word_field(glue(' REF {bkm} \\h ')))
        }
        if(any(.format=="code")){
            fp = fp_text_lite(font.family=getOption("crosstable_font_code", "Consolas"))
            return( ftext(.x, fp))
        }
        rex = regex[.format]
        for(i in rex){
            if(str_detect(.x, i)){
                .x = str_match(.x, i)[[2]]
            }
        }
        
        fp_args = rep(TRUE, length(.format)) %>% set_names(.format) %>% as.list()
        fp = do.call(fp_text_lite, fp_args)
        
        ftext(.x, fp)
    })
    
    
    p=do.call(fpar, args=par_list)
    body_add_fpar(doc, p, style)
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
