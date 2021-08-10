
# Crosstable --------------------------------------------------------------


#' Add a crosstable to an `officer` document
#' @description [body_add_crosstable()] adds such a `flextable` an `officer` document.
#'
#' @param doc a `rdocx` object, created by [officer::read_docx()]
#' @param x a `crosstable` object
#' @param body_fontsize fontsize of the body
#' @param header_fontsize fontsize of the header
#' @param ... further arguments passed to [as_flextable.crosstable()]
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom flextable body_add_flextable fontsize
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
                                header_fontsize=ceiling(body_fontsize*1.2), ...) {
    assert_class(x, "crosstable", .var.name=vname(x))
    ft = as_flextable(x, ...)
    if(length(body_fontsize)!=0)
        fontsize(ft, size = body_fontsize, part = "body")
    if(length(header_fontsize)!=0)
        fontsize(ft, size = header_fontsize, part = "header")
    
    body_add_flextable(doc, ft)
}



# Officer add_xxx ---------------------------------------------------------



#' Add a new paragraph with default style 
#' 
#' Add a new paragraph in an `officer` document with default style. Variables can be inserted as multiple strings (`paste()` style) or enclosed by braces (`glue()` style). 
#' References to any bookmark can be inserted using the syntax "\\@ref(bookmark)". See an example in [body_add_table_legend()].
#'
#' @param doc the doc object (created with the `read_docx` function of `officer` package)
#' @param ... one or several character strings, pasted using `.sep`. As with `glue::glue()`, expressions enclosed by braces will be evaluated as R code. If more than one variable is passed, all should be of length 1.
#' @param .sep Separator used to separate elements.
#'
#' @return a new doc object
#' 
#' @author Dan Chaltiel#' 
#' @export
#' @importFrom glue glue glue_collapse
#' @importFrom officer body_add_par
#' @importFrom purrr map_dbl
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
#'     body_add_normal(info_rows)                                              #vector style
#' #write_and_open(doc)
body_add_normal = function(doc, ..., .sep="") {
    dots = list(...)
    normal_style = getOption('crosstable_style_normal', doc$default_styles$paragraph)
    lengths = map_dbl(dots, length)
    
    if(all(lengths==1)){ #one or several vectors of length 1
        value = glue(..., .sep=.sep, .envir=parent.frame())
        if(str_detect(value, "\\\\@ref\\((.*?)\\)")){
            doc = body_add_par(doc, "") %>% parse_reference(value)
        } else{
            doc = body_add_par(doc, value, style=normal_style)
        }
    } else if(length(dots)==1) { #one vector (of 1 or more) -> recursive call
        for(i in dots[[1]]){
            doc = body_add_normal(doc, i, .sep=.sep)
        }
    } else { #several vectors of which at least one is length 2+
        abort(c("body_add_normal() only accepts either one vector of any length or several vectors of length 1", 
                i=glue("Length of vectors passed: {glue_collapse(lengths, ', ')}")),
              class="officer_wrong_vector_error")
    }
    
    doc
}

#' @usage NULL
#' @importFrom lifecycle deprecate_warn
#' @rdname body_add_normal
#' @author Dan Chaltiel
#' @export
body_add_glued = function(...){
    deprecate_warn("0.2.0", "body_add_glued()", "body_add_normal()")# nocov
    body_add_normal(...)# nocov
}




#' Add a title to an `officer` document
#'
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param value a character string
#' @param level the level of the title. See \code{styles_info(doc)} to know the possibilities.
#' @param style the name of the title style. See \code{styles_info(doc)} to know the possibilities.
#' 
#' @return The docx object `doc`
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom officer body_add_par
#' @examples
#' library(officer)
#' library(crosstable)
#' library(dplyr)
#' doc = read_docx()
#' doc = doc %>% 
#'    body_add_title("La table iris", 1) %>% 
#'    body_add_title("Description", 2) %>% 
#'    body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#' #write_and_open(doc)
body_add_title = function(doc, value, level = 1, 
                          style = getOption('crosstable_style_heading', "heading")) {
    style = paste(style, level)
    body_add_par(doc, value, style = style)
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
#' @author Dan Chaltiel
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
    body_add_par(doc, value, style=style, ...)
}



#' Add a table legend to an `officer` document
#'
#' @param doc a docx object
#' @param legend the table legend. As with [glue::glue()], expressions enclosed by braces will be evaluated as R code.
#' @param bookmark the id of the bookmark. This is the id that should then be called in [body_add_normal()] using `"\\@ref(id)"`.
#' @param legend_style style of of the whole legend. May depend on the docx template
#' @param style style of the number. May depend on the docx template (default to strong)
#' @param legend_name name before the numbering. Useful for translation
#' @param seqfield Keep default. Otherwise, you may figure it out doing this: in a docx file, insert a table legend, right click on the inserted number and select "Toggle Field Codes". This argument should be the value of the field, with extra escaping.
#' 
#' @return The docx object `doc`
#'
#' @section Warning:
#' At first, the legends added with [body_add_table_legend()] or [body_add_figure_legend()] have no numbers. You have to manually update the references in MS Word: select all (\kbd{Ctrl}+\kbd{A}), then update (\kbd{F9}). You might have to do this several times. More info on [https://ardata-fr.github.io/officeverse/faq.html#update-fields](https://ardata-fr.github.io/officeverse/faq.html#update-fields).
#' @author Dan Chaltiel
#' @export
#' 
#' @examples 
#' library(officer)
#' p=ggplot2::quickplot(x=Sepal.Length, y=Sepal.Width, color=Species, data=iris)
#' x=read_docx() %>% 
#'   body_add_normal("As you can see in Table \\@ref(tab1) and in Figure \\@ref(fig1), ", 
#'                   "the iris dataset is about flowers.") %>% 
#'   body_add_normal() %>% 
#'   body_add_table_legend("Iris dataset", bookmark="tab1") %>% 
#'   body_add_crosstable(crosstable(iris)) %>% 
#'   body_add_gg(p) %>% 
#'   body_add_figure_legend("Iris plot", bookmark="fig1")
#' write_and_open(x)
#' #press Ctrl+A then F9 twice for the reference to appear.
body_add_table_legend = function(doc, legend, bookmark=NULL, 
                                 legend_style=getOption('crosstable_style_legend', "Table Caption"), 
                                 style=getOption('crosstable_style_strong', "strong"), 
                                 legend_name="Table",
                                 seqfield="SEQ Table \\* Arabic"){
    body_add_legend(doc=doc, legend=legend, legend_name=legend_name, 
                    bookmark=bookmark, legend_style=legend_style, 
                    style=style, seqfield=seqfield)
}

#' @rdname body_add_table_legend
#' @author Dan Chaltiel
#' @export
body_add_figure_legend = function(doc, legend, bookmark=NULL, 
                                  legend_style=getOption('crosstable_style_legend', "Image Caption"), 
                                  style=getOption('crosstable_style_strong', "strong"), 
                                  legend_name="Figure",
                                  seqfield="SEQ Figure \\* Arabic"){
    body_add_legend(doc=doc, legend=legend, legend_name=legend_name, 
                    bookmark=bookmark, legend_style=legend_style, 
                    style=style, seqfield=seqfield)
}

#' @importFrom stringr str_detect str_match_all
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom officer slip_in_text slip_in_seqfield body_bookmark
#' @keywords internal
#' @noRd
body_add_legend = function(doc, legend, legend_name, bookmark, legend_style, style, seqfield){
    legend = glue(legend, .envir = parent.frame())
    rtn = doc %>% 
        body_add_par(value=legend, style=legend_style) %>% 
        slip_in_text(str=": ", style=style, pos="before") %>% 
        slip_in_seqfield(str=seqfield, style=style, pos="before")
    if(!is.null(bookmark)){
        rtn = body_bookmark(rtn, bookmark)
    }
    rtn %>% 
        slip_in_text(str=glue("{legend_name} "), style=style, pos="before")
}



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
    x=system.file(package = "crosstable", "rmarkdown/page-break.lua")
    paste0("--lua-filter=", x)
}

#' List Word bookmarks, including the ones in header and footer
#' 
#' This is a correction of [officer::docx_bookmarks()]. See [this PR](https://github.com/davidgohel/officer/pull/313).
#'
#' @param x an `rdocx` object
#' @param return_vector use `TRUE` for compatibility with [officer::docx_bookmarks()]
#' 
#' @return a list with all bookmarks
#'
#' @importFrom checkmate assert_class
#' @author Dan Chaltiel
#' @export
docx_bookmarks2 = function(x, return_vector=FALSE) {#nocov start
    #cannot test nor add examples as there is officer::body_bookmark() but no officer::head_bookmark()
    assert_class(x, "rdocx")  
    assert_is_installed("xml2", "docx_bookmarks2()")
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
    if(return_vector){
        return(unname(unlist(c(doc_, head_, foot_)))) #alternative return
    }
    
    list(header=unname(unlist(head_)), body=unname(unlist(doc_)), footer=unname(unlist(foot_)))
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

#' Generate a macro file for autofitting
#'
#' This function generates a file that can be imported into MS Word in order to use a macro for autofitting all tables in a document at once. This macro file should be imported only once per computer.
#' 
#' @section Installation:
#'  * Run `generate_autofit_macro()` in `R` to generate the file `crosstable_autofit.bas` in your working directory. 
#'  * In MS Word, press Alt+F11 to open the VB Editor.
#'  * In the Editor, go to `File` > `Import` or press `Ctrl+M` to open the import dialog, and import `crosstable_autofit.bas`. There should now be a "CrosstableMacros" module in the "Normal" project.
#'  * Run the macro, either from the VB Editor or from `View` > `Macros` > `View Macros` > `Run`.
#'
#' @return nothing
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


# Internal utils ---------------------------------------------------------


#' Recursive helper function 
#' Replace every string containing a reference to a table/figure by the 
#' docx-formatted cross-reference
#' 
#' @importFrom stringr str_detect str_match_all
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom officer slip_in_text slip_in_seqfield
#' 
#' @keywords internal
#' @noRd
parse_reference = function(doc, value){
    normal_style_character = getOption('crosstable_style_character', doc$default_styles$character)
    
    if(!str_detect(value, "\\\\@ref\\((.*?)\\)")){ #recursion out
        doc = doc %>% 
            slip_in_text(value, style=normal_style_character, pos='after')
        return(doc)
    }
    
    x = str_match_all(value, "(.*?)\\\\@ref\\((.*?)\\)(.*)")
    x = unlist(x[[1]][-1])
    if(length(x)!=3) #1rd (.*) is not greedy
        abort(c("This error should not have happened, please report a bug.", # nocov
                i="function = crosstable:::parse_reference"), x=x)           # nocov
    doc = doc %>% 
        slip_in_text(x[1], style=normal_style_character, pos='after') %>% 
        slip_in_seqfield(str = glue(' REF {x[2]} \\h '),  
                         style=normal_style_character, pos='after') %>%
        parse_reference(x[3])
}
