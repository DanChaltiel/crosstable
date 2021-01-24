
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
#' @export
#' @importFrom flextable body_add_flextable fontsize
#' @importFrom checkmate assert_class vname
#' 
#' @examples 
#' #Officer
#' library(officer)
#' library(dplyr)
#' mytable = crosstable(mtcars2)
#' doc = read_docx() %>% 
#'     body_add_crosstable(mytable) %>% 
#'     body_add_break %>% 
#'     body_add_crosstable(mytable, compact=TRUE)
#' 
#' \dontrun{
#' dfile = "examples\\example_doc.docx"
#' print(doc, target = dfile)
#' shell.exec(dfile)
#' }
body_add_crosstable = function (doc, x, body_fontsize=NULL, 
                                header_fontsize=ceiling(body_fontsize*1.2), ...) {
    assert_class(x, "crosstable", .var.name=vname(x))
    ft = as_flextable(x, ...)
    if(length(body_fontsize)!=0)
        fontsize(ft, size = body_fontsize, part = "body")
    if(length(header_fontsize)!=0)
        fontsize(ft, size = header_fontsize, part = "header")
    doc = body_add_flextable(doc, ft)
    return(doc)
}



# Officer helpers ---------------------------------------------------------



#' Add a new paragraph with a Normal style to an `officer` document, inserting variables with \code{base::paste}
#'
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param ... one or several character strings, collapsed into a paragraph with \code{base::paste}
#'
#' @return a new doc object
#' @author Dan Chaltiel
#' @export
#' @importFrom officer body_add_par
#' 
#' @examples
#' \dontrun{
#' library(officer)
#' library(crosstable)
#' library(dplyr)
#' doc = read_docx()
#' doc = doc %>% body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#' }
body_add_normal = function(doc, ...) {
    value = paste0(..., collapse = "")
    if(str_detect(value, "\\\\@ref\\((.*?)\\)")){
        doc %>% 
            body_add_par("") %>% 
            parse_reference(value)
    } else{
        doc %>% 
            body_add_par(value, style = getOption('crosstable_style_normal', doc$default_styles$paragraph))
    }
}



#' Add a new paragraph with a Normal style to an `officer` document, inserting variables with `glue::glue`
#'
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param .x the string with \code{glue::glue} patterns (Expressions enclosed by braces will be evaluated as R code)
#' @param ... parameters to be passed to \code{glue::glue}
#' @return a new doc object
#' @author Dan Chaltiel
#' @importFrom glue glue
#' @importFrom officer body_add_par
#' @examples
#' \dontrun{
#' library(officer)
#' library(crosstable)
#' library(dplyr)
#' doc = read_docx()
#' doc = doc %>%
#'     body_add_glued("The iris table has {ncol(iris)} columns") %>%
#'     body_add_glued("Actually, it also has {nrows} rows", nrows=nrow(iris))
#' }
#' @export
body_add_glued = function(doc, .x, ...) {
    value = glue::glue(.x, ..., .envir = parent.frame())
    body_add_normal(doc, value)
}



#' Add a title to an `officer` document
#'
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param value a character string
#' @param level the level of the title. See \code{styles_info(doc)} to know the possibilities.
#' @param style the name of the title style. See \code{styles_info(doc)} to know the possibilities.
#'
#' @return a new doc object
#' @author Dan Chaltiel
#' @importFrom officer body_add_par
#' @export
#' @examples
#' \dontrun{
#' library(officer)
#' library(crosstable)
#' library(dplyr)
#' doc = read_docx()
#' doc = doc %>% 
#'    body_add_title("La table iris", 1) %>% 
#'    body_add_title("Description", 2) %>% 
#'    body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#' }
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
#' @details Ordered lists and bullet lists are not supported by the default officer template (see [https://github.com/davidgohel/officer/issues/262](#262)). You have to manually set custom styles matching thos list in a custom Word template file. Then, you can use either the `style` argument or crosstable options. See examples for more details.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(officer)
#' library(crosstable)
#' options(crosstable_style_list_ordered="ordered_list")
#' options(crosstable_style_list_unordered="unordered_list")
#' # of course, my_template.docx should have these styles
#' read_docx("my_template.docx") %>%
#'  body_add_list(c("Numbered item 1", "Numbered item 2"), ordered = TRUE) %>%
#'  body_add_list_item("Numbered item 3", ordered = TRUE) %>%
#'  body_add_list(c("Bullet item 1", "Bullet item 2"), ordered = FALSE) %>%
#'  body_add_list_item("Bullet item 3", ordered = FALSE) %>%
#'  write_and_open("result.docx")
#' }
body_add_list = function(doc, value, ordered=FALSE, style=NULL, ...){
    for(i in value){
        doc = body_add_list_item(doc, i, ordered=ordered, style=style, ...)
    }
    doc
}

#' @rdname body_add_list
#' @export
body_add_list_item = function(doc, value, ordered=FALSE, style=NULL, ...){
    if(is.null(style)){
        if(ordered){
            style = getOption('crosstable_style_list_ordered', NULL)
        } else {
            style = getOption('crosstable_style_list_unordered', NULL)
        }
        if(is.null(style)){
            abort("Ordered lists and bullet lists are not supported by the default officer template. You have to set them in a custom template and use either the `style` argument or crosstable options. See `?body_add_list` examples for more details.")
        }
    }
    body_add_par(doc, value, style=style, ...)
}



#' Add a table legend to an `officer` document
#'
#' @param doc a docx object
#' @param legend the table legend
#' @param legend_style may depend on the docx template
#' @param style the legend style (strong, italic...)
#' @param seqfield to figure this out, in a docx file, insert a table legend, right click on the inserted number and select "Toggle Field Codes". This argument should be the value of the field, with extra escaping.
#'
#' @section Warning:
#' At first, the legends added with [body_add_table_legend()] or [body_add_figure_legend()] have no numbers. You have to manualy update the references in MS Word: select all (\kbd{Ctrl}+\kbd{A}), then update (\kbd{F9}). You might have to do this several times. More info on [https://ardata-fr.github.io/officeverse/faq.html#update-fields].
#' @importFrom officer body_add_par slip_in_text slip_in_seqfield
#' @export
#' 
#' @examples 
#' library(officer)
#' library(dplyr) #for pipes
#' library(ggplot2)
#' p=quickplot(x=Sepal.Length, y=Sepal.Width, color=Species, data=iris)
#' x=read_docx() %>% 
#'     body_add_table_legend("Iris dataset") %>% 
#'     body_add_crosstable(crosstable(iris)) %>% 
#'     body_add_gg(p) %>% 
#'     body_add_figure_legend("Iris plot")
#' #print(x, "example.docx")
body_add_table_legend = function(doc, legend, bookmark=NULL, 
                                 legend_style=getOption('crosstable_style_legend', "caption"), 
                                 style=getOption('crosstable_style_strong', "strong"), 
                                 seqfield="SEQ Table \\* Arabic"){
    rtn = doc %>% 
        body_add_par(value=legend, style=legend_style) %>% 
        slip_in_text(str=": ", style=style, pos="before") %>% 
        slip_in_seqfield(str=seqfield, style=style, pos="before")
    if(!is.null(bookmark)){
        rtn = body_bookmark(rtn, bookmark)
    }
    rtn %>% 
        slip_in_text(str="Table ", style=style, pos="before")
}

#' @rdname body_add_table_legend
#' @export
body_add_figure_legend = function(doc, legend, bookmark=NULL, 
                                  legend_style=getOption('crosstable_style_legend', "caption"), 
                                  style=getOption('crosstable_style_strong', "strong"), 
                                  seqfield="SEQ Figure \\* Arabic"){
    rtn = doc %>% 
        body_add_par(value=legend, style=legend_style) %>% 
        slip_in_text(str=": ", style=style, pos="before") %>% 
        slip_in_seqfield(str=seqfield, style=style, pos="before")
    if(!is.null(bookmark)){
        rtn = body_bookmark(rtn, bookmark)
    }
    rtn %>% 
        slip_in_text(str="Figure ", style=style, pos="before")
}




#' List Word bookmarks, including the ones in header and footer
#' 
#' This is a correction of [officer::docx_bookmarks()]. See [this PR](https://github.com/davidgohel/officer/pull/313).
#'
#' @param x an `rdocx` object
#' @param return_vector use `TRUE` for compatibility with [officer::docx_bookmarks()]
#'
#' @export
docx_bookmarks2 = function(x, return_vector=FALSE) {
    #cannot add examples as there is officer::body_bookmark() but no officer::head_bookmark()
    stopifnot(inherits(x, "rdocx"))
    checkmate::assert_class("rdocx")  
    doc_ <- xml_find_all(x$doc_obj$get(), "//w:bookmarkStart[@w:name]")
    doc_ <- setdiff(xml_attr(doc_, "name"), "_GoBack")
    head_ <- sapply(x$headers, function(h) {
        tmp <- xml_find_all(h$get(), "//w:bookmarkStart[@w:name]")
        setdiff(xml_attr(tmp, "name"), "_GoBack")
    })
    foot_ <- sapply(x$footers, function(f) {
        tmp <- xml_find_all(f$get(), "//w:bookmarkStart[@w:name]")
        setdiff(xml_attr(tmp, "name"), "_GoBack")
    })
    if(return_vector){
        return(unname(unlist(c(doc_, head_, foot_)))) #alternative return
    }
    
    list(header=unname(unlist(head_)), body=unname(unlist(doc_)), footer=unname(unlist(foot_)))
}


#' Alternative to default `officer` print() function. Write the file and try to open it right away.
#'
#' @param doc the docx object
#' @param docx.file the name of the target file
#'
#' @return
#' @export
#'
#' @examples
#' write_and_open(x, "example.docx")
write_and_open = function(doc, docx.file){
    tryCatch({
        if(file.exists(docx.file)) {
            file.remove(docx.file )
        }
        print(doc, target=docx.file)
        dfile = paste("\"", sub("(^.+)(/$)", "\\", getwd()), "/", docx.file, "\"", sep = "")
        # browseURL(dfile)
        shell.exec(dfile)
    }, error=function(e) {
        message("Error : file is probably already open")
        message(e)
    }, warning=function(w) {
        message("Warning : Le fichier est déjà ouvert !")
        message(w)
    }, finally={}
    )
}



# Internal utils ---------------------------------------------------------


#' Recursive helper function 
#' Replace every string containing a reference to a table/figure by the 
#' docx-formatted cross-reference
#' 
#' @keywords internal
#' @noRd
parse_reference = function(doc, value){
    if(!str_detect(value, "\\\\@ref\\((.*?)\\)")){ #recursion out
        doc = doc %>% 
            slip_in_text(value, 
                         style = 'Default Paragraph Font', 
                         pos = 'after')
        return(doc)
    }
    
    x = str_match_all(value, "(.*?)\\\\@ref\\((.*?)\\)(.*)")
    x = unlist(x[[1]][-1])
    if(length(x)!=3) #1rd (.*) is not greedy
        abort(c("This error should not have happened, please report a bug.", 
                i="function = crosstable:::parse_reference"), x=x) 
    doc = doc %>% 
        slip_in_text(x[1], 
                     style = 'Default Paragraph Font', 
                     pos = 'after') %>% 
        slip_in_seqfield(str = glue(' REF {x[2]} \\h '),  
                         style = 'Default Paragraph Font', 
                         pos = 'after') %>%
        parse_reference(x[3])
}




