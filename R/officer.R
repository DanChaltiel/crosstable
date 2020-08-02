

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
    body_add_par(doc, value, style = "Normal")
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
body_add_title = function(doc, value, level = 1, style = "heading") {
    style = paste(style, level)
    body_add_par(doc, value, style = style)
}


#' Add a crosstable to an `officer` document
#' @description [body_add_crosstable()] adds such a `flextable` an `officer` document.
#'
#' @param doc a \code{rdocx} object created by \code{read_docx} function (see \code{officer} package)
#' @param x a `crosstable`
#' @param ... further arguments for \code{as_flextable.crosstable}
#'
#' @export
#' @importFrom flextable body_add_flextable
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
body_add_crosstable = function (doc, x, ...) {
    ft = as_flextable(x, ...)
    doc = body_add_flextable(doc, ft)
    return(doc)
}




#' Add a table legend to an `officer` document
#'
#' @param x a docx object
#' @param legend the table legend
#' @param legend_style may depend on the docx template
#' @param style the legend style (strong, italic...)
#' @param seqfield to figure this out, in a docx file, insert a table legend, right click on the inserted number and select "Toggle Field Codes". This argument should be the value of the field, with extra escaping.
#'
#' @section Warning:
#' Sometimes, the legends added with `body_add_table_legend` have no numbers. In this case, you have to manualy update the references: select all (\kbd{Ctrl+\kbd{A}), then update (\kbd{F9}).
#' @importFrom officer body_add_par slip_in_text slip_in_seqfield
#' @export
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
body_add_table_legend = function(x, legend, legend_style="table title", style="strong", 
                                 seqfield="SEQ Table \\* Arabic"){
    x %>% 
        body_add_par(value=legend, style=legend_style) %>% 
        slip_in_text(str=": ", style=style, pos="before") %>% 
        slip_in_seqfield(str=seqfield, style=style, pos="before") %>% 
        slip_in_text(str="Table ", style=style, pos="before") %>%
        identity
}

#' @rdname body_add_table_legend
#' @export
body_add_figure_legend = function (x, legend, legend_style = "graphic title", style = "strong", 
                                   seqfield = "SEQ Figure \\* Arabic") {
    x %>% body_add_par(value = legend, style = legend_style) %>% 
        slip_in_text(str = ": ", style = style, pos = "before") %>% 
        slip_in_seqfield(str = seqfield, style = style, pos = "before") %>% 
        slip_in_text(str = "Figure ", style = style, pos = "before") %>% 
        identity
}