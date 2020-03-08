

#' Add a new paragraph with a Normal style, inserting variables with \code{base::paste}
#'
#' @name body_add_normal
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param ... one or several character strings, collapsed into a paragraph with \code{base::paste}
#' @return a new doc object
#' @author Dan Chaltiel
#' @examples
#' \dontrun{
#' library(officer)
#' library(biostat2)
#' library(dplyr)
#' doc = read_docx()
#' doc = doc %>% body_add_normal("La table iris a ", ncol(iris), " colonnes.")
#' }
#' @export
body_add_normal <- function(doc, ...) {
    value = paste0(..., collapse = "")
    body_add_par(doc, value, style = "Normal")
}

#' Add a new paragraph with a Normal style, inserting variables with \code{glue::glue}
#'
#' @name body_add_normal
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param x the string with \code{glue::glue} patterns (Expressions enclosed by braces will be evaluated as R code)
#' @param ... parameters to be passed to \code{glue::glue}
#' @return a new doc object
#' @author Dan Chaltiel
#' @examples
#' \dontrun{
#' library(officer)
#' library(biostat2)
#' library(dplyr)
#' doc = read_docx()
#' doc = body_add_glued(doc, "La table iris a {ncol(iris)} colonnes.")
#' }
#' @export
body_add_glued <- function(doc, x, ...) {
    value = glue::glue(x, ...,.envir = parent.frame())
    body_add_par(doc, value, style = "Normal")
    # value
}


#' Add a new title
#'
#' @name body_add_title
#' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
#' @param value a character string
#' @param level the level of the title. See \code{styles_info(doc)} to know the possibilities.
#' @return a new doc object
#' @author Dan Chaltiel
#' @examples
#' \dontrun{
#' library(officer)
#' library(biostat2)
#' library(dplyr)
#' doc = read_docx()
#' doc = doc %>% 
#'    body_add_title(doc, "La table iris", 1) %>% 
#'    body_add_title(doc, "Description", 2) %>% 
#'    addNormal(doc, "La table iris a ", ncol(iris), " colonnes.")
#' }
#' @export
body_add_title <- function(x, value, level = 1, style = "heading") {
    style <- paste(style, level)
    body_add_par(x, value, style = style)
}


#TODO: extends all https://github.com/eusebe/biostat2/blob/master/R/officer.r
