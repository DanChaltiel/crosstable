

##' Add a new paragraph with a Normal style
##'
##' @name body_add_normal
##' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
##' @param ... one or several character strings to collapse into a paragraph
##' @return a new doc object
##' @author Dan Chaltiel
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- body_add_normal(doc, "La table iris a ", ncol(iris), " colonnes.")
##' }
##' @keywords univar
##' @export
body_add_normal <- function(doc, ...) {
    value = paste0(..., collapse = "")
    body_add_par(doc, value, style = "Normal")
}



##' Add a new title
##'
##' @name body_add_title
##' @param doc the doc object (created with the \code{read_docx} function of \code{officer} package)
##' @param value a character string
##' @param level the level of the title. See \code{styles_info(doc)} to know the possibilities.
##' @return a new doc object
##' @author Dan Chaltiel
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- doc %>% 
##'    body_add_title(doc, "La table iris", 1) %>% 
##'    body_add_title(doc, "Description", 2) %>% 
##'    addNormal(doc, "La table iris a ", ncol(iris), " colonnes.")
##' }
##' @keywords univar
##' @export
body_add_title <- function(doc, value, level) {
    style = paste("heading", level)
    body_add_par(doc, value, style = style)
}

