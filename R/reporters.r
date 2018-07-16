##' Add a new paragraph with a Normal style
##'
##' @name addNormal
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param stylename a character string 
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addNormal(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addNormal <- function(doc, value, stylename = "Normal", ...) {
    if (class(value) == "character") {
        value <- do.call("set_of_paragraphs", args = lapply(value, pot))
    }
    addParagraph(doc, value, stylename = stylename)
}

##' Add a new paragraph with a Comment style
##'
##' @name addComment
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param stylename a character string 
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addComment(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addComment <- function(doc, value, stylename = "Comment", ...) {
    addParagraph(doc, value, stylename = stylename)
}

##' Add a new paragraph with a Alert style
##'
##' @name addAlert
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param stylename a character string 
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addAlert(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addAlert <- function(doc, value, stylename = "Alert", ...) {
    addParagraph(doc, value, stylename = stylename)
}

##' Add a new paragraph with a Verbatim style
##'
##' @name addVerbatim
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param stylename a character string 
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addVerbatim(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addVerbatim <- function(doc, value, stylename = "Verbatim", ...) {
    addParagraph(doc, value, stylename = stylename, ...)
}

##' Add a new paragraph with a Code style
##'
##' @name addCode
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addCode(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addCode <- function(doc, value, ...) {
    addRScript(doc, text = value, par.properties = parProperties(shading.color = "#eeeeee"))
}

##' Add a new paragraph with a Plotlegend style
##'
##' @name addPlotlegend
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param stylename a character string 
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addPlotlegend(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addPlotlegend <- function(doc, value, stylename = "figurereference", ...) {
    addParagraph(doc, value, stylename = stylename)
}

##' Add a new paragraph with a Tablegend style
##'
##' @name addTablegend
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a character string
##' @param stylename a character string 
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addTablegend(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addTablegend <- function(doc, value, stylename = "tablereference", ...) {
    addParagraph(doc, value, stylename = stylename)
}

##' Add a list of items
##'
##' @name addItemize
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a vector of character strings
##' @param level a integer (1 to 4)
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addItemize(doc, c("Coucou", "Cuicui"))
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addItemize <- function(doc, value, level = 1, ...) {
    unordered.list.level1 <- parProperties(list.style = "unordered", level = 1)
    unordered.list.level2 <- parProperties(list.style = "unordered", level = 2)
    unordered.list.level3 <- parProperties(list.style = "unordered", level = 3)
    unordered.list.level4 <- parProperties(list.style = "unordered", level = 4)
    
    if (level == 1) {
        style <- unordered.list.level1
    } else if (level == 2) {
        style <- unordered.list.level2
    } else if (level == 3) {
        style <- unordered.list.level3
    } else if (level == 4) {
        style <- unordered.list.level4
    }
    addParagraph(doc, value, par.properties = style)
}

##' Add a list of numerated items
##'
##' @name addEnumerate
##' @param doc the doc object (created with the docx function of ReporteRs package)
##' @param value a vector of character strings
##' @param level a integer (1 to 4)
##' @param ... ...
##' @return
##'   a new doc object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- addEnumerate(doc, c("Coucou", "Cuicui"))
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addEnumerate <- function(doc, value, level = 1, ...) {
    ordered.list.level1 <- parProperties(list.style = "ordered", level = 1)
    ordered.list.level2 <- parProperties(list.style = "ordered", level = 2)
    ordered.list.level3 <- parProperties(list.style = "ordered", level = 3)
    ordered.list.level4 <- parProperties(list.style = "ordered", level = 4)
    
    if (level == 1) {
        style <- ordered.list.level1
    } else if (level == 2) {
        style <- ordered.list.level2
    } else if (level == 3) {
        style <- ordered.list.level3
    } else if (level == 4) {
        style <- ordered.list.level4
    }
    addParagraph(doc, value, par.properties = style)
}

##' Create a FlexTable object with a very simple style
##'
##' @name simple.table
##' @param dataset a data.frame
##' @param add.rownames a logical
##' @return
##'   a FlexTable object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- simple.table(head(iris))
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
simple.table <- function(dataset, add.rownames = FALSE) {
    ft = FlexTable(dataset, add.rownames = add.rownames)
    ft[, , to = "header"] = textBold()
    #    ft[, , to = "header"] = parRight()
    ft[, , to = "body"] = textNormal()
    #    ft[, , to = "body"] = parRight()
    ft = setFlexTableBorders(ft, inner.vertical = borderProperties(width = 0),
                             inner.horizontal = borderProperties(width = 0), outer.vertical = borderProperties(width = 0),
                             outer.horizontal = borderProperties(width = 2))
    ft
}



##' Create a docx object with a specific template and informations already inserted
##'
##' @name create.report
##' @param template either 'gerc', 'urc', or 'cephepi'
##' @param title Study title (character string)
##' @param acronym Study acronym (character string)
##' @param version Version number (character string)
##' @param npromo Promotion number (character string, not used with gerc template)
##' @param nct Clinical trial number (character string, not used with gerc template)
##' @param invest Name of the principal investigator (character string)
##' @param biostat Name of the biostatistician (character string)
##' @param methodo Name of the methodologist (character string, not used with gerc template)
##' @param date_lastmodif Date of last modification of the document (character string)
##' @param date_freez Date of data freezing (character string)
##' @param date_update Date of last history description update  (character string)
##' @param history History description. A list of length 1 (min) to 7 (max), each element being a list of four elements: list(version = "Version number", author = "Author", description = c("Description 1", "Description 2"), date = "Date")
##' @return
##'   a FlexTable object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' doc <- create.report(template = 'urc', 
##'                      title = 'A great study', 
##'                      acronym = 'GREAT', 
##'                      version = '1.3', 
##'                      npromo = 'AOR17123', 
##'                      nct = 'NCT123456', 
##'                      invest = 'Casimir', 
##'                      biostat = 'Goldorak', 
##'                      methodo = 'Actarus', 
##'                      date_lastmodif = '23/03/1979', 
##'                      date_freez = '22/03/1979', 
##'                      date_update = '24/03/1979', 
##'                      history = list(
##'                                     list(version = '1', 
##'                                          author = 'Goldorak', 
##'                                          description = c('First description', 'Primary outcome analysis'), 
##'                                          date = '28/03/1979'), 
##'                                     list(version = '1.3', 
##'                                          author = 'Goldorak', 
##'                                          description = 'Add forgotten things', 
##'                                          date = '29/03/1979')
##'                                    )
##'                     )
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
create.report <- function(template = c("gerc", "urc", "cephepi"), title = "", acronym = "", version = "", npromo = "", nct = "", invest = "", biostat = "", methodo = "", date_lastmodif = "", date_freez = "", date_update = "", history = NULL) {
    
    URC <- FALSE
    if (template[1] == "urc") {
        template.file <- system.file("templates/template_urc.docx", package = "biostat2")
        URC <- TRUE
    } else if (template[1] == "cephepi") {
        template.file <- system.file("templates/template_cephepi.docx", package = "biostat2")
        URC <- TRUE
    } else if (template[1] == "gerc") {
        template.file <- system.file("templates/template_gerc.docx", package = "biostat2")
    }
    doc <- docx(template = template.file)
    
    doc <- addParagraph(doc
                        , value = paste("VERSION ", version, " du ", date_lastmodif, sep = "")
                        , stylename = "Textpage1"
                        , bookmark = "VERSION_DATE"
    )
    
    doc <- addParagraph(doc
                        , value = acronym
                        , stylename = "Bigpage1"
                        , bookmark = "ACRONYME"
    )
    
    doc <- addParagraph(doc
                        , value = title
                        , stylename = "Smallpage1"
                        , bookmark = "TITRE"
    )
    
    if (URC) {
        doc <- addParagraph(doc
                            , value = npromo
                            , stylename = "Subtextpage1"
                            , bookmark = "NPROMO"
        )
        
        
        doc <- addParagraph(doc
                            , value = nct
                            , stylename = "Subtextpage1"
                            , bookmark = "NCT"
        )
    }
    doc <- addParagraph(doc
                        , value = invest
                        , stylename = "Subtextpage1"
                        , bookmark = "NOM_INVESTIGATEUR"
    )
    
    doc <- addParagraph(doc
                        , value = biostat
                        , stylename = "Subtexttabpage1"
                        , bookmark = "NOM_BIOSTAT"
    )
    
    if (URC) {
        doc <- addParagraph(doc
                            , value = methodo
                            , stylename = "Subtexttabpage1"
                            , bookmark = "NOM_RESP"
        )
    }
    
    doc <- addParagraph(doc
                        , value = acronym
                        , stylename = "Entetegauche"
                        , bookmark = "ENTETE_ACRONYME"
    )
    
    doc <- addParagraph(doc
                        , value = date_lastmodif
                        , stylename = "Entetemilieu"
                        , bookmark = "ENTETE_DATE"
    )
    
    doc <- addParagraph(doc
                        , value = biostat
                        , stylename = "Entetedroite"
                        , bookmark = "ENTETE_BIOSTAT"
    )
    
    doc <- addParagraph(doc
                        , value = date_freez
                        , stylename = "Normal"
                        , bookmark = "DATE_GEL"
    )
    
    doc <- addParagraph(doc
                        , value = date_update
                        , stylename = "Normal"
                        , bookmark = "DATE_MAJ"
    )
    
    
    if (!is.null(history)) {
        for (i in 1:length(history)) {
            ver <- history[[i]]
            doc <- addParagraph(doc
                                , value = ver$version
                                , stylename = "Normal"
                                , bookmark = paste("V0", i, sep = "")
            )
            
            doc <- addParagraph(doc
                                , value = ver$author
                                , stylename = "Normal"
                                , bookmark = paste("V0", i, "_AUTEUR", sep = "")
            )
            
            doc <- addParagraph(doc
                                , value = ver$description
                                , stylename = "Normal"
                                , bookmark = paste("V0", i, "_DESC", sep = "")
            )
            
            doc <- addParagraph(doc
                                , value = ver$date
                                , stylename = "Normal"
                                , bookmark = paste("V0", i, "_DATE", sep = "")
            )
            
        }
    }
    return(doc)
}
