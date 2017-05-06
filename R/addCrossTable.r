##' Compact the result of cross function
##'
##' @param x x
##' @author David Hajage
##' @keywords internal
##' @importFrom plyr alply
##' @importFrom plyr dlply
compact <- function(x) {
    if ("p" %in% names(x)) {
        res <- do.call("rbind", dlply(x, ".id", function(y) {
            labs <- y$label
            y$label <- NULL
            idx <- grep("^p$", names(y))
            lidx <- alply(cbind(c(1, idx[-length(idx)]+1), idx), 1, function(x) x[1]:x[2])
            lidx[[1]] <- lidx[[1]][lidx[[1]] > 2]
            tab <- rbind(as.character(unique(labs)), cbind(variable = c(paste("    ", c(as.character(y$variable), "Test"), sep = "")), do.call("cbind", lapply(lidx, function(z) {
                tmp <- y[, z[-length(z)], FALSE]
                p <- as.character(unique(y[, z[length(z)]]))
                rbind(sapply(tmp, as.character), p)
            }))))
        }))
    } else {
        res <- do.call("rbind", dlply(x, ".id", function(y) {
            tmp <- sapply(y[, -(1:2), FALSE], as.character)
            dim(tmp) <- dim(y[, -(1:2), FALSE])
            dimnames(tmp) <- dimnames(y[, -(1:2), FALSE])
            tmp[, 1] <- paste("    ", tmp[, 1], sep = "")
            rbind(as.character(unique(y$label)), tmp)
        }))
    }
    rownames(res) <- NULL
    res
}

##' Create a FlexTable object from a table made by the cross function
##'
##' @name FlexCrossTable-ReporteRs
##' @param crosstable the result of \code{cross} function
##' @param compact compact the table?
##' @param id name of the 'id' column
##' @param variable name of the 'variable' column
##' @param value name of the 'value' column
##' @param p name of the 'p' column
##' @return
##'   A \code{FlexTable} object (see \code{ReporteRs} package)
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
##' FlexCrossTable(mytable)
##' FlexCrossTable(mytable, TRUE)
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
FlexCrossTable <- function(crosstable, compact = FALSE, id = ".id", variable = "variable", value = "value", p = "p") {

    header <- names(crosstable)[names(crosstable) != "label"]
    names(header) <- header
    header[header == ".id"] <- id
    header[header == "variable"] <- variable
    header[header == "value"] <- value
    header[header == "p"] <- p

    if (!compact) {
        ft <- FlexTable(crosstable[, -1], header.columns = FALSE)
        if ("p" %in% names(crosstable)) {
            ft <- spanFlexTableRows(ft, j = c(1, which(names(crosstable) == "p")-1), runs = crosstable$`.id`)
        } else {
            ft <- spanFlexTableRows(ft, j = 1, runs = crosstable$`.id`)
        }
        if (length(attr(crosstable, "noms.col")) > 0) {
            colspan <- c(2, sapply(attr(crosstable, "n.col"), function(x) c(x, 1)))
            colspan <- colspan[-length(colspan)]
            ft <- addHeaderRow(ft, value = c(sapply(attr(crosstable, "labs.col"), function(x) c("", x))), colspan = colspan, par.properties = parProperties(text.align = "center"))
        }
        ft <- addHeaderRow(ft, value = header)
        ft <- setFlexTableBorders(ft,
                                  inner.vertical =  borderProperties(width = 0), inner.horizontal = borderProperties(width = 1),
                                  outer.vertical = borderProperties(width = 0), outer.horizontal = borderProperties(width = 3))
        ft[, 1] <- textProperties(font.style = "italic")
    } else {
        crosstable2 <- compact(crosstable)
        ft <- FlexTable(data = crosstable2, header.columns = FALSE)
        header <- header[!(names(header) %in% c(".id", "p"))]
        if (length(attr(crosstable, "noms.col")) > 0) {
            colspan <- unlist(lapply(attr(crosstable, "n.col"), function(x) c(1, x-ifelse("p" %in% names(crosstable), 1, 0))))
            ft <- addHeaderRow(ft, value = c(sapply(attr(crosstable, "labs.col"), function(x) c("", x))), colspan = colspan, par.properties = parProperties(text.align = "center"))
        }
        ft <- addHeaderRow(ft, value = header)

        ft <- setFlexTableBorders(ft,
                                  inner.vertical =  borderProperties(width = 0), inner.horizontal = borderProperties(width = 0),
                                  outer.vertical = borderProperties(width = 0), outer.horizontal = borderProperties(width = 3))

        if ("p" %in% names(crosstable)) {
            bord <- cumsum(table(crosstable$.id)+2)
            bord2 <- bord - (table(crosstable$.id)+1)
            ft[bord[-length(bord)], , side = "bottom"] = borderProperties(width = 1)
            ft[bord, , side = "top"] = borderProperties(style = "dashed")
            ft[bord2, ] <- textProperties(font.style = "italic")
            for (i in bord) {
                ft <- spanFlexTableColumns(ft, i = i, runs = crosstable2[i, ])
            }
            for (i in bord2) {
                ft <- spanFlexTableColumns(ft, i = i, runs = crosstable2[i, ])
            }
        } else {
            bord <- cumsum(table(crosstable$.id)+1)
            bord2 <- bord - (table(crosstable$.id))
            ft[bord[-length(bord)], , side = "bottom"] = borderProperties(width = 1)
            ft[bord2, ] <- textProperties(font.style = "italic")
            for (i in bord2) {
                ft <- spanFlexTableColumns(ft, i = i, runs = crosstable2[i, ])
            }
        }
    }
    return(ft)
}


##' add a table made by the cross function into a ReporteRs document
##'
##' @param doc a \code{docx} object created by \code{docx} function (see \code{ReporteRs} package)
##' @param crosstable the result of \code{cross} function
##' @param compact compact the table?
##' @param id name of the 'id' column
##' @param variable name of the 'variable' column
##' @param value name of the 'value' column
##' @param p name of the 'p' column
##' @return
##'   A \code{docx} object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(ReporteRs)
##' mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
##' doc <- docx()
##' doc <- addCrossTable(doc, mytable)
##' doc <- addPageBreak(doc)
##' doc <- addCrossTable(doc, mytable, TRUE)
##' }
##' @keywords univar
##' @export
##' @import ReporteRs
addCrossTable <- function(doc, crosstable, compact = FALSE, id = ".id", variable = "variable", value = "value", p = "p") {
    if (!inherits(crosstable, "FlexTable")) {
        ft <- FlexCrossTable(crosstable, compact, id, variable, value, p)
    } else {
        ft <- crosstable
    }
    doc <- addFlexTable(doc, ft)
    return(doc)
}

##' add_header_list
##'
##' @param x x
##' @param top top
##' @param args args
##' @author David Hajage
##' @keywords internal
##' @import flextable
##' @importFrom purrr map
add_header_list <- function(x, top = TRUE, args) {
    args_ <- map(x$col_keys, function(x) "")
    names(args_) <- x$col_keys
    args_[names(args)] <- map(args, format)
    header_data <- as.data.frame(args_, stringsAsFactors = FALSE, check.names = FALSE)
    header_ <- flextable:::add_rows(x$header, header_data, first = top)
    header_ <- flextable:::span_rows(header_, rows = seq_len(nrow(header_data)))
    x$header <- flextable:::span_columns(header_, x$col_keys)
    x
}

## Equivalent functions, but for flextable package
##' Create a flextable object from a table made by the cross function
##'
##' @name flexcrosstable-flextable
##' @param crosstable the result of \code{cross} function
##' @param compact compact the table?
##' @return
##'   A \code{flextable} object (see \code{flextable} package)
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(flextable)
##' mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
##' flexcrosstable(mytable)
##' flexcrosstable(mytable, TRUE)
##' }
##' @keywords univar
##' @export
##' @import flextable
flexcrosstable <- function(crosstable, compact = FALSE, id = ".id", variable = "variable", value = "value", p = "p") {
    crosstable$`.id` <- factor(crosstable$`.id`, unique(crosstable$`.id`), unique(crosstable$`.id`))

    if (!compact) {
        ft <- theme_vanilla(flextable(crosstable[, -1]))
        if ("p" %in% names(crosstable)) {
            ft <- merge_v(ft, j = c("label", "p"))
        } else {
            ft <- merge_v(ft, j = c("label"))
        }
        ft <- italic(ft, j = 1)
        ft <- set_header_labels(ft, .id = id, variable = variable, value = value, p = p)

        if (length(attr(crosstable, "noms.col")) > 0) {
            l <- NULL
            for (i in 1:length(attr(crosstable, "labs.col"))) {
                l <- c(l, c("", rep(attr(crosstable, "labs.col")[i], attr(crosstable, "n.col")[i])))
            }

            args <- lapply(dlply(data.frame(var = names(crosstable), label = c("", "", l), stringsAsFactors = FALSE), .(var), function(x) x[2]), function(x) x[1, 1])
            ft <- add_header_list(ft, args = args[names(args) != ".id"])
        }
        ft <- align(ft, align = "center", part = "header")
    } else {
        crosstable2 <- as.data.frame(compact(crosstable), check.names = FALSE)
        nm <- names(crosstable2)
        if (table(nm)["variable"] > 1) {
            nm[nm =="variable"] <- c("variable", paste(rep("variable", table(nm)["variable"]-1), 1:(table(nm)["variable"]-1), sep = "."))
            names(crosstable2) <- nm
        }

        ft <- theme_vanilla(flextable(crosstable2))
        ft <- border(ft, border = fp_border(width = 0))

        if ("p" %in% names(crosstable)) {
            bord <- cumsum(table(crosstable$.id)+2)
            bord2 <- bord - (table(crosstable$.id)+1)
            ft <- border(x = ft, i = bord, border.bottom = fp_border(width = 1))
            ft <- border(x = ft, i = bord, border.top = fp_border(width = 1, style = "dashed"))
            ft <- style(x = ft, i = bord, pr_p = fp_par(text.align = "left"))
            ft <- style(x = ft, i = bord2, pr_t = fp_text(italic = TRUE), pr_p = fp_par(text.align = "left"))

            for (i in bord) {
                ft <- merge_h(ft, i = i)
            }
            for (i in bord2) {
                ft <- merge_h(ft, i = i)
            }
        } else {
            bord <- cumsum(table(crosstable$.id)+1)
            bord2 <- bord - (table(crosstable$.id))
            ft <- border(x = ft, i = bord, border.bottom = fp_border(width = 1))
            ft <- style(x = ft, i = bord2, pr_t = fp_text(italic = TRUE), pr_p = fp_par(text.align = "left"))
            for (i in bord2) {
                ft <- merge_h(ft, i = i)
            }
        }
        ft <- set_header_labels(ft, variable = variable, value = value)
        if (length(attr(crosstable, "noms.col")) > 0) {
            l <- NULL
            for (i in 1:length(attr(crosstable, "labs.col"))) {
                l <- c(l, c("", rep(attr(crosstable, "labs.col")[i], attr(crosstable, "n.col")[i])))
            }

            args <- lapply(dlply(data.frame(var = names(crosstable), label = c("", "", l), stringsAsFactors = FALSE), .(var), function(x) x[2]), function(x) x[1, 1])
            ft <- add_header_list(ft, args = args[!(names(args) %in% c(".id", "label", "p"))])
        }
        ft <- align(ft, align = "center", part = "header")
    }

    # ft <- autofit(ft, 0, 0)
    # ft <- width(ft, width = 6.3*dim(ft)$widths/sum(dim(ft)$widths))

    return(ft)
}

##' add a table made by the cross function into a ReporteRs document
##'
##' @param doc a \code{docx} object created by \code{read_docx} function (package \code{flextable})
##' @param crosstable the result of \code{cross} function
##' @param compact compact the table?
##' @param width width of the table (if \code{NULL}, width of the current document is used)
##' @param pos where to add the flextable relative to the cursor, one of "after", "before", "on" (end of line) (see \code{?body_add_flextable}).
##' @return
##'   A \code{docx} object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' library(flextable)
##' mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
##' doc <- read_docx()
##' doc <- body_add_crosstable(doc, mytable)
##' doc <- body_add_break(doc)
##' doc <- body_add_crosstable(doc, mytable, TRUE)
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_crosstable <- function(doc, value, compact = FALSE, width = NULL, pos = "after") {
    if (!inherits(value, "flextable")) {
        ft <- flexcrosstable(value, compact)
    } else {
        ft <- value
    }

    if (is.null(width)) {
        width <- docx_dim(doc)$page["width"]
    }

    ft <- autofit(ft, 0, 0)
    ft <- width(ft, width = width*dim(ft)$widths/sum(dim(ft)$widths))

    doc <- body_add_flextable(doc, value = ft, pos = pos)
    return(doc)
}
