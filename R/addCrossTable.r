##' Compact the result of cross function
##'
##' @param x x
##' @author David Hajage
##' @keywords internal
##' @importFrom plyr alply
##' @importFrom plyr dlply
compact <- function(x) {
    ordre <- unique(x$.id)
    if ("p" %in% names(x) | "effect" %in% names(x)) {
        rajout <- NULL
        if ("p" %in% names(x))
            rajout <- c(rajout, "Test")
        if ("effect" %in% names(x))
            rajout <- c(rajout, "Effect")
        
        # attributes(attr(x, "n.col")) <- NULL
        res <- do.call("rbind", dlply(x, ".id", function(y) {
            labs <- y$label
            y$label <- NULL
            idx.p <- grep("^p$", names(y))
            pval <- y[, idx.p, FALSE]
            y[, idx.p] <- NULL
            idx.e <- grep("^effect$", names(y))
            eval <- y[, idx.e, FALSE]
            y[, idx.e] <- NULL
            
            # n.col <- attr(x, "n.col")
            # n.col <- ncol(y) - 1
            nb <- sum(unique(names(x) == "p")) + sum(unique(names(x)) == "effect")
            n.col <- attr(x, "n.col") - nb + 1
            
            lidx <- lapply(c(1:(length(n.col))), function(i) {(c(1, cumsum(n.col))[i]+i-1):c(1, cumsum(n.col))[i+1]}+1)
            
            # tab <- rbind(as.character(unique(labs)), cbind(variable = c(paste("    ", c(as.character(y$variable), "Test"), sep = "")), do.call("cbind", lapply(1:length(lidx), function(i) {
            #     z <- lidx[[i]][-1]
            #     tmp <- y[, z, FALSE]
            #     p <- as.character(unique(pval[, i]))
            #     rbind(sapply(tmp, as.character), p)
            # }))))
            # 
            
            tab <- rbind(as.character(unique(labs)), do.call("cbind", lapply(1:length(lidx), function(i) {
                z <- lidx[[i]][-1]
                tmp <- y[, z, FALSE]
                p <- e <- NULL
                if (ncol(pval) >= 1) {
                    p <- as.character(unique(pval[, i]))
                }
                if (ncol(eval) >=1) {
                    e <- as.character(unique(eval[, i]))
                }
                
                cbind(variable = c(paste("    ", c(as.character(y$variable), rajout), sep = "")), rbind(sapply(tmp, as.character), p, e))
            })))
            
        })[ordre])
        
    } else {
        x[, grep("variable", names(x))] <- paste.matrix("    ", x[, grep("variable", names(x))], sep = "")
        res <- do.call("rbind", dlply(x, ".id", function(y) {
            tmp <- sapply(y[, -(1:2), FALSE], as.character)
            dim(tmp) <- dim(y[, -(1:2), FALSE])
            dimnames(tmp) <- dimnames(y[, -(1:2), FALSE])
            # tmp[, 1] <- paste("    ", tmp[, 1], sep = "")
            rbind(as.character(unique(y$label)), tmp)
        })[ordre])
    }
    rownames(res) <- NULL
    
    ## Correction finale si plusieurs variables en colonne...
    if (sum(colnames(res) == "variable") > 1 & "p" %in% names(x)) {
        res[res[, 1] == "    Test", which(colnames(res) == "variable")] <- "    Test"
    }
    if (sum(colnames(res) == "variable") > 1 & "effect" %in% names(x)) {
        res[res[, 1] == "    Effect", which(colnames(res) == "variable")] <- "    Effect"
    }
    
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
##' @param effect name of the 'effect' column
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
FlexCrossTable <- function(crosstable, compact = FALSE, id = ".id", variable = "variable", value = "value", effect = "effect", p = "p") {

    header <- names(crosstable)[names(crosstable) != "label"]
    names(header) <- header
    header[header == ".id"] <- id
    header[header == "variable"] <- variable
    header[header == "value"] <- value
    header[header == "effect"] <- effect
    header[header == "p"] <- p
    
    if (!compact) {
        ft <- FlexTable(crosstable[, -1], header.columns = FALSE)
        ft <- spanFlexTableRows(ft, j = 1, runs = crosstable$`.id`)
        if ("p" %in% names(crosstable)) {
            ft <- spanFlexTableRows(ft, j = c(1, which(names(crosstable) == "p")-1), runs = crosstable$`.id`)
        }
         if ("effect" %in% names(crosstable)) {
            ft <- spanFlexTableRows(ft, j = c(1, which(names(crosstable) == "effect")-1), runs = crosstable$`.id`)
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
        header <- header[!(names(header) %in% c(".id", "effect", "p"))]
        if (length(attr(crosstable, "noms.col")) > 0) {
            colspan <- unlist(lapply(attr(crosstable, "n.col"), function(x) c(1, x-ifelse("p" %in% names(crosstable), 1, 0)-ifelse("effect" %in% names(crosstable), 1, 0))))
            ft <- addHeaderRow(ft, value = c(sapply(attr(crosstable, "labs.col"), function(x) c("", x))), colspan = colspan, par.properties = parProperties(text.align = "center"))
        }
        ft <- addHeaderRow(ft, value = header)

        ft <- setFlexTableBorders(ft,
                                  inner.vertical =  borderProperties(width = 0), inner.horizontal = borderProperties(width = 0),
                                  outer.vertical = borderProperties(width = 0), outer.horizontal = borderProperties(width = 3))

        if ("p" %in% names(crosstable) | "effect" %in% names(crosstable)) {
            if ("effect" %in% names(crosstable)) {
                bord <- grep("    Effect", crosstable2[, "variable"])
            } else {
                bord <- grep("    Test", crosstable2[, "variable"])
            }
            nb <- sum(unique(names(crosstable) == "p")) + sum(unique(names(crosstable)) == "effect")
            bord2 <- cumsum(table(factor(crosstable$.id, unique(crosstable$.id)))+nb+1) - (table(factor(crosstable$.id, unique(crosstable$.id)))+nb)
            
            bord3 <- grep("    Effect|    Test", crosstable2[, "variable"])
                
            ft[bord[-length(bord)], , side = "bottom"] = borderProperties(width = 1)
            ft[bord- (nb-1), , side = "top"] = borderProperties(style = "dashed")
            ft[bord2, ] <- textProperties(font.style = "italic")
            # runs <- unlist(lapply(1:length(attr(crosstable, "noms.col")), function(x) rep(attr(crosstable, "noms.col")[x], attr(crosstable, "n.col")[x])))
            # runsp <- c("xeaxj", unlist(lapply(1:length(attr(crosstable, "noms.col")), function(x) rep(attr(crosstable, "noms.col")[x], attr(crosstable, "n.col")[x])))[-1])
            for (i in bord3) {
                ft <- spanFlexTableColumns(ft, i = i, runs = crosstable2[i, ])
                # ft <- spanFlexTableColumns(ft, i = i, runs = runsp)
            }
            for (i in bord2) {
                ft <- spanFlexTableColumns(ft, i = i, runs = crosstable2[i, ])
                # ft <- spanFlexTableColumns(ft, i = i, runs = runs)
            }
        } else {
            bord <- cumsum(table(factor(crosstable$.id, unique(crosstable$.id)))+1)
            bord2 <- bord - (table(factor(crosstable$.id, unique(crosstable$.id))))
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
# 
# ##' add_header_list
# ##'
# ##' @param x x
# ##' @param top top
# ##' @param args args
# ##' @author David Hajage
# ##' @keywords internal
# ##' @importFrom flextable flextable
# ##' @importFrom purrr map
# add_header_list <- function(x, top = TRUE, args) {
#     args_ <- map(x$col_keys, function(x) "")
#     names(args_) <- x$col_keys
#     args_[names(args)] <- map(args, format)
#     header_data <- as.data.frame(args_, stringsAsFactors = FALSE, check.names = FALSE)
#     header_ <- flextable:::add_rows.complex_tabpart(x$header, header_data, first = TRUE)
#     header_ <- flextable:::span_rows(header_, rows = seq_len(nrow(header_data)))
#     x$header <- flextable:::span_columns(header_, x$col_keys)
#     x
# }
# 
# merge_at_custom <- function(ft, j, runs) {
#     for (i in 1:length(runs)) {
#         ft <- merge_at(ft, i = runs[[i]], j = j)
#     }
#     ft
# }
# 
# set_header_labels_list <- function (x, args) {
#     # args <- list(...)
#     if (nrow(x$header$dataset) < 1) 
#         stop("there is no header row to be replaced")
#     header_ <- x$header$dataset
#     values <- as.list(tail(x$header$dataset, n = 1))
#     args <- args[is.element(names(args), x$col_keys)]
#     values[names(args)] <- args
#     x$header$dataset <- rbind(header_[-nrow(header_), ], as.data.frame(values, 
#                                                                        stringsAsFactors = FALSE, check.names = FALSE))
#     x
# }
# 
# ## Equivalent functions, but for flextable package
# ##' Create a flextable object from a table made by the cross function
# ##'
# ##' @name flexcrosstable-flextable
# ##' @param crosstable the result of \code{cross} function
# ##' @param compact compact the table?
# ##' @return
# ##'   A \code{flextable} object (see \code{flextable} package)
# ##' @author David Hajage
# ##' @examples
# ##' \dontrun{
# ##' library(biostat2)
# ##' library(flextable)
# ##' mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
# ##' flexcrosstable(mytable)
# ##' flexcrosstable(mytable, TRUE)
# ##' }
# ##' @keywords univar
# ##' @export
# ##' @importFrom flextable flextable
# flexcrosstable <- function(crosstable, compact = FALSE, id = ".id", variable = "variable", value = "value", p = "p") {
#     crosstable$`.id` <- factor(crosstable$`.id`, unique(crosstable$`.id`), unique(crosstable$`.id`))
# 
#     
#     if (!compact) {
# 
#         nm <- names(crosstable)
#         nm[nm == ".id"] <- id
#         nm[nm == "variable"] <- variable
#         nm[nm == "value"] <- value
#         nm[nm == "p"] <- p
#         
#         names(crosstable) <- make.names(names(crosstable), unique = TRUE)
#         
#         noms <- nm
#         names(noms) <- names(crosstable)
#         
#         ft <- theme_vanilla(flextable(crosstable[, -1]))
#         length.cum <- cumsum(rle(as.character(crosstable$`.id`))$lengths)
#         runs <- mapply(function(x, y) {x:y}, c(1, length.cum[-length(length.cum)]+1), length.cum, SIMPLIFY = FALSE)
#         
#         ft <- merge_at_custom(ft, 1, runs)
#         
#         if ("p" %in% names(crosstable)) {
#             pi <- grep(p, nm)-1
#             for (i in pi) {
#                 ft <- merge_at_custom(ft, i, runs)
#             }
#         }
#         ft <- italic(ft, j = 1)
#         ft <- set_header_labels_list(ft, noms)
# 
#         if (length(attr(crosstable, "noms.col")) > 0) {
#             l <- NULL
#             for (i in 1:length(attr(crosstable, "labs.col"))) {
#                 l <- c(l, c("", rep(attr(crosstable, "labs.col")[i], attr(crosstable, "n.col")[i])))
#             }
# 
#             args <- lapply(dlply(data.frame(var = names(crosstable), label = c("", "", l), stringsAsFactors = FALSE), .(var), function(x) x[2]), function(x) x[1, 1])
#             ft <- add_header_list(ft, args = args[names(args) != ".id"])
#         }
#         ft <- align(ft, align = "center", part = "header")
#     } else {
#         crosstable2 <- as.data.frame(compact(crosstable), check.names = FALSE)
#         
#         nm <- names(crosstable2)
#         nm[nm == ".id"] <- id
#         nm[nm == "variable"] <- variable
#         nm[nm == "value"] <- value
#         nm[nm == "p"] <- p
#         
#         names(crosstable) <- make.names(names(crosstable), unique = TRUE)
#         names(crosstable2) <- make.names(names(crosstable2), unique = TRUE)
#         
#         noms <- nm
#         names(noms) <- names(crosstable2)
# 
#         ft <- theme_vanilla(flextable(crosstable2))
#         ft <- border(ft, border = fp_border(width = 0))
# 
#         if ("p" %in% names(crosstable)) {
#             bord <- cumsum(table(crosstable$.id)+2)
#             bord2 <- bord - (table(crosstable$.id)+1)
#             ft <- border(x = ft, i = bord, border.bottom = fp_border(width = 1))
#             ft <- border(x = ft, i = bord, border.top = fp_border(width = 1, style = "dashed"))
#             ft <- style(x = ft, i = bord, pr_p = fp_par(text.align = "left"))
#             ft <- style(x = ft, i = bord2, pr_t = fp_text(italic = TRUE), pr_p = fp_par(text.align = "left"))
# 
#             for (i in bord) {
#                 ft <- merge_h(ft, i = i)
#             }
#             for (i in bord2) {
#                 ft <- merge_h(ft, i = i)
#             }
#         } else {
#             bord <- cumsum(table(crosstable$.id)+1)
#             bord2 <- bord - (table(crosstable$.id))
#             ft <- border(x = ft, i = bord, border.bottom = fp_border(width = 1))
#             ft <- style(x = ft, i = bord2, pr_t = fp_text(italic = TRUE), pr_p = fp_par(text.align = "left"))
#             for (i in bord2) {
#                 ft <- merge_h(ft, i = i)
#             }
#         }
# 
#         ft <- set_header_labels_list(ft, noms[!(noms %in% c(".id", "label", "p"))])
#         if (length(attr(crosstable, "noms.col")) > 0) {
#             l <- NULL
#             for (i in 1:length(attr(crosstable, "labs.col"))) {
#                 l <- c(l, c("", rep(attr(crosstable, "labs.col")[i], attr(crosstable, "n.col")[i]-("p" %in% names(crosstable)))))
#             }
# 
#             args <- lapply(dlply(data.frame(var = names(crosstable2), label = c(l), stringsAsFactors = FALSE), .(var), function(x) x[2]), function(x) x[1, 1])
#             ft <- add_header_list(ft, args = args[!(names(args) %in% c(".id", "label", "p"))])
#         }
#         ft <- align(ft, align = "center", part = "header")
#     }
# 
#     # ft <- autofit(ft, 0, 0)
#     # ft <- width(ft, width = 6.3*dim(ft)$widths/sum(dim(ft)$widths))
# 
#     return(ft)
# }
# 
# ##' add a table made by the cross function into a ReporteRs document
# ##'
# ##' @param doc a \code{docx} object created by \code{read_docx} function (package \code{flextable})
# ##' @param crosstable the result of \code{cross} function
# ##' @param compact compact the table?
# ##' @param width width of the table (if \code{NULL}, width of the current document is used)
# ##' @param pos where to add the flextable relative to the cursor, one of "after", "before", "on" (end of line) (see \code{?body_add_flextable}).
# ##' @return
# ##'   A \code{docx} object
# ##' @author David Hajage
# ##' @examples
# ##' \dontrun{
# ##' library(biostat2)
# ##' library(officer)
# ##' library(flextable)
# ##' mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
# ##' doc <- read_docx()
# ##' doc <- body_add_crosstable(doc, mytable)
# ##' doc <- body_add_break(doc)
# ##' doc <- body_add_crosstable(doc, mytable, TRUE)
# ##' }
# ##' @keywords univar
# ##' @export
# ##' @importFrom flextable flextable
# ##' @importFrom officer autofit
# ##' @importFrom officer width
# ##' @importFrom officer docx_dim
# body_add_crosstable <- function(doc, value, compact = FALSE, w = NULL, pos = "after") {
#     if (!inherits(value, "flextable")) {
#         ft <- flexcrosstable(value, compact)
#     } else {
#         ft <- value
#     }
# 
#     if (is.null(w)) {
#         tmp <- docx_dim(doc)
#         w <- tmp$page["width"] - sum(tmp$margins[c("left", "right")])
#     }
# 
#     # ft <- autofit(ft, 0, 0)
#     # ft <- width(ft, width = w)
#     ft <- width(ft, width = w*dim(ft)$widths/sum(dim(ft)$widths))
# 
#     doc <- body_add_flextable(doc, value = ft, pos = pos)
#     return(doc)
# }
