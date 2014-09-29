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
            idx <- grep("^p$", names(y))
            lidx <- alply(cbind(c(1, idx[-length(idx)]+1), idx), 1, function(x) x[1]:x[2])
            lidx[[1]] <- lidx[[1]][lidx[[1]] > 2]
            tab <- rbind(as.character(unique(y$`.id`)), cbind(variable = c(paste("    ", c(as.character(y$variable), "Test"), sep = "")), do.call("cbind", lapply(lidx, function(z) {
                tmp <- y[, z[-length(z)], FALSE]
                p <- as.character(unique(y[, z[length(z)]]))
                rbind(sapply(tmp, as.character), p)
            }))))
        }))
    } else {
        res <- do.call("rbind", dlply(x, ".id", function(y) {
            tmp <- sapply(y[, -1, FALSE], as.character)
            dim(tmp) <- dim(y[, -1, FALSE])
            dimnames(tmp) <- dimnames(y[, -1, FALSE])
            tmp[, 1] <- paste("    ", tmp[, 1], sep = "")
            rbind(as.character(unique(y$`.id`)), tmp)
        }))
    }
    rownames(res) <- NULL
    res
}

##' add a table made by the cross function into a ReporteRs document
##'
##' @param doc a \code{docx} object created by \code{docx} function (package \code{ReporteRs})
##' @param crosstable the result of \code{cross} function
##' @param compact compact the table?
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
addCrossTable <- function(doc, crosstable, compact = FALSE) {

    if (!compact) {
        ft <- FlexTable(crosstable)
        if ("p" %in% names(crosstable)) {
            ft <- spanFlexTableRows(ft, j = c(1, which(names(crosstable) == "p")), runs = crosstable$`.id`)
        } else {
            ft <- spanFlexTableRows(ft, j = 1, runs = crosstable$`.id`)
        }
        ft <- setFlexTableBorders(ft,
                                  inner.vertical =  borderProperties(width = 0), inner.horizontal = borderProperties(width = 1),
                                  outer.vertical = borderProperties(width = 0), outer.horizontal = borderProperties(width = 3))
        ft[, 1] <- textProperties(font.style = "italic")
    } else {
        crosstable2 <- compact(crosstable)
        ft <- FlexTable(data = crosstable2)
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
    doc <- addFlexTable(doc, ft)
    return(doc)
}
