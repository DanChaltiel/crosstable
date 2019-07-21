##' Compute a contingency table
##'
##' @param x x
##' @param y y
##' @param showNA showNA
##' @param margin margin
##' @param total total
##' @param digits digits
##' @param test test
##' @param test.tabular test.tabular
##' @param show.test show.test
##' @param plim plim
##' @param effect effect
##' @param effect.tabular effect.tabular 
##' @param conf.level conf.level
##' @param show.effect show.effect
##' @param show.method show.method
##'
##' @author David Hajage
##' @keywords internal
tabular <- function(x, y, showNA = c("no", "ifany", "always"), margin = 0:2, total = FALSE, digits = 2, test = FALSE, test.tabular = test.tabular.auto, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.tabular = or.row.by.col, conf.level = 0.95, show.effect = display.effect) {

    nn <- table(x, y)
    n <- table(x, y, useNA = showNA)

    if (is.logical(total)) {
        if (total) {
            addmargins <- 1:2
        } else {
            addmargins <- 0
        }
    } else {
        addmargins <- total
    }

    if (all(addmargins != 0)) {
        ## margin <- addmargins
        ## totmargin <- ifelse(margin == 1, 2, margin)
        ## totmargin <- ifelse(margin == 2, 1, totmargin)

        ## if (length(totmargin) != 0) {
        ##     n <- addmargins(n, margin = totmargin, FUN = list(Total = sum), quiet = TRUE)
        ## }
        if (1 %in% addmargins) {
            totr <- margin.table(nn, 1)
            totr2 <- totr[1:nrow(n)]
            if (length(totr) != length(totr2))
                totr2[length(totr2)] <- sum(n[nrow(n), ])
        }
        if (2 %in% addmargins) {
            totc <- margin.table(nn, 2)
            totc2 <- totc[1:ncol(n)]
            if (length(totc) != length(totc2))
                totc2[length(totc2)] <- sum(n[, ncol(n)])
        }

        if (1 %in% addmargins & 2 %in% addmargins) {
            n <- cbind(n, Total = totr2)
            n <- rbind(n, Total = c(totc2, sum(totr)))
        } else if (1 %in% addmargins) {
            n <- cbind(n, Total = totr2)
        } else if (2 %in% addmargins) {
            n <- rbind(n, Total = totc2)
        }
    }

    ## %
    props <- lapply(margin, function(margin) {
        if (margin != 0) {
            prop <- sweep(nn, margin, margin.table(nn, margin), "/", check.margin = FALSE)
        } else {
            prop <- nn/sum(nn)
        }
        prop
    })
    names(props) <- sapply(as.character(margin), function(x) switch(x, "0" = "cell", "1" = "row", "2" = "col"))

    pourcs <- lapply(props, function(x) paste.matrix(round(x*100, digits), "%", sep = ""))
    p <- paste.matrix("(", do.call("paste.matrix", c(pourcs, sep  = " / ")), ")", sep = "")
    rownames(p) <- rownames(nn)
    colnames(p) <- colnames(nn)

    if (all(addmargins != 0)) {
        if (1 %in% addmargins) {
            totr <- paste("(", round(100*margin.table(nn, 1)/sum(margin.table(nn, 1)), digits), "%)", sep = "")
            p <- cbind(p, Total = totr)
        }
        if (2 %in% addmargins) {
            totc <- paste("(", round(100*margin.table(nn, 2)/sum(margin.table(nn, 2)), digits), "%)", sep = "")
            if (1 %in% addmargins) {
                totc <- c(totc, "(100%)")
            }
            p <- rbind(p, Total = totc)
        }
    }

    if (anyNA(rownames(n)) & !anyNA(rownames(p))) {
        if ("Total" %in% rownames(p)) {
            p <- rbind(p[-nrow(p), ], rep("", ncol(p)), p[nrow(p), ])
        } else {
            p <- rbind(p, rep("", ncol(p)))
        }
    }
    if (anyNA(colnames(n)) & !anyNA(colnames(p))) {
        if ("Total" %in% colnames(p)) {
            p <- cbind(p[, -ncol(p)], rep("", nrow(p)), p[, ncol(p)])
        } else {
            p <- cbind(p, rep("", nrow(p)))
        }
    }

    results <- trim(paste.matrix(n, p))
    rownames(results) <- rownames(n)
    colnames(results) <- colnames(n)

    if (effect) {
        results <- cbind(results, effect = show.effect(effect.tabular(x, y, conf.level), digits = digits))
    }

    if (test) {
        results <- cbind(results, p = show.test(test.tabular(x, y), digits = plim, method = show.method))
    }

    
    rn <- rownames(results)
    rn[is.na(rn)] <- "NA"
    results <- cbind(variable = rn, results)

    nom <- colnames(results)
    nom[is.na(nom)] <- "NA"
    colnames(results) <- nom

    ## Si NA n'est pas dans le facteur, on met la colonne apres "Total"
    if ((any(colnames(results) == "NA") & any(colnames(results) %in% c("Total", "p"))) & !anyNA(levels(y))) {
        tmp <- results[, "NA"]
        results <- cbind(results[, colnames(results) != "NA"], "NA" = tmp)
    }
    ## idem pour les lignes
    if ((any(results[, "variable"] == "NA") & any(results[, "variable"] == "Total")) & !anyNA(levels(x))) {
        tmp <- results[results[, "variable"] == "NA", ]
        results <- rbind(results[results[, "variable"] != "NA", ], tmp)
    }
    rownames(results) <- NULL

    results
}

##' Compute a contingency table (data.frame input)
##'
##' @importFrom Hmisc label
##' @param dfx data.frame
##' @param dfy data.frame
##' @param margin margin
##' @param showNA showNA
##' @param total total
##' @param digits digits
##' @param test test
##' @param test.tabular test.tabular
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @param label label
##' @author David Hajage
##' @keywords internal
##' @importFrom plyr mapvalues
tabular.data.frame <- function(dfx, dfy, margin = 0:2, showNA = c("no", "ifany", "always"), total = FALSE, digits = 2, test = FALSE, test.tabular = test.tabular.auto, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.tabular = or.row.by.col, conf.level = 0.95, show.effect = display.effect, label = FALSE) {

    noms.dfx <- names(dfx)
    noms.dfy <- names(dfy)

    if (label) {
        labs.dfx <- sapply(dfx, label)
        labs.dfx[labs.dfx == ""] <- noms.dfx[labs.dfx == ""]
        # names(dfx) <- noms.dfx
        labs.dfy <- sapply(dfy, label)
        labs.dfy[labs.dfy == ""] <- noms.dfy[labs.dfy == ""]
        # names(dfy) <- noms.dfy
    } else {
        labs.dfx <- noms.dfx
        labs.dfy <- noms.dfy
    }


    results <- llply(dfy, function(y) llply(dfx, function(x) tabular(x, y, margin = margin, showNA = showNA, total = total, digits = digits, test = test, test.tabular = test.tabular, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.tabular = effect.tabular, conf.level = conf.level, show.effect = show.effect)))

    results <- lapply(results, function(x) {
        noms <- names(x)
        for (i in 1:length(x)) {
            x[[i]] <- cbind(".id" = noms[i], x[[i]])
        }
        x
    })

    n.dfx <- sapply(results[[1]], nrow)

    results <- lapply(results, rbind.list)

    if (length(results) > 1) {
        n.dfy <- laply(results, ncol) - 2
        results <- cbind(results[[1]], cbind.list(lapply(results[-1], function(x) x[, -(1:2)])))
    } else {
        n.dfy <- ncol(results[[1]]) - 2
        results <- results[[1]]
    }

    results <- data.frame(results, check.names = FALSE)
    results$label <- mapvalues(results$`.id`, from = noms.dfx, to = labs.dfx)
    results <- results[, c(".id", "label", names(results)[!(names(results) %in% c(".id", "label"))])]

    attr(results, "noms.lig") <- noms.dfx
    attr(results, "noms.col") <- noms.dfy
    attr(results, "labs.lig") <- labs.dfx
    attr(results, "labs.col") <- labs.dfy
    attr(results, "n.lig") <- n.dfx
    attr(results, "n.col") <- n.dfy

    results
}

## ##' Ascii for tabular object.
## ##'
## ##' Ascii method for tabular object (internal).
## ##'
## ##' @export
## ##' @method ascii tabular
## ##' @import ascii
## ##' @param x a tabular object
## ##' @param format see \code{?ascii} in \code{ascii} package
## ##' @param digits see \code{?ascii} in \code{ascii} package
## ##' @param include.rownames see \code{?ascii} in \code{ascii} package
## ##' @param include.colnames see \code{?ascii} in \code{ascii} package
## ##' @param header see \code{?ascii} in \code{ascii} package
## ##' @param rstyle see \code{?ascii} in \code{ascii} package
## ##' @param caption see \code{?ascii} in \code{ascii} package
## ##' @param caption.level see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords univar
## ascii.tabular <- function(x, format = "nice", digits = 5, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, rstyle = "d", caption = NULL, caption.level = NULL, ...) {
##   do.call(cbind.ascii, c(lapply(x, function(x) {
##     ascii(x, format = format, digits = digits, include.rownames = include.rownames, include.colnames = include.colnames, header = header, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), tgroup = attr(x, "tgroup"), n.tgroup = attr(x, "n.tgroup"), rgroup = attr(x, "rgroup"), n.rgroup = attr(x, "n.rgroup"), rstyle = rstyle, ...)}), caption = caption, caption.level = caption.level))
## }

## ##' Print tabular object.
## ##'
## ##' Print tabular object (internal).
## ##'
## ##' @export
## ##' @method print tabular
## ##' @import ascii
## ##' @param x a tabular object
## ##' @param type type of output (see \code{?ascii} in \code{ascii}
## ##' package)
## ##' @param lstyle see \code{?ascii} in \code{ascii} package
## ##' @param tstyle see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords univar
## print.tabular <- function(x, type = "rest", lstyle = "", tstyle = "", ...) {
##   print(ascii.tabular(x, lstyle = lstyle, tstyle = tstyle, ...), type = type)
##   ## invisible(x)
## }

## ##' as.data.frame for tabular object.
## ##'
## ##' as.data.frame for tabular object (internal).
## ##'
## ##' @export
## ##' @param x a tabular object
## ##' @param ... not used
## ##' @author David Hajage
## ##' @keywords internal
## as.data.frame.tabular <- function(x, ...) {
##   xx <- do.call("cbind", x)
##   stat <- attr(x[[1]], "lgroup")[[1]]
##   levels <- unlist(mapply(rep, attr(x[[1]], "lgroup")[[2]], attr(x[[1]], "n.lgroup")[[2]], SIMPLIFY = FALSE))
##   var <- rep(attr(x[[1]], "lgroup")[[3]], attr(x[[1]], "n.lgroup")[[3]])
##   data.frame(var = var, levels = levels, stat = stat, xx, row.names = NULL, check.names = FALSE)
## }

## ##' Test if \code{x} is an tabular object
## ##'
## ##' @param x a tabular object
## ##' @author David Hajage
## ##' @keywords internal
## is.tabular <- function(x)
##   inherits(x, "tabular")
