##' Compute correlation
##'
##' @param x numerical object
##' @param y numerical object
##' @param method method
##' @param digits digits
##' @author David Hajage
##' @keywords internal
correlation <- function(x, y, method = c("pearson", "kendall", "spearman"), digits = 2) {
  results <- as.character(round(cor.test(x, y, method = method)$estimate, digits))
  ## class(results) <- c("correlation", "matrix")
  results
}

##' Compute correlation (data.frame input)
##'
##' @param dfx data.frame
##' @param dfy data.frame
##' @param method method
##' @param digits digits
##' @param label label
##' @author David Hajage
##' @keywords internal
##' @importFrom Hmisc label.default
##' @importFrom plyr mapvalues
correlation.data.frame <- function(dfx, dfy, method = c("pearson", "kendall", "spearman"), digits = 2, label = FALSE) {

    noms.dfx <- names(dfx)
    noms.dfy <- names(dfy)

    if (label) {
        labs.dfx <- sapply(dfx, label.default)
        labs.dfx[labs.dfx == ""] <- noms.dfx[labs.dfx == ""]
        # names(dfx) <- noms.dfx
        labs.dfy <- sapply(dfy, label.default)
        labs.dfy[labs.dfy == ""] <- noms.dfy[labs.dfy == ""]
        # names(dfy) <- noms.dfy
    } else {
        labs.dfx <- noms.dfx
        labs.dfy <- noms.dfy
    }

    results <- sapply(dfy, function(y) sapply(dfx, function(x) correlation(x, y, method = method, digits = digits)))
    results <- data.frame(cbind(names(dfx), method[1], results))
    names(results) <- c(".id", "variable", names(dfy))
    rownames(results) <- NULL

    results$label <- mapvalues(results$`.id`, from = noms.dfx, to = labs.dfx)
    results <- results[, c(".id", "label", names(results)[!(names(results) %in% c(".id", "label"))])]

    attr(results, "noms.lig") <- noms.dfx
    attr(results, "noms.col") <- noms.dfy
    attr(results, "labs.lig") <- labs.dfx
    attr(results, "labs.col") <- labs.dfy
    attr(results, "n.lig") <- rep(1, nrow(results))
    attr(results, "n.col") <- rep(1, ncol(results) - 3)

    results
}

## ##' Ascii for correlation object.
## ##'
## ##' Ascii method for correlation object (internal).
## ##'
## ##' @export
## ##' @method ascii correlation
## ##' @import ascii
## ##' @param x a correlation object
## ##' @param format see \code{?ascii} in \code{ascii} package
## ##' @param digits see \code{?ascii} in \code{ascii} package
## ##' @param include.rownames see \code{?ascii} in \code{ascii} package
## ##' @param include.colnames see \code{?ascii} in \code{ascii} package
## ##' @param header see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords internal
## ascii.correlation <- function(x, format = "nice", digits = 5, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, ...) {
##   class(x) <- class(x)[-1]
##   ascii(x, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
## }

## ##' Print correlation object.
## ##'
## ##' Print correlation object (internal).
## ##'
## ##' @export
## ##' @method print correlation
## ##' @import ascii
## ##' @param x a correlation object
## ##' @param type type of output (see \code{?ascii} in \code{ascii}
## ##' package)
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords internal
## print.correlation <- function(x, type = "rest", ...) {
##   print(ascii.correlation(x, ...), type = type)
##   ## invisible(x)
## }

## ##' as.data.frame for correlation object.
## ##'
## ##' as.data.frame for correlation object (internal).
## ##'
## ##' @export
## ##' @param x a correlation object
## ##' @param ... not used
## ##' @author David Hajage
## ##' @keywords internal
## as.data.frame.correlation <- function(x, ...) {
##   as.data.frame(unclass(x))
## }

## ##' Test if \code{x} is a correlation object
## ##'
## ##' @param x a correlation object
## ##' @author David Hajage
## ##' @keywords internal
## is.correlation <- function(x)
##   inherits(x, "correlation")
