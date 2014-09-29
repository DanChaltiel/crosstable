##' Compute frequencies
##'
##' @param x factor
##' @param showNA showNA
##' @param total total
##' @param digits digits
##' @author David Hajage
##' @keywords internal
freq <- function(x, showNA = c("no", "ifany", "always"), total = FALSE, digits = 2) {

    total <- sum(total)

  nn <- table(x, useNA = showNA)
  n <- as.character(nn)
  names(n) <- rownames(nn)
  p <- paste("(", as.character(round(100*prop.table(table(x)), digits)), "%)", sep = "")
  if (length(n) != length(p)) {
      p <- c(p, "NA" = "")
  }

  if (total) {
      n <- c(n, Total = as.character(sum(table(x))))
      p <- c(p, Total = "(100%)")
  }

  value <- paste.matrix(n, p)

  nom <- names(n)
  nom[is.na(nom)] <- "NA"
  results <- data.frame("variable" = nom, value = value)

  ## Si NA n'est pas dans le facteur, on met la colonne après "Total"
  if ((any(results$variable == "NA") & any(results$variable == "Total")) & !anyNA(levels(x))) {
      tmp <- results[results$variable == "NA", ]
      results <- rbind(results[results$variable != "NA", ], tmp)
  }

  results
}

##' Compute frequencies (data.frame input)
##'
##' @param df data.frame
##' @param showNA showNA
##' @author David Hajage
##' @keywords internal
##' @importFrom Hmisc label.default
freq.data.frame <- function(df, showNA = c("no", "ifany", "always"), total = FALSE, digits = 2, label = FALSE) {
  dfl <- as.list(df)

  if (label) {
      labs <- sapply(dfl, label.default)
      names(dfl)[labs != ""] <- labs[labs != ""]
  }

  results <- ldply(dfl, freq, showNA = showNA, total = total, digits = digits)
  results
}

## ##' Ascii for freq object.
## ##'
## ##' Ascii method for freq object (internal).
## ##'
## ##' @export
## ##' @method ascii freq
## ##' @import ascii
## ##' @param x a freq object
## ##' @param format see \code{?ascii} in \code{ascii} package
## ##' @param digits see \code{?ascii} in \code{ascii} package
## ##' @param include.rownames see \code{?ascii} in \code{ascii} package
## ##' @param rownames see \code{?ascii} in \code{ascii} package
## ##' @param include.colnames see \code{?ascii} in \code{ascii} package
## ##' @param header see \code{?ascii} in \code{ascii} package
## ##' @param lgroup see \code{?ascii} in \code{ascii} package
## ##' @param n.lgroup see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords univar
## ascii.freq <- function(x, format = "nice", digits = 3, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), ...) {
##   class(x) <- class(x)[-1]
##   ascii(x, include.colnames = include.colnames, include.rownames = include.rownames, header = header, lgroup = lgroup, n.lgroup = n.lgroup, format = format, digits = digits, ...)
## }

## ##' Print freq object.
## ##'
## ##' Print freq object (internal).
## ##'
## ##' @export
## ##' @import ascii
## ##' @method print freq
## ##' @param x a freq object
## ##' @param type type of output (see \code{?ascii} in \code{ascii}
## ##' package)
## ##' @param lstyle see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords univar
## print.freq <- function(x, type = "rest", lstyle = "", ...) {
##   print(ascii.freq(x, lstyle = lstyle, ...), type = type)
##   invisible(x)
## }

## ##' as.data.frame for freq object.
## ##'
## ##' as.data.frame for freq object (internal).
## ##'
## ##' @export
## ##' @param x a freq object
## ##' @param ... not used
## ##' @author David Hajage
## ##' @keywords internal
## as.data.frame.freq <- function(x, ...) {
##   xx <- unclass(x)
##   var <- unlist(mapply(rep, attr(x, "lgroup")[[2]], attr(x, "n.lgroup")[[2]], SIMPLIFY = FALSE))
##   levels <- attr(x, "lgroup")[[1]]

##   data.frame(var = var, levels = levels, xx, row.names = NULL, check.names = FALSE)
## }

## ##' Test if \code{x} is an freq object
## ##'
## ##' @export
## ##' @param x a freq object
## is.freq <- function(x)
##   inherits(x, "freq")
