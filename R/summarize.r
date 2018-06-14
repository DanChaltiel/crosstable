##' Compute summary statistics
##'
##' @param x numeric object
##' @param funs functions
##' @param ... passed to funs
##' @param digits number of digits
##' @author David Hajage
##' @keywords internal
summarize <- function(x, funs = c(mean, sd, quantile, n, na), ..., digits = 2) {
  if (!is.numeric(x))
    stop("x doit etre numerique")

  if (!is.character(funs)) {
      nomf <- names(funs)
      funs <- as.character(as.list(substitute(funs)))
      funs <- funs[funs != "c" & funs != "list"]
      names(funs) <- nomf
  }

  fun <- do.call(funs2fun, as.list(funs))
  results <- fun(x, ...)
  results <- sapply(results, function(x) if (is.numeric(x)) as.character(round(x, digits)) else as.character(x))
  results <- data.frame(variable = names(results), value = results, row.names = NULL, check.names = FALSE)

  results
}

##' Compute summary statistics (data.frame input)
##'
##' @param df a data.frame
##' @param funs functions
##' @param ... passed to funs
##' @param digits number of digits
##' @param label add labels
##' @author David Hajage
##' @keywords internal
##' @importFrom Hmisc label
##' @importFrom plyr ldply mapvalues
summarize.data.frame <- function(df, funs = c(mean, sd, quantile, n, na), ..., digits = 2, label = FALSE) {
  if (!is.character(funs)) {
      nomf <- names(funs)
      funs <- as.character(as.list(substitute(funs)))
      funs <- funs[funs != "c" & funs != "list"]
      names(funs) <- nomf
  }

  noms.df <- names(df)

  if (label) {
      labs.df <- sapply(df, label)
      labs.df[labs.df == ""] <- noms.df[labs.df == ""]
      # names(df) <- noms.df
  } else {
      labs.df <- noms.df
  }

  dfl <- as.list(df)
  results <- ldply(dfl, summarize, funs = funs, ..., digits = digits)

  n.df <- rep(length(unique(results$variable)), length(dfl))

  results$label <- mapvalues(results$`.id`, from = noms.df, to = labs.df)
  results <- results[, c(".id", "label", names(results)[!(names(results) %in% c(".id", "label"))])]

  attr(results, "noms.lig") <- noms.df
  attr(results, "noms.col") <- character(0)
  attr(results, "labs.lig") <- labs.df
  attr(results, "labs.col") <- character(0)
  attr(results, "n.lig") <- n.df
  attr(results, "n.col") <- numeric(0)

  results
}

## ##' Ascii for summarize object.
## ##'
## ##' Ascii method for summarize object (internal).
## ##'
## ##' @export
## ##' @method ascii summarize
## ##' @import ascii
## ##' @param x a summarize object
## ##' @param format see \code{?ascii} in \code{ascii} package
## ##' @param digits see \code{?ascii} in \code{ascii} package
## ##' @param include.rownames see \code{?ascii} in \code{ascii} package
## ##' @param include.colnames see \code{?ascii} in \code{ascii} package
## ##' @param header see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords univar
## ascii.summarize <- function(x, format = "nice", digits = 5, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, ...) {
##   if (is.null(nrow(x))) {
##     x <- t(x)
##   }
##   class(x) <- class(x)[-1]
##   ascii(x, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
## }

## ##' Print summarize object.
## ##'
## ##' Print summarize object (internal).
## ##'
## ##' @export
## ##' @method print summarize
## ##' @import ascii
## ##' @param x a summarize object
## ##' @param type type of output (see \code{?ascii} in \code{ascii}
## ##' package)
## ##' @param lstyle see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords internal
## print.summarize <- function(x, type = "rest", lstyle = "", ...) {
##   print(ascii.summarize(x, lstyle = lstyle, ...), type = type)
##   ## invisible(x)
## }

## ##' as.data.frame for summarize object.
## ##'
## ##' as.data.frame for summarize object (internal).
## ##'
## ##' @export
## ##' @param x a summarize object
## ##' @param ... not used
## ##' @author David Hajage
## ##' @keywords internal
## as.data.frame.summarize <- function(x, ...) {
##   as.data.frame(unclass(x))
## }

## ##' Test if \code{x} is a summarize object
## ##'
## ##' @param x a summarize object
## ##' @author David Hajage
## ##' @keywords internal
## is.summarize <- function(x)
##   inherits(x, "summarize")
