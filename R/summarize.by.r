##' Compute summary statistics according to a factor
##'
##' @param x numeric object
##' @param by factor
##' @param funs functions
##' @param ... passed to funs
##' @param showNA showNA
##' @param total total
##' @param digits digits
##' @param test test
##' @param test.summarize test.summarize
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @author David Hajage
##' @keywords internal
##' @importFrom plyr dlply
##' @importFrom plyr ddply
##' @importFrom plyr .
##' @importFrom reshape2 dcast
summarize.by <- function(x, by, funs = c(mean, sd, quantile, n, na), ..., showNA = c("no", "ifany", "always"), total = FALSE, digits = 2, test = FALSE, test.summarize = test.summarize.auto, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.summarize = diff.mean.auto, conf.level = 0.95, show.effect = display.effect) {

    showNA <- showNA[1]

    by2 <- by
    if (showNA == "always" | (showNA == "ifany" & anyNA(by))) {
        by2 <- addNA(by2)
    }

    if (!is.numeric(x))
        stop("x doit etre numerique")

    if (!is.character(funs)) {
        nomf <- names(funs)
        funs <- as.character(as.list(substitute(funs)))
        funs <- funs[funs != "c" & funs != "list"]
        names(funs) <- nomf
    }

    fun <- function(df, funs = funs, ...) summarize(df[, 1], funs = funs, digits = digits, ...)
    df <- data.frame(x, by2, check.names = FALSE)
    if (showNA == "no") {
        df <- df[!is.na(df$by2), ]
    }
    names(df) <- c(names(df)[1], "by")
    res <- ddply(df, .(by), fun, funs = funs, ..., .drop = FALSE)

    results <- dcast(res, variable ~ by, value.var = "value")

    results$variable <- factor(results$variable, as.character(unique(res$variable)), as.character(unique(res$variable)))
    results <- results[order(results$variable), ]

  if (identical(total, 1) | identical(total, 1:2) | identical(total, TRUE)) {
        results$Total <- summarize(x, funs = funs, ..., digits = digits)[, 2]
    }


    if (effect) {
        results <- cbind(results, effect = show.effect(effect.summarize(x, by, conf.level), digits = digits))
    }
    
    if (test) {
        results <- cbind(results, p = show.test(test.summarize(x, by), digits = plim, method = show.method))
    }

    results
}

##' Compute summary statistics according to a factor (data.frame input)
##'
##' @param df data.frame
##' @param by data.frame
##' @param funs fuctions
##' @param ... passed to funs
##' @param showNA showNA
##' @param total total
##' @param digits digits
##' @param test test
##' @param test.summarize test.summarize
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @param label label
##' @author David Hajage
##' @keywords internal
##' @importFrom Hmisc label
##' @importFrom plyr ldply mapvalues
summarize.data.frame.by <- function(df, by, funs = c(mean, sd, quantile, n, na), ..., showNA = c("no", "ifany", "always"), total = FALSE, digits = 2, test = FALSE, test.summarize = test.summarize.auto, show.test = display.test, plim = 4, show.method = TRUE, label = FALSE, effect = FALSE, effect.summarize = diff.mean.auto, conf.level = 0.95, show.effect = display.effect) {
  if (!is.character(funs)) {
      nomf <- names(funs)
      funs <- as.character(as.list(substitute(funs)))
      funs <- funs[funs != "c" & funs != "list"]
      names(funs) <- nomf
  }

    noms.df <- names(df)
    noms.by <- names(by)

  if (label) {
    labs.df <- sapply(df, label)
    labs.df[labs.df == ""] <- noms.df[labs.df == ""]
    # names(df) <- noms.df
    labs.by <- sapply(by, label)
    labs.by[labs.by == ""] <- noms.by[labs.by == ""]
    # names(by) <- noms.by
  } else {
      labs.df <- noms.df
      labs.by <- noms.by
  }

    # results <- llply(by, function(y) ldply(df, function(x) summarize.by(x, y, funs = funs, showNA = showNA, total = total, digits = digits, test = test, test.summarize = test.summarize, show.test = show.test, plim = plim, show.method = show.method)))
    results <- llply(by, function(y) ldply(df, function(x) summarize.by(x, y, funs = funs, ..., showNA = showNA, total = total, digits = digits, test = test, test.summarize = test.summarize, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, conf.level = conf.level, show.effect = show.effect)))

  if (length(results) > 1) {
      n.df <- rep(length(unique(results[[1]]$variable)), length(results))
      n.by <- laply(results, ncol) - 2
      results <- cbind(results[[1]], cbind.list(lapply(results[-1], function(x) x[, -(1:2)])))
  } else {
      n.df <- nrow(results[[1]])
      n.by <- ncol(results[[1]]) - 2
      results <- results[[1]]
  }

    results$label <- mapvalues(results$`.id`, from = noms.df, to = labs.df)
    results <- results[, c(".id", "label", names(results)[!(names(results) %in% c(".id", "label"))])]
    attr(results, "noms.lig") <- noms.df
    attr(results, "noms.col") <- noms.by
    attr(results, "labs.lig") <- labs.df
    attr(results, "labs.col") <- labs.by
    attr(results, "n.lig") <- n.df
    attr(results, "n.col") <- n.by

  results
}

## ##' Ascii for summarize.by object.
## ##'
## ##' Ascii method for summarize.by object (internal).
## ##'
## ##' @export
## ##' @method ascii summarize.by
## ##' @import ascii
## ##' @param x a summarize.by object
## ##' @param format see \code{?ascii} in \code{ascii} package
## ##' @param digits see \code{?ascii} in \code{ascii} package
## ##' @param include.rownames see \code{?ascii} in \code{ascii} package
## ##' @param include.colnames see \code{?ascii} in \code{ascii} package
## ##' @param header see \code{?ascii} in \code{ascii} package
## ##' @param lgroup see \code{?ascii} in \code{ascii} package
## ##' @param n.lgroup see \code{?ascii} in \code{ascii} package
## ##' @param rgroup see \code{?ascii} in \code{ascii} package
## ##' @param n.rgroup see \code{?ascii} in \code{ascii} package
## ##' @param rstyle see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords univar
## ascii.summarize.by <- function(x, format = "nice", digits = 5, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), rgroup = attr(x, "rgroup"), n.rgroup = attr(x, "n.rgroup"), rstyle = "d", ...) {
##   ascii(unclass(x), lgroup = lgroup, n.lgroup = n.lgroup, rgroup = rgroup, n.rgroup = n.rgroup, rstyle = rstyle, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
## }

## ##' Print summarize.by object.
## ##'
## ##' Print summarize.by object (internal).
## ##'
## ##' @export
## ##' @method print summarize.by
## ##' @import ascii
## ##' @param x a summarize.by object
## ##' @param type type of output (see \code{?ascii} in \code{ascii} package)
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords internal
## print.summarize.by <- function(x, type = "rest", lstyle = "", ...) {
##   print(ascii.summarize.by(x, lstyle = lstyle, ...), type = type)
##   ## invisible(x)
## }

## ##' as.data.frame for summarize.by object.
## ##'
## ##' as.data.frame for summarize.by object (internal).
## ##'
## ##' @export
## ##' @param x a summarize.by object
## ##' @param ... not used
## ##' @author David Hajage
## ##' @keywords internal
## as.data.frame.summarize.by <- function(x, ...) {
##   xx <- unclass(x)
##   if (!attr(x, "revert")) {
##     lgroup <- attr(x, "lgroup")
##     n.lgroup <- attr(x, "n.lgroup")
##     lgroup[[2]] <- unlist(mapply(rep, lgroup[[2]], each = n.lgroup[[2]], SIMPLIFY = FALSE))
##     lgroup[[3]] <- unlist(mapply(rep, lgroup[[3]], n.lgroup[[3]], SIMPLIFY = FALSE))
##     xx <- data.frame(by = lgroup[[3]], var = lgroup[[2]], levels = lgroup[[1]], xx, row.names = NULL, check.names = FALSE)
##   } else {
##     lgroup <- attr(x, "lgroup")
##     n.lgroup <- attr(x, "n.lgroup")
##     lgroup[[2]] <- unlist(mapply(rep, lgroup[[2]], each = n.lgroup[[2]], SIMPLIFY = FALSE))
##     lgroup[[3]] <- unlist(mapply(rep, lgroup[[3]], n.lgroup[[3]], SIMPLIFY = FALSE))
##     xx <- data.frame(by = lgroup[[3]], levels = lgroup[[2]], var = lgroup[[1]], xx, row.names = NULL, check.names = FALSE)
##   }
##   xx
## }

## ##' Test if \code{x} is a summarize.by object
## ##'
## ##' @param x a summarize.by object
## ##' @author David Hajage
## ##' @keywords internal
## is.summarize.by <- function(x)
##   inherits(x, "summarize.by")
