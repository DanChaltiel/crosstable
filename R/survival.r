##' Compute survival
##'
##' @import survival
##' @importFrom Hmisc label
##' @importFrom plyr daply
##'
##' @param surv a Surv object
##' @param by by
##' @param times times
##' @param followup followup
##' @param digits digits
##' @param test test
##' @param test.survival test.survival
##' @param show.test show.test
##' @param plim plim
##' @param total total 
##' @param effect effect 
##' @param effect.survival effect.survival 
##' @param show.effect show.effect 
##' @param conf.level conf.level 
##' @param label label
##' @param show.method show.method
##'
##' @author David Hajage
##' @keywords internal
survival <- function(surv, by = NULL, times = NULL, followup = FALSE, total = FALSE, digits = 2, test = FALSE, test.survival = test.survival.logrank, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.survival = effect.survival.coxph, show.effect = display.effect, conf.level = 0.95, label = FALSE) {

  df <- unclass(surv)
  if (!is.null(by)) {
    formula <- as.formula(paste("Surv(df[, 1], df[, 2]) ~ by", sep = ""))
    formula.followup <- as.formula(paste("Surv(df[, 1], 1-df[, 2]) ~ by", sep = ""))
  } else {
    formula <- as.formula(paste("Surv(df[, 1], df[, 2]) ~ 1", sep = ""))
    formula.followup <- as.formula(paste("Surv(df[, 1], 1-df[, 2]) ~ 1", sep = ""))
  }

  survfit.obj <- survfit(formula)

  if (followup) {
    suivfit.obj <- survfit(formula.followup)
  }

  if (is.null(times)) {
    times <- sort(survfit.obj$time)
  }
  x <- summary(survfit.obj, times = times, extend = TRUE)

  mat <- cbind(x$surv, x$n.event, x$n.risk)
  mat <- paste.matrix(round(mat[, 1], digits), " (", mat[, 2], "/", mat[, 3], ")", sep = "")

  if (!is.null(x$strata)) {
    strata <- x$strata
    ## results <- tocharac(do.call("cbind", split(as.data.frame(mat), strata)))
    results <- sapply(do.call("cbind", split(as.data.frame(mat), strata)), as.character)
    nstrata <- length(unique(strata))

    if (followup) {
      mediansuiv <- round(summary(suivfit.obj)$table[, "median"], digits = digits)
      tmp <- data.frame(unclass(model.frame(formula)[, 1]), model.frame(formula)[, 2])
      bornes <- daply(tmp, .(tmp[, 3]), function(df) paste("[", round(min(df[df[, 2] == 0, 1]), digits = digits), " ; ", round(max(df[, 1]), digits = digits), "]", sep = ""))
      suiv <- paste(mediansuiv, bornes)
    } else {
      suiv <- NULL
    }
    if (effect) {
        eff <- show.effect(effect.survival(formula, conf.level), digits = digits)
    } else {
        eff <- NULL
    }
    
    if (test) {
      p <- show.test(test.survival(formula), digits = plim, method = show.method)
    } else {
      p <- NULL
    }
    cnames <- names(table(by))
  } else {
    results <- mat
    nstrata <- 1
    if (followup) {
      mediansuiv <- round(summary(suivfit.obj)$table["median"], digits = digits)
      tmp <- unclass(model.frame(formula)[, 1])
      minsuiv <- round(min(tmp[tmp[, 2] == 0, 1]), digits = digits)
      maxsuiv <- round(max(tmp[, 1]), digits = digits)
      suiv <- paste(mediansuiv, " [", minsuiv, " ; ", maxsuiv, "]", sep = "")
    } else {
      suiv <- NULL
    }
    p <- NULL
    eff <- NULL
    cnames <- "value"
  }

  if (followup) {
    rnames <- c(times, "Median follow up [min ; max]", "Median survival")
  } else {
    rnames <- c(times, "Median survival")
  }
  if (nstrata == 1) {
    mediansurv <- round(x$table["median"], digits = digits)
  } else if (nstrata > 1) {
    mediansurv <- round(x$table[,"median"], digits = digits)
  }
  # mediansurv <- biostat2:::expand(round(x$table, digits = digits), nrow = nstrata, ncol = 7, drop = F)[, 7]

  results <- data.frame(paste("Surv(", colnames(df)[1], ", ", colnames(df)[2], ")", sep = ""), rnames, unname(rbind(results, suiv, mediansurv)), row.names = NULL)
  colnames(results) <- c(".id", "variable", cnames)
  results$p <- p
  results$effect <- eff
  results
}

##' Compute survival (data.frame input)
##'
##' @importFrom Hmisc label
##' @param df df
##' @param times times
##' @param followup followup
##' @param digits digits
##' @param label label
##' @author David Hajage
##' @importFrom plyr mapvalues
survival.data.frame <- function(df, times = NULL, digits = 2, followup = FALSE, label = FALSE) {

    noms.df <- names(df)

    if (label) {
        labs.df <- sapply(df, label)
        labs.df[labs.df == ""] <- noms.df[labs.df == ""]
        # names(df) <- noms.df
    } else {
        labs.df <- noms.df
    }

  results <- lapply(df, survival, NULL, times = times, followup = followup, digits = digits)

  for (i in 1:length(results)) {
      results[[i]][, 1] <- noms.df[i]
  }
  n.df <- sapply(results, nrow)

  results <- rbind.list(results)
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

##' Compute survival according to a factor (data.frame input)
##'
##' @import survival
##' @importFrom Hmisc label
##'
##' @param df df
##' @param by by
##' @param times times
##' @param followup followup
##' @param total total
##' @param digits digits
##' @param test test
##' @param test.survival test.survival
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @param effect effect 
##' @param effect.survival effect.survival 
##' @param show.effect show.effect 
##' @param conf.level conf.level 
##' @param label label
##'
##' @author David Hajage
survival.data.frame.by <- function(df, by, times = NULL, followup = FALSE, total = FALSE, digits = 2, test = FALSE, test.survival = test.survival.logrank, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.survival = effect.survival.coxph, show.effect = display.effect, conf.level = 0.95, label = FALSE) {

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

  dfx <- as.list(df)
  byx <- as.list(by)

  results <- lapply(byx, function(y) lapply(dfx, survival, y, times = times, followup = followup, digits = digits, test = test, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.survival = effect.survival, show.effect = show.effect, conf.level = conf.level))

  if (identical(total, 1) | identical(total, 1:2) | identical(total, TRUE)) {
      results.tot <- lapply(dfx, survival, times = na.omit(suppressWarnings(as.numeric(as.character(results[[1]][[1]]$variable)))), followup = followup, digits = digits, test = test, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.survival = effect.survival, show.effect = show.effect, conf.level = conf.level)

      for (i in 1:length(results)) {
          for (j in 1:length(results.tot)) {
              results[[i]][[j]]$Total <- results.tot[[j]]$value
          }
      }
  }


  results <- lapply(results, function(x) {
      for (i in 1:length(x)) {
          x[[i]][, 1] <- noms.df[i]
      }
      x
  })

  n.df <- sapply(results[[1]], nrow)
  n.df <- unname(n.df)
  
  results <- lapply(results, rbind.list)
  n.by <- sapply(results, ncol) - 2
  n.by <- unname(n.by)
  
  results <- data.frame(cbind.list(c(results[1], lapply(results[-1], function(x) x[, -c(1, 2)]))), stringsAsFactors = FALSE, check.names = FALSE)
  if ("p" %in% names(results) & "Total" %in% names(results)) {
      results <- results[, c(names(results)[!(names(results) %in% c("p", "Total"))], "Total", "p")]
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

## ##' Ascii for survival object.
## ##'
## ##' Ascii method for survival object (internal).
## ##'
## ##' @export
## ##' @method ascii survival
## ##' @import ascii
## ##' @param x a survival object
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
## ascii.survival <- function(x, format = "nice", digits = 5, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, rstyle = "d", caption = NULL, caption.level = NULL, ...) {
##   do.call(cbind.ascii, c(lapply(x, function(x) {
##     ascii(x, format = format, digits = digits, include.rownames = include.rownames, include.colnames = include.colnames, header = header, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), tgroup = attr(x, "tgroup"), n.tgroup = attr(x, "n.tgroup"), rgroup = attr(x, "rgroup"), n.rgroup = attr(x, "n.rgroup"), rstyle = rstyle, ...)}), caption = caption, caption.level = caption.level))
## }

## ##' Print survival object.
## ##'
## ##' Print summarize object (internal).
## ##'
## ##' @export
## ##' @method print survival
## ##' @import ascii
## ##' @param x a summarize object
## ##' @param type type of output (see \code{?ascii} in \code{ascii}
## ##' package)
## ##' @param lstyle see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii}
## ##' @author David Hajage
## ##' @keywords internal
## print.survival <- function(x, type = "rest", lstyle = "", ...) {
##   print(ascii.survival(x, lstyle = lstyle, ...), type = type)
##   ## invisible(x)
## }

## ##' as.data.frame for survival object.
## ##'
## ##' as.data.frame for survival object (internal).
## ##'
## ##' @export
## ##' @param x a summarize object
## ##' @param ... not used
## ##' @author David Hajage
## ##' @keywords internal
## as.data.frame.survival <- function(x, ...) {
##   xx <- do.call("cbind", x)
##   stat <- attr(x[[1]], "lgroup")[[1]]
##   var <- rep(attr(x[[1]], "lgroup")[[2]], attr(x[[1]], "n.lgroup")[[2]])
##   data.frame(var = var, stat = stat, xx, row.names = NULL, check.names = FALSE)
## }

## ##' Test if \code{x} is a survival object
## ##'
## ##' @param x a summarize object
## ##' @author David Hajage
## ##' @keywords internal
## is.survival <- function(x)
##   inherits(x, "survival")
