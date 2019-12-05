##' test
##'
##' @param x x
##' @author David Hajage
##' @keywords internal
is.character.or.factor <- function(x) {
  is.character(x) | is.factor(x)
}

##' test
##'
##' @param x x
##' @author David Hajage
##' @keywords internal
##' @import survival
is.numeric.and.not.surv <- function(x) {
  is.numeric(x) & !is.Surv(x)
}

##' Cross one x and one y
##'
##' @param x x
##' @param y y
##' @param funs funs
##' @param ... \dots
##' @param margin margin
##' @param total total
##' @param digits digits
##' @param showNA showNA
##' @param method method
##' @param times times
##' @param followup followup
##' @param test test
##' @param test.tabular test.tabular
##' @param test.summarize test.summarize
##' @param test.survival test.survival
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @param effect effect
##' @param effect.summarize effect.summarize
##' @param effect.tabular effect.tabular
##' @param conf.level conf.level
##' @param label label
##' @param effect.survival effect.survival 
##'
##' @author David Hajage
##' @keywords internal
cross_one <- function(x, y = NULL, funs = c(mean, sd, quantile, n, na), ..., margin = 0:2, total = FALSE, digits = 2, showNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman"), times = NULL, followup = FALSE, test = FALSE, test.tabular = test.tabular.auto, test.summarize = test.summarize.auto, test.survival = test.survival.logrank, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.summarize = diff.mean.auto, effect.tabular = or.row.by.col, effect.survival = effect.survival.coxph, conf.level = 0.95, label = FALSE) {
  if (!is.character(funs)) {
      nomf <- names(funs)
      funs <- as.character(as.list(substitute(funs)))
      funs <- funs[funs != "c" & funs != "list"]
      names(funs) <- nomf
  }

  results <- "What?"

  if (!is.null(x) & !is.null(y)) {
    if (is.numeric.and.not.surv(x[, 1]) & is.character.or.factor(y[, 1])) {
      results <- summarize.data.frame.by(x, y, funs = funs, ..., total = total, digits = digits, showNA = showNA, test = test, test.summarize = test.summarize, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, conf.level = conf.level, label = label)
    }
    if (is.numeric.and.not.surv(y[, 1]) & is.character.or.factor(x[, 1])) {
      results <- summarize.data.frame.by(y, x, funs = funs, ..., total = total, digits = digits, showNA = showNA, test = test, test.summarize = test.summarize, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, conf.level = conf.level, label = label)
    }
    if (is.Surv(x[, 1]) & is.character.or.factor(y[, 1])) {
      results <- survival.data.frame.by(x, y, times = times, followup = followup, total = total, digits = digits, test = test, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.survival = effect.survival, label = label)
    }
    if (is.Surv(y[, 1]) & is.character.or.factor(x[, 1])) {
      results <- survival.data.frame.by(y, x, times = times, followup = followup, digits = digits, total = total, test = test, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.survival = effect.survival, label = label)
    }
    if (is.character.or.factor(x[, 1]) & is.character.or.factor(y[, 1])) {
      results <- tabular.data.frame(x, y, margin = margin, total = total, digits = digits, showNA = showNA, test = test, test.tabular = test.tabular, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.tabular = effect.tabular, conf.level = conf.level, label = label)
    }
    if (is.numeric.and.not.surv(x[, 1]) & is.numeric.and.not.surv(y[, 1])) {
      results <- correlation.data.frame(x, y, method = method, label = label)
    }
  } else if (is.null(y)) {
    if (is.character.or.factor(x[, 1])) {
      results <- freq.data.frame(x, total = total, digits = digits, showNA = showNA, label = label)
    }
    if (is.numeric.and.not.surv(x[, 1])) {
      results <- summarize.data.frame(x, funs = funs, ..., digits = digits, label = label)
    }
    if (is.Surv(x[, 1])) {
      results <- survival.data.frame(x, times = times, followup = followup, digits = digits, label = label)
    }
  } else if (is.null(x)) {
    if (is.character.or.factor(y[, 1])) {
      results <- freq.data.frame(y, total = total, digits = digits, showNA = showNA, label = label)
    }
    if (is.numeric.and.not.surv(y[, 1])) {
      results <- summarize.data.frame(y, funs = funs, ..., digits = digits, label = label)
    }
    if (is.Surv(y[, 1])) {
      results <- survival.data.frame(y, times = times, followup = followup, digits = digits, label = label)
    }
  }

  results
}

##' Cross all x with all y
##'
##' @param x x
##' @param y y
##' @param funs funs
##' @param ... \dots
##' @param margin margin
##' @param total total
##' @param digits digits
##' @param showNA showNA
##' @param method method
##' @param times times
##' @param followup followup
##' @param test test
##' @param test.tabular test.tabular
##' @param test.summarize test.summarize
##' @param test.survival test.survival
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @param effect effect
##' @param effect.summarize effect.summarize
##' @param effect.tabular effect.tabular
##' @param conf.level conf.level
##' @param label label
##' @param effect.survival effect.survival 
##'
##' @author David Hajage
##' @keywords internal
cross_all <- function(x, y = NULL, funs = c(mean, sd, quantile, n, na), ..., margin = 0:2, total = FALSE, digits = 2, showNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman"), times = NULL, followup = FALSE, test = FALSE, test.tabular = test.tabular.auto, test.summarize = test.summarize.auto, test.survival = test.survival.logrank, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.summarize = diff.mean.auto, effect.tabular = or.row.by.col, effect.survival = effect.survival.coxph, conf.level = 0.95, label = FALSE) {

    if (!is.character(funs)) {
        nomf <- names(funs)
        funs <- as.character(as.list(substitute(funs)))
        funs <- funs[funs != "c" & funs != "list"]
        names(funs) <- nomf
    }

    results <- "What?"

    if (!is.null(x) & !is.null(y)) {
        df <- data.frame(x, y, check.names = FALSE)
        nx <- names(x)
        ny <- names(y)
        croix <- expand.grid(nx, ny, stringsAsFactors = FALSE)
        type <- expand.grid(sapply(x, is.character.or.factor), sapply(y, is.character.or.factor))
        ## croix <- croix[!(!type$Var1 & !type$Var2), ]
        ## type <- type[!(!type$Var1 & !type$Var2), ]

        if (nrow(croix) > 0) {
            for (i in 1:nrow(type)) {
                if (type$Var1[i] & !type$Var2[i]) {
                    croix[i, ] <- rev(croix[i, ])
                    type[i, ] <- rev(type[i, ])
                }
            }

            res <- NULL
            for (i in 1:nrow(croix)) {
                # res <- c(res, list(cross_one(df[, croix$Var1[i], F], df[, croix$Var2[i], F], funs = funs, margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, label = label)))
                res <- c(res, list(cross_one(df[, croix$Var1[i], F], df[, croix$Var2[i], F], funs = funs, ..., margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, label = label)))
            }

            idx <- tapply(1:nrow(croix), croix$Var2, c)[unique(croix$Var2)]
            results <- unname(lapply(idx, function(i) rbind.list(res[i], TRUE)))

            idx2 <- tapply(1:length(results), tapply(croix$Var1, croix$Var2, paste, collapse = ""), c)

            ## idx2 <- tapply(1:length(results), sapply(results, function(x)  paste(paste.matrix(x[, 1:2], byrow = F, collapse = ""), collapse = "")), c)
            ## idx2 <- idx2[order(sapply(idx2, function(x) x[1]))]
            ## idx2 <- unname(idx2)

            results <- unname(lapply(idx2, function(i) {
                tmp <- results[i]
                if (length(tmp) > 1) {
                    # res <- cbind(tmp[[1]], cbind.list(lapply(tmp[-1], function(x) x[, -(1:2), FALSE])))
                    res <- cbind.list(c(list(tmp[[1]]), list(cbind.list(lapply(tmp[-1], function(x) {
                        xx <- x[, -(1:2), FALSE]
                        attr(xx, "noms.col") <- attr(x, "noms.col")
                        attr(xx, "labs.col") <- attr(x, "labs.col")
                        attr(xx, "n.col") <- attr(x, "n.col")
                        attr(xx, "noms.lig") <- attr(x, "noms.lig")
                        attr(xx, "labs.lig") <- attr(x, "labs.lig")
                        attr(xx, "n.lig") <- attr(x, "n.lig")
                        xx}), TRUE))), TRUE)
                } else {
                    res <- tmp[[1]]
                }
                res
            }))
        }
    } else if (is.null(y)) {
        res <- NULL
        for (i in 1:ncol(x)) {
            # res <- c(res, list(cross_one(x[, i, FALSE], NULL, funs = funs, margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, show.test = show.test, plim = plim, show.method = show.method, label = label)))
            res <- c(res, list(cross_one(x[, i, FALSE], NULL, funs = funs, ..., margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, label = label)))
        }
        results <- rbind.list(res, TRUE)
    } else if (is.null(x)) {
        res <- NULL
        for (i in 1:ncol(y)) {
            res <- c(res, list(cross_one(y[, i, FALSE], NULL, funs = funs, ..., margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, label = label)))
        }
        results <- rbind.list(res, TRUE)
    }

    if (length(results) == 1) {
        results <- results[[1]]
    }

    results
}

##' Cross variables in a list
##'
##' @param l l
##' @param funs funs
##' @param ... \dots
##' @param margin margin
##' @param total total
##' @param digits digits
##' @param showNA showNA
##' @param method method
##' @param times times
##' @param followup followup
##' @param test test
##' @param test.summarize test.summarize
##' @param test.survival test.survival
##' @param test.tabular test.tabular
##' @param show.test show.test
##' @param plim plim
##' @param show.method show.method
##' @param effect effect
##' @param effect.summarize effect.summarize
##' @param effect.tabular effect.tabular
##' @param conf.level conf.level
##' @param label label
##' @param effect.survival effect.survival 
##'
##' @author David Hajage
##' @keywords internal
cross_list <- function(l, funs = c(mean, sd, quantile, n, na), ..., margin = 0:2, total = FALSE, digits = 2, showNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman"), times = NULL, followup = FALSE, test = FALSE, test.summarize = test.summarize.auto, test.survival = test.survival.logrank, test.tabular = test.tabular.auto, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.summarize = diff.mean.auto, effect.tabular = or.row.by.col, effect.survival = effect.survival.coxph, conf.level = 0.95, label = FALSE) {

  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  x <- l[[1]]
  if (length(l) == 2) {
    y <- l[[2]]
  } else {
    y <- NULL
  }

  # cross_all(x = x, y = y, funs = funs, margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, label = label)
  
  cross_all(x = x, y = y, funs = funs, ..., margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, label = label)
}

##' Regroup factors with factors, and numerical variables with numerical variables
##'
##' @param vars vars
##' @param numdata numdata
##' @param catdata catdata
##' @param survdata survdata
##' @author David Hajage
##' @keywords internal
regroup <- function(vars, numdata, catdata, survdata) {
  vars <- lapply(vars, function(x) remove_blank(elements(x)))

  results <- unique(unlist(lapply(vars, function(x) {
    numvars <- x[x %in% numdata]
    catvars <- x[x %in% catdata]
    survvars <- x[x %in% survdata]
    dotvars <- x[x == "."]
    xx <- c(if (length(numvars) > 1) paste("cbind(", paste(numvars, collapse = ","), ")", sep = "") else numvars,
            if (length(catvars) > 1) paste("cbind(", paste(catvars, collapse = ","), ")", sep = "") else catvars,
            if (length(survvars) > 1) paste("cbind(", paste(survvars, collapse = ","), ")", sep = "") else survvars,
            if (length(dotvars) >= 1) ".")
    xx[xx != "cbind()"]
  })))

  if (length(results) == 0)
    results <- "."
  results
}

##' Describe everything
##'
##' A quick and easy function for describing datasets.
##'
##' @param formula a formula (see Details).
##' @param data a data.frame.
##' @param funs Functions used for describing numeric
##' variables. Vector (named or not): \code{c(fun1, fun2, fun3)} or
##' \code{c("fun1", "fun2", "fun3")}.
##' @param ... further arguments, all passed to funs. For example
##' {na.rm = TRUE}
##' @param margin index, or vector of indices to indicate which
##' proportions should be computed in frequency tables (0: cell, 1:
##' row, 2: col).
##' @param total whether to add margins. Integers (\code{c(1, 2)}:
##' both row and col margins, 1: row margins, 2: col margins, 0: no
##' margins) or logical (\code{TRUE}: row and col margins,
##' \code{FALSE}: no margins).
##' @param digits number of digits
##' @param showNA whether to show NA (\code{c("no", "ifany", "always"}
##' like in \code{table()})
##' @param method a character string indicating which correlation
##' coefficient is to be used. One of \code{"pearson"},
##' \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
##' @param times vector of times (see \code{?summary.survival} in
##' package \code{survival}).
##' @param followup whether to display follow-up time.
##' @param test whether to perform tests
##' @param test.summarize a function of two arguments (continuous
##' variable and grouping variable), used to compare continuous
##' variable. Returns a list of two components : \code{p.value} and
##' \code{method} (the test name). See \code{test.summarize.auto},
##' \code{test.summarize.kruskal},
##' \code{test.summarize.oneway.equalvar}, or
##' \code{test.summarize.unequalvar} for some examples of such
##' functions. Users can provide their own function.
##' @param test.survival a function of one argument (a formula), used
##' to compare survival estimations. Returns the same components as
##' created by \code{test.summarize}. See
##' \code{test.survival.logrank}. Users can provide their own
##' function.
##' @param test.tabular a function of two arguments (two categorical
##' variables), used to test association between two factors.  Returns
##' the same components as created by \code{test.summarize}. See
##' \code{test.tabular.auto} and \code{test.tabular.fisher}. Users can
##' provide their own function.
##' @param show.test function used to display the test result. See
##' \code{display.test}.
##' @param plim number of digits for the p value
##' @param show.method wether to display the test name (logical)
##' @param effect whether to compute a effect measure
##' @param effect.summarize a function of three arguments (continuous
##' variable, grouping variable and conf.level), used to compare continuous
##' variable. Returns a list of five components : \code{effect} (the effect value(s)),
##' \code{ci} (the matrix of confidence interval(s)), \code{effect.name} (the 
##' interpretiation(s) of the effect value(s)), \code{effect.type} (the description of 
##' the measure used) and \code{conf.level} (the confidence interval level). See 
##' \code{diff.mean.auto}, \code{diff.mean.student} or \code{diff.mean.boot} for some examples of such
##' functions. Users can provide their own function.
##' @param effect.tabular a function of three arguments (two categorical variables and conf.level) used to measure the associations between two factors.
##' Returns a list of five components : \code{effect} (the effect value(s)),
##' \code{ci} (the matrix of confidence interval(s)), \code{effect.name} (the 
##' interpretiation(s) of the effect value(s)), \code{effect.type} (the description of 
##' the measure used) and \code{conf.level} (the confidence interval level). See 
##' \code{or.row.by.col}, \code{rr.row.by.col}, \code{rd.row.by.col}, \code{or.col.by.row}, \code{rr.col.by.row}, or \code{rd.col.by.row} for some examples of such functions. Users can provide their own function.
##' @param effect.survival a function of two argument (a formula and conf.level), used
##' to measure the association between a consored and a factor. Returns the same components as
##' created by \code{effect.summarize}. See
##' \code{effect.survival.coxph}. Users can provide their own
##' function.
##' @param conf.level The desired confidence interval level 
##' @param label whether to display labels of variables (using
##' \code{label} in package \code{Hmisc})
##' @param regroup whether to regroup numerics with numerics and
##' factors with factors in \code{cbind} (logical)
##' @note The formula has the following format: \code{x_1 + x_2 +
##' ... ~ y_1 + y_2 + ...}
##'
##' There are a couple of special variables: \code{...} represents all
##' other variables not used in the formula and \code{.}  represents
##' no variable, so you can do \code{formula = var1 ~ .}.
##'
##' If \code{var1} is numeric, \code{var1 ~ .} produce a summary table
##' using \code{funs}. If \code{var1} is a factor, \code{var1 ~ .}
##' produce a frequency table. If \code{var1} is of class \code{Surv},
##' \code{var1 ~ .} produce a table with the estimates of survival at
##' \code{times}. If \code{var1} is numeric and \code{var2} is
##' numeric, \code{var1 ~ var2} produces a correlation correlation
##' coefficient. if \code{var1} is numeric and \code{var2} is a
##' factor, \code{var1 ~ var2} produce a summary table (using
##' functions in \code{funs}) according to the levels of
##' \code{var2}. If \code{var1} is a factor and \code{var2} is a
##' factor, \code{var1 ~ var2} produce a contingency table. If
##' \code{var1} is of class \code{Surv} and \code{var2} is a factor,
##' \code{var1 ~ var2} produce a table with the estimates of survival
##' for each level of \code{var2}.
##'
##' You can group several variables together with \code{cbind(var1,
##' var2, var3)}: \code{var1}, \code{var2} and \code{var3} will be
##' grouped in the same table. \code{cbind(...)} works (ie regroups
##' all variables of the data.frame together). When a \code{cbind} is
##' in both sides of the formula, \code{cross} will do its best to
##' group everything in the same table, but only if it is possible...
##'
##'
##' @return
##'   A data.frame, or a list of data.frames.
##' @author David Hajage, inspired by the design and the code of
##'   \code{summary.formula} (\code{Hmisc} package, FE Harrell) and
##'   \code{cast} (\code{reshape} package, H Wickham).
##' @seealso \code{cast} (reshape) and \code{summary.formula} (Hmisc).
##' @examples
##'
##' library(biostat2)
##' cross(data = iris)
##' cross(cbind(...) ~ ., iris[, sapply(iris, is.numeric)], funs=c(median, mad, min, max))
##' cross(cbind(Sepal.Length, I(Sepal.Width^2)) ~ Species, iris, funs=quantile, probs=c(1/3, 2/3), total="line") #tertiles 1 and 2 by Species
##' cross(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width, iris)
##' cross(cbind(Sepal.Length, Sepal.Width) ~ cbind(Petal.Length, Petal.Width), iris)
##' cross(... ~ ., esoph) #returns a list
##' cross(alcgp ~ tobgp, esoph, margin="line", total="both", test=TRUE)
##' cross(cbind(hp, mpg) ~ factor(am), mtcars, effect=TRUE, test=TRUE, show.method=FALSE)
##' library(survival)
##' cross(Surv(time, status) ~ x, data = aml)
##' @keywords univar
##' @export
##' @import checkmate
##' @importFrom plyr llply is.formula
# TODO: show.effect.details=T, 
cross <- function(formula = cbind(...) ~ ., data = NULL, funs = c(" " = mysummary), ..., margin = c("all", "line", "column", "cell"), total = c("none", "all", "line", "column", "FALSE", "TRUE", 0, 1, 2), digits = 2, showNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman"), times = NULL, followup = FALSE, test = FALSE, test.summarize = test.summarize.auto, test.survival = test.survival.logrank, test.tabular = test.tabular.auto, show.test = display.test, plim = 4, show.method = TRUE, effect = FALSE, effect.summarize = diff.mean.auto, effect.tabular = or.row.by.col, effect.survival = effect.survival.coxph, conf.level = 0.95, label = FALSE, regroup = FALSE) {
  
  coll = makeAssertCollection()
  assertFormula(formula, add=coll)
  assertDataFrame(data, add=coll)
  assertCount(digits, add=coll)
  assertLogical(label, add=coll)
  
  if (is.formula(formula))
    formula <- paste(deparse(formula, 500), collapse = "")
  
  if (!is.character(funs)) {
    nomf <- names(funs)
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
    names(funs) <- nomf
  }
   
  if (missing(margin)) margin = "all"
  if (is.character(margin)) {
    assertSubset(margin, c("all", "line", "column", "cell"), add=coll)
    if(is.null(margin)) {
      margin=0:2 #defaulting 
    } else {
      marginopts = list(all = 0:2,
                        line = 1,
                        column = 2,
                        cell = 0)
      margin <- unname(unlist(marginopts[margin]))
    }
  }
  
  if (missing(total)) total = "none"
  if (is.character(total)) {
    assertChoice(total, c("none", "both", "all", "line", "column"), add=coll)
    if(is.null(total)) {
      total=0 #defaulting
    } else {
      totalopts = list(all = 1:2,
                       both = 1:2,
                       line = 1,
                       column = 2,
                       none = 0)
      total <- unname(unlist(totalopts[total]))
    }
  }
  
  reportAssertions(coll)
  
  
  varnames <- names(data)
  parsed <- parse_formula(formula, varnames)
  # parsed$left = gsub("\\n *", "", parsed$left) 
  
  data <-   parse_data(expand_formula(formula, varnames), data)
  names(data) <- remove_blank(names(data))
  varform <- names(data)

  if (regroup) {
      numdata <- varform[sapply(data, function(x) is.numeric(x) & !is.Surv(x))]
      catdata <- varform[sapply(data, is.character.or.factor)]
      survdata <- varform[sapply(data, is.Surv)]

      parsed$left <- regroup(parsed$left, numdata, catdata, survdata)
      parsed$right <- regroup(parsed$right, numdata, catdata, survdata)
  }

  eg <- expand.grid(parsed$left, parsed$right)

  comb <- lapply(apply(eg, 1, list), function(x) {
      y <- unlist(x)
      y <- y[y != "."]
      ## y <- sub("(cbind\\()(.*)(\\))", "\\2", y)
      ## lapply(y, function(z) data[, strsplit(z, ",")[[1]], drop = FALSE])})
      lapply(y, function(z) data[, remove_blank(elements(z)), drop = FALSE])
  })

  
  results <- llply(comb, function(x) 
    cross_list(x, funs = funs, ..., margin = margin, total = total, digits = digits, showNA = showNA, method = method, times = times, followup = followup, test = test, test.summarize = test.summarize, test.tabular = test.tabular, test.survival = test.survival, show.test = show.test, plim = plim, effect = effect, effect.summarize = effect.summarize, effect.tabular = effect.tabular, effect.survival = effect.survival, conf.level = conf.level, show.method = show.method, label = label))

  if (length(results) == 1) {
      results <- results[[1]]
      class(results) <- c("cross", "data.frame")

  } else {
      results <- results[results != "What?"]
      ## class(results) <- c("cross", "list")
  }
  ## names(results) <- apply(eg, 1, paste, collapse = " ~ ")

  ## class(results) <- c("remix")
  ## attr(results, "formula") <- formula
  # attr(results, "left") <- parsed$left
  # attr(results, "right") <- parsed$right
  ## ## attr(results, "by") <- parsed$by

  ## attr(results, "data") <- data
  ## attr(results, "test") <- test
  
  results
}





# Old ---------------------------------------------------------------------



## ##' Print a cross object
## ##'
## ##' Print cross object using ascii package
## ##'
## ##' @export
## ##' @method print cross
## ##' @param x a cross object
## ##' @param ... other arguments passed to nothing
## ##'    which has no effect)
## ##' @author David Hajage
## ##' @keywords univar
## print.cross <- function(x, ...) {
##     fun <- function(obj) {
##         y <- obj
##         y[, 1] <- ifelse(!duplicated(y[, 1]), as.character(y[, 1]), "")
##         if (attr(obj, "test"))
##             y[, ncol(y)] <- ifelse(!duplicated(y[, ncol(y)]), as.character(y[, ncol(y)]), "")
##         class(y) <- "data.frame"
##         y
##     }

##     if (is.data.frame(x)) {
##         res <- fun(x)
##     } else {
##         res <- lapply(x, fun)
##     }
##     print(res)
## }

## ##' Ascii for remix object.
## ##'
## ##' Ascii method for remix object.
## ##'
## ##' @export
## ##' @method ascii remix
## ##' @import ascii
## ##' @param x a remix object
## ##' @param caption.level see \code{?ascii} in \code{ascii} package
## ##' @param format see \code{?ascii} in \code{ascii} package
## ##' @param digits see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii} (all except \code{caption}
## ##'    which has no effect)
## ##' @author David Hajage
## ##' @keywords univar
## ascii.remix <- function(x, caption.level = c("s", "e", "m"), format = "nice", digits = 2, ...) {
##   caption.level <- rep(caption.level, length = 3)
##   caption.level1 <- caption.level[1]
##   caption.level2 <- caption.level[2]
##   caption.level3 <- caption.level[3]

##   xx <- list()
##   ## if (all(attr(x, "by") == ".")) {
##     captions <- names(x)
##     for (i in 1:length(x)) {
##       xx[[i]] <- ascii(x[[i]], caption = captions[i], caption.level = caption.level1, format = format, digits = digits, ...)
##     }
##   ## } else if (length(attr(x, "by")) == 1) {
##   ##   captions1 <- names(x)
##   ##   captions2 <- names(x[[1]])
##   ##   for (i in 1:length(x)) {
##   ##     asc.cap1 <- ascii(list(NULL), caption = captions1[i], caption.level = caption.level1)
##   ##     xx[[paste("obj", i, sep = "")]] <- asc.cap1
##   ##     for (j in 1:length(x[[i]])) {
##   ##       xx[[paste("obj", i, j, sep = "")]] <- ascii(x[[i]][[j]], caption = captions2[j], caption.level = caption.level2, format = format, digits = digits, ...)
##   ##     }
##   ##   }
##   ## } else if (length(attr(x, "by")) > 1) {
##   ##   captions1 <- names(x)
##   ##   captions3 <- names(x[[1]][[1]])
##   ##   for (i in 1:length(x)) {
##   ##     asc.cap1 <- ascii(list(NULL), caption = captions1[i], caption.level = caption.level1)
##   ##     xx[[paste("obj", i, sep = "")]] <- asc.cap1
##   ##     for (j in 1:length(x[[i]])) {
##   ##       captions2 <- names(x[[i]])
##   ##       asc.cap2 <- ascii(list(NULL), caption = captions2[j], caption.level = caption.level2)
##   ##       xx[[paste("obj", i, j, sep = "")]] <- asc.cap2
##   ##       for (k in 1:length(x[[i]][[j]])) {
##   ##         xx[[paste("obj", i, j, k, sep = "")]] <- ascii(x[[i]][[j]][[k]], caption = captions3[k], caption.level = caption.level3, format = format, digits = digits)
##   ##       }
##   ##     }
##   ##   }
##   ## }
##   asciiMixed$new(args = xx)
## }

## ##' Print a remix object
## ##'
## ##' Print remix object using ascii package
## ##'
## ##' @export
## ##' @import ascii
## ##' @method print remix
## ##' @param x a remix object
## ##' @param type type of output. See \code{?ascii} in \code{ascii} package
## ##' @param caption.level see \code{?ascii} in \code{ascii} package
## ##' @param lstyle see \code{?ascii} in \code{ascii} package
## ##' @param tstyle see \code{?ascii} in \code{ascii} package
## ##' @param ... other arguments passed to \code{ascii} (all except \code{caption}
## ##'    which has no effect)
## ##' @author David Hajage
## ##' @keywords univar
## print.remix <- function(x, type = "rest", caption.level = 1:3, lstyle = "", tstyle = "", ...) {
##   print(ascii.remix(x, caption.level = caption.level, lstyle = lstyle, tstyle = tstyle, ...), type = type)
##   ## invisible(x)
## }

## ##' Demix
## ##'
## ##' Transfrom a remix object into a (list of) data.frame(s).
## ##'
## ##' @export
## ##' @param x a remix object
## ##'
## ##' @return
## ##'   A list of data.frame.
## ##' @author David Hajage
## ##' @seealso \code{remix}
## ##' @examples
## ##'   x <- remix(... ~ ., esoph, cum = TRUE)
## ##'   demix(x)
## demix <- function(x) {
##   ## if (all(attr(x, "by") == ".")) {
##     result <- lapply(x, as.data.frame)
##   ## } else if (length(attr(x, "by")) == 1) {
##   ##   result <- lapply(x, function(x) lapply(x, as.data.frame))
##   ## } else if (length(attr(x, "by")) > 1) {
##   ##   result <- lapply(x, function(x) lapply(x, function(x) lapply(x, as.data.frame)))
##   ## }
##   return(result)
## }

## ##' Test if \code{x} is an remix object
## ##'
## ##' Test if \code{x} is an remix object
## ##'
## ##' @param x a remix object
## ##' @author David Hajage
## ##' @keywords internal
## is.remix <- function(x)
##     inherits(x, "remix")
