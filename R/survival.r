## ##' Compute survival
## ##'
## ##' @import ascii
## ##' @param surv a Surv object
## ##' @param by by
## ##' @param times times
## ##' @param followup followup
## ##' @param test test
## ##' @param test.survival test.survival
## ##' @param show.test show.test
## ##' @param plim plim
## ##' @param show.method show.method
## ##' @author David Hajage
## ##' @keywords internal
## survival <- function(surv, by = NULL, times = NULL, followup = FALSE, test = FALSE, test.survival = test.survival.logrank, show.test = display.test, plim = 4, show.method = TRUE) {

##   df <- unclass(surv)
##   if (!is.null(by)) {
##     formula <- as.formula(paste("Surv(df[, 1], df[, 2]) ~ by", sep = ""))
##     formula.followup <- as.formula(paste("Surv(df[, 1], 1-df[, 2]) ~ by", sep = ""))
##   }
##   else {
##     formula <- as.formula(paste("Surv(df[, 1], df[, 2]) ~ 1", sep = ""))
##     formula.followup <- as.formula(paste("Surv(df[, 1], 1-df[, 2]) ~ 1", sep = ""))
##   }

##   survfit.obj <- survfit(formula)

##   if (followup) {
##     suivfit.obj <- survfit(formula.followup)
##   }

##   if (is.null(times)) {
##     times <- sort(survfit.obj$time)
##     x <- summary(survfit.obj, times = times, extend = TRUE)
##   } else {
##     x <- summary(survfit.obj, times = times, extend = TRUE)
##   }

##   mat <- cbind(x$surv, x$n.event, x$n.risk)
##   mat <- paste.matrix(round(mat[, 1], 4), " (", mat[, 2], "/", mat[, 3], ")", sep = "")
##   if (!is.null(x$strata)) {
##     strata <- x$strata
##     results <- tocharac(do.call("cbind", split(as.data.frame(mat), strata)))
##     nstrata <- length(unique(strata))

##     if (followup) {
##       mediansuiv <- summary(suivfit.obj)$table[, 5]
##       tmp <- data.frame(unclass(model.frame(formula)[, 1]), model.frame(formula)[, 2])
##       bornes <- daply(tmp, .(tmp[, 3]), function(df) paste("[", min(df[df[, 2] == 0, 1]), " ; ", max(df[, 1]), "]", sep = ""))
##       suiv <- paste(mediansuiv, bornes)
##     } else {
##       suiv <- NULL
##     }
##     if (test) {
##       p <- show.test(test.survival(formula), digits = plim, method = show.method)
##     } else {
##       p <- NULL
##     }
##     cnames <- names(table(by))
##   } else {
##     results <- mat
##     nstrata <- 1
##     if (followup) {
##       mediansuiv <- summary(suivfit.obj)$table[5]
##       tmp <- unclass(model.frame(formula)[, 1])
##       minsuiv <- min(tmp[tmp[, 2] == 0, 1])
##       maxsuiv <- max(tmp[, 1])
##       suiv <- paste(mediansuiv, " [", minsuiv, " ; ", maxsuiv, "]", sep = "")
##     } else {
##       suiv <- NULL
##     }
##     p <- NULL
##     cnames <- NULL
##   }

##   if (followup) {
##     rnames <- c("Median follow up [min ; max]", "Median survival", times)
##   } else {
##     rnames <- c("Median survival", times)
##   }
##   mediansurv <- expand(x$table, nrow = nstrata, ncol = 7, drop = F)[, 5]

##   results <- rbind(suiv, mediansurv, results)
##   colnames(results) <- cnames
##   attr(results, "rgroup") <- p
##   attr(results, "lgroup") <- list(rnames, paste("Surv(", colnames(df)[1], ", ", names(df)[2], ")", sep = ""))
##   attr(results, "n.lgroup") <- list(rep(1, nrow(results)), nrow(results))
##   class(results) <- c("survival", "matrix")
##   results
## }

## ##' Compute survival according to a factor (data.frame input)
## ##'
## ##' @importFrom Hmisc label
## ##' @param df df
## ##' @param by by
## ##' @param times times
## ##' @param followup followup
## ##' @param test test
## ##' @param test.survival test.survival
## ##' @param show.test show.test
## ##' @param plim plim
## ##' @param show.method show.method
## ##' @param label label
## ##' @author David Hajage
## survival.data.frame.by <- function(df, by, times = NULL, followup = FALSE, test = FALSE, test.survival = test.survival.logrank, show.test = display.test, plim = 4, show.method = TRUE, label = FALSE) {

##   dfx <- as.list(df)
##   byx <- as.list(by)

##   results <- lapply(byx, function(y) lapply(dfx, survival, y, times = times, followup = followup, test = test, test.survival = test.survival, show.test = show.test, plim = plim, show.method = show.method))

##   if (!label)
##     tgroup <- names(by)
##   else
##     tgroup <- sapply(by, Hmisc:::label.default)
##   n.tgroup <- sapply(by, nlevels)

##   lgroup <- lapply(results[[1]], function(x) attr(x, "lgroup"))
##   n.lgroup <- lapply(results[[1]], function(x) attr(x, "n.lgroup"))

##   for (i in 1:length(n.lgroup)) {
##     n.lgroup[[i]] <- c(n.lgroup[[i]], sum(n.lgroup[[i]][[2]]))
##   }
##   n.lgroup1 <- unlist(lapply(n.lgroup, function(x) x[[1]]))
##   n.lgroup2 <- unlist(lapply(n.lgroup, function(x) x[[2]]))
##   n.lgroup <- list(n.lgroup1, n.lgroup2)

##   lgroup1 <- unlist(lapply(lgroup, function(x) x[[1]]))
##   if (!label)
##     lgroup2 <- names(df)
##   else
##     lgroup2 <- sapply(df, Hmisc:::label.default)
##   lgroup <- list(lgroup1, lgroup2)

##   rgroup <- lapply(results, function(x) sapply(x, attr, "rgroup"))
##   n.rgroup <- n.lgroup[[2]]

##   results <- lapply(results, rbind.list)

##   attr(results[[1]], "lgroup") <- lgroup
##   attr(results[[1]], "n.lgroup") <- n.lgroup

##   if(test) {
##     for (i in 1:length(results)) {
##       attr(results[[i]], "rgroup") <- rgroup[[i]]
##       attr(results[[i]], "n.rgroup") <- n.rgroup
##     }
##   }

##   for (i in 1:length(results)) {
##     attr(results[[i]], "tgroup") <- tgroup[i]
##     attr(results[[i]], "n.tgroup") <- n.tgroup[i]
##   }
##   class(results) <- "survival"
##   results
## }

## ##' Compute survival (data.frame input)
## ##'
## ##' @param df df
## ##' @param times times
## ##' @param followup followup
## ##' @param label label
## ##' @author David Hajage
## survival.data.frame <- function(df, times = NULL, followup = FALSE, label = FALSE) {

##   results <- lapply(df, survival, NULL, times = times, followup = followup)

##   lgroup <- lapply(results, function(x) attr(x, "lgroup"))
##   n.lgroup <- lapply(results, function(x) attr(x, "n.lgroup"))

##   n.lgroup1 <- unlist(lapply(n.lgroup, function(x) x[[1]]))
##   n.lgroup2 <- unlist(lapply(n.lgroup, function(x) x[[2]]))
##   n.lgroup <- list(n.lgroup1, n.lgroup2)

##   lgroup1 <- unlist(lapply(lgroup, function(x) x[[1]]))


##   if (!label)
##     lgroup2 <- names(df)
##   else
##     lgroup2 <- sapply(df, Hmisc:::label.default)

##   lgroup <- list(lgroup1, lgroup2)

##   results <- rbind.list(results)
##   colnames(results) <- "Survival"
##   results <- list(results)

##   attr(results[[1]], "lgroup") <- lgroup
##   attr(results[[1]], "n.lgroup") <- n.lgroup

##   class(results) <- "survival"
##   results
## }

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
