##' Remove blancks at the begining and the end
##'
##' @param x x
##' @author David Hajage
##' @keywords internal
trim <- function (x) {
    x <- sub("^ +", "", x)
    x <- sub(" +$", "", x)
    x
}

##' Concatenate functions
##'
##' @param ... functions
##' @author David Hajage
##' @keywords internal
funs2fun <- function(...) {
  fnames <- as.character(match.call()[-1])
  fs <- list(...)
  fnames2 <- names(fs)

  if (!is.null(fnames2)) {
      fnames[fnames2 != ""] <- fnames2[fnames2 != ""]
  }

  n <- length(fs)
  function(x, ...) {
    results <- NULL
    args <- list(...)
    namesargs <- names(args)
    for (i in 1:n) {
      func <- match.fun(fs[[i]])
      forms <- formals(func) # Pour min et max (et les autres
                             # primitives), il faudrait mettre
                             # 'formals(args(func))'. Le probleme est
                             # que min et max retourne le minimum de
                             # tout ce qui n'est pas 'na.rm', donc si
                             # je met un autre argument (genre probs =
                             # 1/3), min et max prennent en compte sa
                             # valeur, d'ou surprises... Je prefere
                             # laisser comme ca.
      namesforms <- names(forms)
      if (all(namesforms != "...")) {
        finalargs <- c(list(x = x), args[namesargs %in% namesforms])
      } else {
        finalargs <- c(list(x = x), args)
      }
      tmp <- do.call(func, finalargs)
      names(tmp) <- trim(paste(fnames[i], names(tmp)))
      results <- c(results, as.list(tmp))
    }
    data.frame(results, check.names = FALSE)
  }
}

## Default summary function

##' Return min and max
##'
##' @param x a numeric vector
##' @param na.rm \code{TRUE} as default
##' @param dig number of digits
#' @export
minmax <- function(x, na.rm = TRUE, dig = 2) {
    mi <- ifelse(!all(is.na(x)), round(min(x, na.rm = na.rm), dig), NA)
    ma <- ifelse(!all(is.na(x)), round(max(x, na.rm = na.rm), dig), NA)
    paste(mi, "/", ma)
}

##' Return median and IQR
##'
##' @param x a numeric vector
##' @param na.rm \code{TRUE} as default
##' @param dig number of digits
#' @export
mediqr <- function(x, na.rm = TRUE, dig = 2) {
    med <- round(median(x, na.rm = na.rm), dig)
    iqr <- round(quantile(x, probs = c(0.25, 0.75), na.rm = na.rm), dig)
    paste(med, " [", iqr[1], ";", iqr[2], "]", sep = "")
}

##' Return mean and sd
##'
##' @param x a numeric vector
##' @param na.rm \code{TRUE} as default
##' @param dig number of digits
#' @export
moystd <- function(x, na.rm = TRUE, dig = 2) {
    moy <- round(mean(x, na.rm = na.rm), dig)
    std <- round(sd(x, na.rm = na.rm), dig)
    paste(moy, " (", std, ")", sep = "")
}

##' Return n and na
##'
##' @param x a numeric vector
#' @export
nna <- function(x) {
    paste(n(x), " (", na(x), ")", sep = "")
}

##' Summarize a numeric vector
##'
##' @param x a numeric vector
##' @param na.rm \code{TRUE} as default
##' @param dig number of digits
##' @keywords internal
mysummary <- function(x, na.rm = TRUE, dig = 2) {
    return(c("Min / Max" = minmax(x, dig=dig), "Med [IQR]" = mediqr(x, dig=dig), 
             "Moy (std)" = moystd(x, dig=dig), "N (NA)" = nna(x)))
}

