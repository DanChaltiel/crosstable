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
                             # laisser comme ça.
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
