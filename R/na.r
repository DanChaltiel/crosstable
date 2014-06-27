##' Return the number of non NA observations
##'
##' @export
##' @param x a vector
##' @param na.rm not used
##' @author David Hajage
##' @keywords univar
n <- function(x, na.rm = FALSE) {
  sum(!is.na(x))
}

##' Return the number of NA observations
##'
##' @export
##' @param x a vector
##' @param na.rm not used
##' @author David Hajage
##' @keywords univar
na <- function(x, na.rm = FALSE) {
  sum(is.na(x))
}

##' Return the min (and have formals)
##'
##' @export
##' @param x a vector
##' @param na.rm Remove NA?
##' @author David Hajage
##' @keywords univar
Min <- function(x, na.rm = FALSE)
    min(x, na.rm = na.rm)

##' Return the max (and have formals)
##'
##' @export
##' @param x a vector
##' @param na.rm Remove NA?
##' @author David Hajage
##' @keywords univar
Max <- function(x, na.rm = FALSE)
    max(x, na.rm = na.rm)
