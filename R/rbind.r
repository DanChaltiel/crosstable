##' Rbind for list
##'
##' @param l a list
##' @author David Hajage
##' @keywords internal
rbind.list <- function(l) {
  n <- length(l)
  results <- NULL
  for (i in 1:n) {
    results <- rbind(results, l[[i]])
  }
  results
}

##' Cbind for list
##'
##' @param l a list
##' @author David Hajage
##' @keywords internal
cbind.list <- function(l) {
  n <- length(l)
  results <- NULL
  for (i in 1:n) {
    if (is.null(results))
      results <- l[[i]]
    else
      results <- cbind(results, l[[i]])
  }
  results
}
