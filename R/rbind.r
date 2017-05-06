##' Rbind for list
##'
##' @param l a list
##' @author David Hajage
##' @keywords internal
rbind.list <- function(l, preserve.attr = FALSE) {
  n <- length(l)
  results <- NULL
  for (i in 1:n) {
      if (is.null(results))
          results <- l[[i]]
      else
          results <- rbind(results, l[[i]])
  }
  if (preserve.attr) {
      noms.lig <- sapply(l, function(x) attr(x, "noms.lig"))
      labs.lig <- sapply(l, function(x) attr(x, "labs.lig"))
      n.lig <- sapply(l, function(x) attr(x, "n.lig"))
      attr(results, "noms.lig") <- noms.lig
      attr(results, "labs.lig") <- labs.lig
      attr(results, "n.lig") <- n.lig
      attr(results, "noms.col") <- attr(l[[1]], "noms.col")
      attr(results, "labs.col") <- attr(l[[1]], "labs.col")
      attr(results, "n.col") <- attr(l[[1]], "n.col")
  }
  results
}

##' Rbind for crosstables
##'
##' @param ... Some crosstable objects
##' @return A \code{docx} object
##' @author David Hajage
##' @export
rbind_crosstable <- function(...) {
    args <- list(...)
    rbind.list(args, preserve.attr = TRUE)
}

##' Cbind for list
##'
##' @param l a list
##' @author David Hajage
##' @keywords internal
cbind.list <- function(l, preserve.attr = FALSE) {
  n <- length(l)
  results <- NULL
  for (i in 1:n) {
    if (is.null(results))
      results <- l[[i]]
    else
      results <- cbind(results, l[[i]])
  }
  if (preserve.attr) {
      noms.col <- sapply(l, function(x) attr(x, "noms.col"))
      labs.col <- sapply(l, function(x) attr(x, "labs.col"))
      n.col <- sapply(l, function(x) attr(x, "n.col"))
      attr(results, "noms.col") <- noms.col
      attr(results, "labs.col") <- labs.col
      attr(results, "n.col") <- n.col
      attr(results, "noms.lig") <- attr(l[[1]], "noms.lig")
      attr(results, "labs.lig") <- attr(l[[1]], "labs.lig")
      attr(results, "n.lig") <- attr(l[[1]], "n.lig")
  }
  results
}

