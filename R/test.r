##' format p values
##'
##' @param p p values
##' @param digits number of digits
##' @return formated p values
##' @export
##' @author David Hajage
plim <- function (p, digits = 4) {
    pround <- round(p, digits)
    lim <- 10^(-digits)
    ptxt <- vector("character", length(p))
    ptxt[pround < lim] <- paste("<", "0.", paste(rep("0", digits -
        1), collapse = ""), "1", sep = "")
    ptxt[pround >= lim] <- formatC(pround[pround >= lim], format = "f",
        digits = digits)
    return(ptxt)
}

##' Display a test result
##'
##' @param test test
##' @param digits number of digits
##' @param method display method
##' @return a character string
##' @author David Hajage
##' @export
display.test <- function(test, digits = 4, method = TRUE) {
  if (all(sapply(test, is.null)))
    "No test"
  else {
    p <- plim(test$p.value, digits = digits)
    if (method)
    paste("p value: ", p, " (", test$method, ")", sep = "")
  else
    paste("p value: ", p, sep = "")
  }
}

##' test for contingency table
##'
##' Compute a chisq.test, a chisq.test with correction of continuity
##' or a fisher test as appropriate
##'
##' @param x vector
##' @param y another vector
##' @author David Hajage
##' @return a list with two componments: p.value and method
##' @export
test.tabular.auto <- function(x, y) {
  tab <- table(x, y)
  exp <- rowSums(tab)%*%t(colSums(tab))/sum(tab)
  if (any(dim(table(x, y)) == 1))
    test <- list(p.value = NULL, method = NULL)
  else if (all(exp >= 5))
    test <- suppressWarnings(chisq.test(x, y, correct = FALSE))
  else if (all(exp >= 3))
    test <- suppressWarnings(chisq.test(x, y, correct = TRUE))
  else
    test <- fisher.test(x, y)

  p <- test$p.value
  method <- test$method
  list(p.value = p, method = method)
}

##' test for contingency table
##'
##' Compute a fisher test
##'
##' @param x vector
##' @param y another vector
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
test.tabular.fisher <- function(x, y) {
  if (any(dim(table(x, y)) == 1))
    test <- list(p.value = NULL, method = NULL)
  else
    test <- fisher.test(x, y)

  p <- test$p.value
  method <- test$method
  list(p.value = p, method = method)
}

##' test for mean comparison
##'
##' Compute a oneway.test (with equal or unequal variance) or a
##' kruskal.test as appropriate.
##'
##' @param x vector
##' @param g another vector
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
test.summarize.auto <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else {
    shapirog <- tapply(x, g, function(x) shapiro.test(x)$p.value)
    if (any(ng < 30) | any(shapirog < 0.05)) {
      type <- "kruskal"
    } else {
      bartlettg <- bartlett.test(x, g)$p.value
      if (bartlettg < 0.05) {
        type <- "unequalvar"
      } else {
        type <- "equalvar"
      }
    }
    test <- switch(type,
                   kruskal = kruskal.test(x, g),
                   unequalvar = oneway.test(x ~  g, var.equal = FALSE),
                   equalvar = oneway.test(x ~ g, var.equal = TRUE))
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

##' test for mean comparison
##'
##' Compute a kruskal.test.
##'
##' @param x vector
##' @param g another vector
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
test.summarize.kruskal <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else {
    test <- kruskal.test(x, g)
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

##' test for mean comparison
##'
##' Compute a oneway.test. with var.equal = TRUE
##'
##' @param x vector
##' @param g another vector
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
test.summarize.oneway.equalvar <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else {
    test <- oneway.test(x ~  g, var.equal = TRUE)
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

##' test for mean comparison
##'
##' Compute a oneway.test. with var.equal = FALSE
##'
##' @param x vector
##' @param g another vector
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
test.summarize.oneway.unequalvar <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else {
    test <- oneway.test(x ~  g, var.equal = FALSE)
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}


##' test for survival comparison
##'
##' Compute a logrank test
##'
##' @param formula a formula
##' @return a list with two componments: p.value and method
##' @author David Hajage
##' @export
##' @import survival 
test.survival.logrank <- function(formula) {
  survdiff.obj <- survdiff(formula)
  p <- 1-pchisq(survdiff.obj$chisq, length(survdiff.obj$n)-1)
  list(p.value = p, method = "Logrank test")
}
