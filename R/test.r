
#' Default arguments for \code{\link{crosstable}} tests
#'
#' @return A list with testing parameters:
#' \itemize{
#'   \item \code{test.summarize} - a function of two arguments (continuous variable and grouping variable), used to compare continuous variable. Returns a list of two components : \code{p.value} and \code{method} (the test name). See [test.summarize.auto], [test.summarize.kruskal], [test.summarize.oneway.equalvar], [test.summarize.contrasts.lin], or [test.summarize.oneway.unequalvar] for some examples of such functions. Users can provide their own function.
#'   \item \code{test.survival} - a function of one argument (a formula), used to compare survival estimations. Returns the same components as created by \code{test.summarize}. See \code{\link{test.survival.logrank}}. Users can provide their own function. 
#'   \item \code{test.tabular} - a function of two arguments (two categorical variables), used to test association between two factors.  Returns the same components as created by \code{test.summarize}. See \code{\link{test.tabular.auto}} and \code{\link{test.tabular.fisher}}. Users can provide their own function.
#'   \item \code{show.test} - function used to display the test result. See \code{\link{display.test}}.
#'   \item \code{plim} - number of digits for the p value
#'   \item \code{show.method} - whether to display the test name (logical)
#' } 
#' 
#' @export
#' @author Dan Chaltiel
crosstable_test_args = function(){
  list(
    test.summarize = test.summarize.auto, 
    test.survival = test.survival.logrank, 
    test.tabular = test.tabular.auto, 
    show.test = display.test, 
    plim = 4, 
    show.method = TRUE
  )
}



#' format p values
#'
#' @param p p values
#' @param digits number of digits
#' @return formated p values
#' @export
#' @author David Hajage
plim <- function(p, digits = 4) {
  pround <- round(p, digits)
  lim <- 10^(-digits)
  ptxt <- vector("character", length(p))
  ptxt[pround < lim] <- paste("<", "0.", paste(rep("0", digits -
                                                     1), collapse = ""), "1", sep = "")
  ptxt[pround >= lim] <- formatC(pround[pround >= lim], format = "f",
                                 digits = digits)
  return(ptxt)
}

#' Display a test result
#'
#' @param test test
#' @param digits number of digits
#' @param method display method
#' @return a character string
#' @author David Hajage
#' @export
display.test <- function(test, digits = 4, method = TRUE) {
  if (all(sapply(test, is.null)))
    "No test"
  else {
    p <- plim(test$p.value, digits = digits)
    if (method)
      paste("p value: ", p, " \n(", test$method, ")", sep = "")
    else
      p
  }
}

#' test for contingency table
#'
#' Compute a chisq.test, a chisq.test with correction of continuity
#' or a fisher test as appropriate
#'
#' @param x vector
#' @param y another vector
#' @author David Hajage
#' @return a list with two componments: p.value and method
#' @importFrom stats chisq.test fisher.test
#' @export
test.tabular.auto <- function(x, y) {
  tab <- table(x, y)
  exp <- rowSums(tab)%*%t(colSums(tab))/sum(tab)
  if (any(dim(table(x, y)) == 1))
    test <- list(p.value = NULL, method = NULL)
  else if (all(exp >= 5))
    test <- suppressWarnings(chisq.test(x, y, correct = FALSE))
  ## else if (all(exp >= 3))
  ##   test <- suppressWarnings(chisq.test(x, y, correct = TRUE))
  else
    test <- fisher.test(x, y)
  
  p <- test$p.value
  method <- test$method
  list(p.value = p, method = method)
}

#' test for contingency table
#'
#' Compute a fisher test
#'
#' @param x vector
#' @param y another vector
#' @return a list with two componments: p.value and method
#' @author David Hajage
#' @importFrom stats fisher.test
#' @export
test.tabular.fisher <- function(x, y) {
  if (any(dim(table(x, y)) == 1))
    test <- list(p.value = NULL, method = NULL)
  else
    test <- fisher.test(x, y)
  
  p <- test$p.value
  method <- test$method
  list(p.value = p, method = method)
}


#' test for mean comparison
#'
#' Compute a oneway.test (with equal or unequal variance) or a
#' kruskal.test as appropriate.
#'
#' @param x vector
#' @param g another vector
#' @return a list with two componments: p.value and method
#' @author David Hajage, Dan Chaltiel
#' @importFrom nortest ad.test
#' @importFrom stats shapiro.test bartlett.test wilcox.test kruskal.test t.test oneway.test
#' @export
test.summarize.auto <- function(x, g) {
  ng <- table(g)
  
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else {
    if (any(ng < 50)) {
      normg <- tapply(x, g, function(x) shapiro.test(x)$p.value)
    } else {
      normg <- tapply(x, g, function(x) ad.test(x)$p.value)
    }
    if (any(normg < 0.05)) {
      if (length(ng) == 2) {
        type <- "wilcox"
      } else {
        type <- "kruskal"
      }
    } else {
      bartlettg <- bartlett.test(x, g)$p.value
      if (bartlettg < 0.05 & length(ng) == 2) {
        type <- "t.unequalvar"
      } else if (bartlettg < 0.05 & length(ng) > 2) {
        type <- "a.unequalvar"
      } else if (bartlettg >= 0.05 & length(ng) == 2) {
        type <- "t.equalvar"
      } else if (bartlettg >= 0.05 & length(ng) > 2) {
        type <- "a.equalvar"
      }
    }
    test <- switch(type,
                   wilcox = wilcox.test(x ~ g, correct = FALSE),
                   kruskal = kruskal.test(x, g),
                   t.unequalvar = t.test(x ~  g, var.equal = FALSE),
                   t.equalvar = t.test(x ~  g, var.equal = TRUE),
                   a.unequalvar = oneway.test(x ~  g, var.equal = FALSE),
                   a.equalvar = oneway.test(x ~ g, var.equal = TRUE))
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

#' test for mean comparison
#'
#' Compute a kruskal.test.
#'
#' @param x vector
#' @param g another vector
#' @return a list with two componments: p.value and method
#' @importFrom stats wilcox.test kruskal.test
#' @author David Hajage
#' @export
test.summarize.kruskal <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else if (length(ng) == 2) {
    test <- wilcox.test(x ~ g, correct = FALSE)
    p <- test$p.value
    method <- test$method
  } else if (length(ng) > 2) {
    test <- kruskal.test(x, g)
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

#' test for mean comparison
#'
#' Compute a oneway.test. with var.equal = TRUE
#'
#' @param x vector
#' @param g another vector
#' @return a list with two componments: p.value and method
#' @importFrom stats t.test oneway.test
#' @author David Hajage
#' @export
test.summarize.oneway.equalvar <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else if (length(ng) == 2) {
    test <- t.test(x ~ g, var.equal = TRUE)
    p <- test$p.value
    method <- test$method
  } else if (length(ng) > 2) {
    test <- oneway.test(x ~ g, var.equal = TRUE)
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

#' test for mean comparison
#'
#' Compute a oneway.test. with var.equal = FALSE
#'
#' @param x vector
#' @param g another vector
#' @return a list with two componments: p.value and method
#' @importFrom stats t.test oneway.test
#' @author David Hajage
#' @export
test.summarize.oneway.unequalvar <- function(x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else if (length(ng) == 2) {
    test <- t.test(x ~  g, var.equal = FALSE)
    p <- test$p.value
    method <- test$method
  } else if (length(ng) > 2) {
    test <- oneway.test(x ~  g, var.equal = FALSE)
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}


#' test for survival comparison
#'
#' Compute a logrank test
#'
#' @param formula a formula
#' @return a list with two componments: p.value and method
#' @author David Hajage
#' @export
#' @importFrom survival survdiff
#' @importFrom stats pchisq
test.survival.logrank <- function(formula) {
  survdiff.obj <- survdiff(formula)
  p <- 1-pchisq(survdiff.obj$chisq, length(survdiff.obj$n)-1)
  list(p.value = p, method = "Logrank test")
}


#' Test for linear trend across ordered factor with contrasts
#'
#' @param x vector
#' @param y ordered factor
#'
#' @return a list with two componments: p.value and method
#' @author Dan Chaltiel
#' @export
#' @importFrom gmodels fit.contrast
#' @importFrom stats lm
#'
#' @examples
#' library(dplyr)
#' my_test_args=crosstable_test_args()
#' my_test_args$test.summarize = test.summarize.contrasts.lin
#' iris %>%
#'   mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
#'   crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
test.summarize.contrasts.lin = function(x, y){
  stopifnot(is.ordered(y))
  levels_seq = 1:length(levels(y))
  contr = levels_seq - mean(levels_seq)  #centered on 0, step of 1
  m = lm(x ~ y)
  t = gmodels::fit.contrast(m, y, coeff=contr)
  list(p.value=t[,"Pr(>|t|)"], method="Contrast test for linear trend")
}



# DAN ---------------------------------------------------------------------

#TODO faire tout ça !

#' @importFrom stats shapiro.test bartlett.test wilcox.test kruskal.test t.test oneway.test
#' @keywords internal
#' @noRd
test.summarize.auto.dan = function (x, g) {
  ng <- table(g)
  if (length(ng) <= 1) {
    p <- NULL
    method <- NULL
  } else {
    if(length(x)<3){ #shapiro.test throws an error if n<3
      shapirog=0
    } else if(length(x)<5000){ 
      shapirog <- tapply(x, g, function(x) shapiro.test(x)$p.value)
    } else { #on large samples, shapiro.test is not relevant
      shapirog=1
    }
    
    if (any(ng < 30) | any(shapirog < 0.05)) {
      if (length(ng) == 2) {
        type <- "wilcox"
      } else {
        type <- "kruskal"
      }
    }
    else {
      bartlettg <- bartlett.test(x, g)$p.value
      if (bartlettg < 0.05 & length(ng) == 2) {
        type <- "t.unequalvar"
      }
      else if (bartlettg < 0.05 & length(ng) > 2) {
        type <- "a.unequalvar"
      }
      else if (bartlettg >= 0.05 & length(ng) == 2) {#DAN ajoute un egal au cas où bartlettg==0.05!
        type <- "t.equalvar"
      }
      else if (bartlettg >= 0.05 & length(ng) > 2) {
        type <- "a.equalvar"
      }
    }
    test <- switch(type, 
                   wilcox = wilcox.test(x ~ g, correct = FALSE), 
                   kruskal = kruskal.test(x, g), 
                   t.unequalvar = t.test(x ~ g, var.equal = FALSE), 
                   t.equalvar = t.test(x ~ g, var.equal = TRUE), 
                   a.unequalvar = oneway.test(x ~ g, var.equal = FALSE), 
                   a.equalvar = oneway.test(x ~ g, var.equal = TRUE))
    p <- test$p.value
    method <- test$method
  }
  list(p.value = p, method = method)
}

# @importFrom DescTools CochranArmitageTest
#' @importFrom stats cor.test chisq.test fisher.test
#' @keywords internal
#' @noRd
test.tabular.auto2 = function (x, y) {
  tab <- table(x, y)
  if(is.ordered(x) & is.ordered(y)){
    test <- cor.test(as.numeric(x), as.numeric(y), method = "spearman", exact = FALSE)
  } else if((is.ordered(x) | is.ordered(y)) & any(dim(tab)==2)){
    test <- CochranArmitageTest(tab, alternative = "two.sided")
  } else{
    exp <- rowSums(tab) %*% t(colSums(tab))/sum(tab)
    if (any(dim(table(x, y)) == 1)) 
      test <- list(p.value = NULL, method = NULL)
    else if (all(exp >= 5)) 
      test <- suppressWarnings(chisq.test(x, y, correct = FALSE))
    else test <- fisher.test(x, y)
  }
  p <- test$p.value
  method <- test$method
  list(p.value = p, method = method)
}
