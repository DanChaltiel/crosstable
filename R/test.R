
#' Default arguments for tests in [crosstable]
#' 
#' This is the starting point for refining the testing algorithm used in crosstable. Users can provide their own functions for test.~.
#'
#' @return A list with testing parameters:
#' \itemize{
#'   \item `test.summarize` - a function of two arguments (continuous variable and grouping variable), used to compare continuous variable. Must return a list of two components : `p.value` and `method`. See [`test.summarize.auto`] or [`test.summarize.linear.contrasts`] for some examples of such functions.
#'   
#'   \item `test.tabular` - a function of two arguments (two categorical variables), used to test association between two categorical variables.  Must return a list of two components : `p.value` and `method`. See [`test.tabular.auto`] for example.
#'   
#'   \item `test.correlation` - a function of three arguments (two continuous variables plus the correlation method), used to test association between two continuous variables.  Like `cor.test`, it must return a list of at least `estimate`, `p.value`, and `method`, with also `conf.int` optionnaly. See [`test.correlation.auto`] for example.
#'   
#'   \item `test.survival` - a function of one argument (the formula `surv~by`), used to compare survival estimations. Must return a list of two components : `p.value` and `method`. See [`test.survival.logrank`] for example.
#'   
#'   \item `show.test` - function used to display the test result. See [`display.test`].
#'   \item `plim` - number of digits for the p value
#'   \item `show.method` - whether to display the test name (logical)
#' } 
#' 
#' @aliases test_args
#' 
#' @seealso [`test.summarize.auto`], [`test.tabular.auto`], [`test.survival.logrank`], [`test.summarize.linear.contrasts`], [`display.test`]
#' 
#' @export
#' @author Dan Chaltiel
#' 
#' 
#' @examples
#' library(dplyr)
#' my_test_args=crosstable_test_args()
#' my_test_args$test.summarize = test.summarize.linear.contrasts
#' iris %>%
#'   mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
#'   crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
crosstable_test_args = function(){
  list(
    test.summarize = test.summarize.auto, 
    test.tabular = test.tabular.auto, 
    test.correlation = test.correlation.auto, 
    test.survival = test.survival.logrank, 
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
plim = function(p, digits = 4) {
  pround = round(p, digits)
  lim = 10^(-digits)
  ptxt = vector("character", length(p))
  ptxt[pround < lim] = paste("<", "0.", paste(rep("0", digits -
                                                    1), collapse = ""), "1", sep = "")
  ptxt[pround >= lim] = formatC(pround[pround >= lim], format = "f",
                                digits = digits)
  return(ptxt)
}

#' Display a test result
#'
#' @param test test
#' @param digits number of digits
#' @param method display method
#' @importFrom stringr str_squish
#' @export
display.test = function(test, digits = 4, method = TRUE) {
  if (all(sapply(test, is.null)))
    "No test"
  else {
    p = plim(test$p.value, digits = digits)
    if (method)
      paste0("p value: ", p, " \n(", str_squish(test$method), ")")
    else
      p
  }
}



#' test for correlation coefficients
#'
#' @param x vector
#' @param by another vector
#' @param method "pearson", "kendall", or "spearman"
#'
#' @return the correlation test with appropriate method
#' @importFrom stringr str_detect
#' @export
test.correlation.auto = function(x, by, method) {
  exact=TRUE
  ct = withCallingHandlers(
    tryCatch(cor.test(x, by, method = method)), 
    warning=function(m) {
      if(str_detect(conditionMessage(m), "exact p-value"))
        exact<<-FALSE 
      invokeRestart("muffleWarning")
    }
  )
  
  if(method %in% c("kendall", "spearman")){
    if(!exact){
      ct$method = paste0(ct$method, ", normal approximation")
    } else {
      ct$method = paste0(ct$method, ", exact test")
    }
  }
  ct
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
test.tabular.auto = function(x, y) {
  tab = table(x, y)
  exp = rowSums(tab)%*%t(colSums(tab))/sum(tab)
  if (any(dim(table(x, y)) == 1))
    test = list(p.value = NULL, method = NULL)
  else if (all(exp >= 5))
    test = chisq.test(x, y, correct = FALSE)
  else
    test = fisher.test(x, y)
  
  p = test$p.value
  method = test$method
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
#' @importFrom stats shapiro.test bartlett.test kruskal.test t.test oneway.test
#' @export
test.summarize.auto = function(x, g) {
  ng = table(g)
  
  if (length(ng) <= 1) {
    return(list(p.value=NULL, method=NULL))
  } 
  
  if (any(ng < 50)) {
    normg = tapply(x, g, function(x) {
      if(length(unique(x))==1) return(0)
      shapiro.test(x)$p.value
    })
  } else {
    normg = tapply(x, g, function(x) {
      if(length(unique(x))==1) return(0)
      ad.test(x)$p.value
    })
  }
  
  if (any(normg < 0.05)) {
    if (length(ng) == 2) {
      type = "wilcox"
    } else {
      type = "kruskal"
    }
  } else {
    bartlettg = bartlett.test(x, g)$p.value
    if (bartlettg < 0.05 & length(ng) == 2) {
      type = "t.unequalvar"
      test = t.test(x ~ g, var.equal = FALSE)
    } else if (bartlettg < 0.05 & length(ng) > 2) {
      type = "a.unequalvar"
      test = oneway.test(x ~ g, var.equal = FALSE)
    } else if (bartlettg >= 0.05 & length(ng) == 2) {
      type = "t.equalvar"
      test = t.test(x ~ g, var.equal = TRUE)
    } else if (bartlettg >= 0.05 & length(ng) > 2) {
      type = "a.equalvar"
      test = oneway.test(x ~ g, var.equal = TRUE)
    }
  }
  test = switch(type,
                wilcox = wilcox.test2(x, g),
                kruskal = kruskal.test(x, g),
                t.unequalvar = t.test(x ~ g, var.equal = FALSE),
                t.equalvar = t.test(x ~ g, var.equal = TRUE),
                a.unequalvar = oneway.test(x ~ g, var.equal = FALSE),
                a.equalvar = oneway.test(x ~ g, var.equal = TRUE))
  
  
  list(p.value = test$p.value, 
       method = test$method)
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
test.survival.logrank = function(formula) {
  survdiff.obj = survdiff(formula)
  p = 1-pchisq(survdiff.obj$chisq, length(survdiff.obj$n)-1)
  list(p.value = p, method = "Logrank test")
}





#' Wilcoxon-MW test handling the "ties" warning
#' @keywords internal
#' @importFrom stats wilcox.test
#' @importFrom stringr str_detect
#' @noRd
wilcox.test2 = function(x, g) {
  test = withCallingHandlers(
    tryCatch(wilcox.test(x ~ g, correct = FALSE, exact=NULL)), 
    warning=function(m) {
      if(!str_detect(conditionMessage(m), "exact p-value")) {
        warning(m)
      }
      invokeRestart("muffleWarning")
    }
  )
  test
}

# dummy_data2=dummy_data[c(1:48),]
# dummy_data3=dummy_data[c(1:150),] %>% rbind(list(25,0.003,29,"A","C","A"))
# dummy_data4=dummy_data[c(1:48,25),]
# dummy_data5=dummy_data[c(1:150,25),] %>% rbind(list(25,0.003,29,"A","C","A"))
# 
# #n<50, no ties, exact test
# wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=FALSE, exact=NULL)$method
# wilcox.test2(dummy_data2$x_exp, dummy_data2$tmt2)$method
# #n>50, no ties, not exact test
# wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=FALSE, exact=NULL)$method
# wilcox.test2(dummy_data3$x_exp, dummy_data3$tmt2)$method
# #n<50, ties, not exact test + WARNING
# wilcox.test(dummy_data4$x_exp ~ dummy_data4$tmt2, correct=FALSE, exact=NULL)$method
# wilcox.test2(dummy_data4$x_exp, dummy_data4$tmt2)$method
# #n>50, ties, not exact test
# wilcox.test(dummy_data5$x_exp ~ dummy_data5$tmt2, correct=FALSE, exact=NULL)$method
# wilcox.test2(dummy_data5$x_exp, dummy_data5$tmt2)$method
# 
# # wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=TRUE, exact=NULL)$method
# # wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=TRUE, exact=TRUE)$method
# # wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=TRUE, exact=FALSE)$method
# # wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=FALSE, exact=NULL)$method
# # wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=FALSE, exact=TRUE)$method
# # wilcox.test(dummy_data2$x_exp ~ dummy_data2$tmt2, correct=FALSE, exact=FALSE)$method
# 
# # wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=TRUE, exact=NULL)$method
# # wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=TRUE, exact=TRUE)$method
# # wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=TRUE, exact=FALSE)$method
# # wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=FALSE, exact=NULL)$method
# # wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=FALSE, exact=TRUE)$method
# # wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=FALSE, exact=FALSE)$method
# 
# wilcox.test(mtcars3$disp ~ mtcars3$vs, correct=TRUE, exact=TRUE)$method
# wilcox.test(mtcars3$disp ~ mtcars3$vs, correct=TRUE, exact=FALSE)$method
# wilcox.test(mtcars3$disp ~ mtcars3$vs, correct=FALSE, exact=TRUE)$method
# wilcox.test(mtcars3$disp ~ mtcars3$vs, correct=FALSE, exact=FALSE)$method
# 
# wilcox.test2(dummy_data$x_exp, dummy_data$tmt2)
# wilcox.test2(dummy_data2$x_exp, dummy_data2$tmt2)
# wilcox.test2(dummy_data3$x_exp, dummy_data3$tmt2)
# wilcox.test2(mtcars3$disp, mtcars3$vs)

#' Test for linear trend across ordered factor with contrasts
#'
#' @param x vector
#' @param y ordered factor
#'
#' @return a list with two componments: p.value and method
#' @author Dan Chaltiel
#' @export
#' @importFrom stats lm
#'
#' @examples
#' library(dplyr)
#' my_test_args=crosstable_test_args()
#' my_test_args$test.summarize = test.summarize.linear.contrasts
#' iris %>%
#'   mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
#'   crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
test.summarize.linear.contrasts = function(x, y){
  if(!requireNamespace("gmodels", quietly=TRUE))
    stop("This function needs the package `gmodels` to run")
  stopifnot(is.ordered(y))
  levels_seq = 1:length(levels(y))
  contr = levels_seq - mean(levels_seq)  #centered on 0, step of 1
  m = lm(x ~ y)
  t = gmodels::fit.contrast(m, y, coeff=contr)
  list(p.value=t[,"Pr(>|t|)"], method="Contrast test for linear trend")
}



# DAN ---------------------------------------------------------------------

# nocov start

#' @importFrom stats shapiro.test bartlett.test wilcox.test kruskal.test t.test oneway.test
#' @keywords internal
#' @noRd
test.summarize.auto.dan = function (x, g) {
  ng = table(g)
  if (length(ng) <= 1) {
    p = NULL
    method = NULL
  } else {
    if(length(x)<3){ #shapiro.test throws an error if n<3
      shapirog=0
    } else if(length(x)<5000){ 
      shapirog = tapply(x, g, function(x) shapiro.test(x)$p.value)
    } else { #on large samples, shapiro.test is not relevant
      shapirog=1
    }
    
    if (any(ng < 30) | any(shapirog < 0.05)) {
      if (length(ng) == 2) {
        type = "wilcox"
      } else {
        type = "kruskal"
      }
    }
    else {
      bartlettg = bartlett.test(x, g)$p.value
      if (bartlettg < 0.05 & length(ng) == 2) {
        type = "t.unequalvar"
      }
      else if (bartlettg < 0.05 & length(ng) > 2) {
        type = "a.unequalvar"
      }
      else if (bartlettg >= 0.05 & length(ng) == 2) {
        type = "t.equalvar"
      }
      else if (bartlettg >= 0.05 & length(ng) > 2) {
        type = "a.equalvar"
      }
    }
    test = switch(type, 
                  wilcox = wilcox.test(x ~ g, correct = FALSE), 
                  kruskal = kruskal.test(x, g), 
                  t.unequalvar = t.test(x ~ g, var.equal = FALSE), 
                  t.equalvar = t.test(x ~ g, var.equal = TRUE), 
                  a.unequalvar = oneway.test(x ~ g, var.equal = FALSE), 
                  a.equalvar = oneway.test(x ~ g, var.equal = TRUE))
    p = test$p.value
    method = test$method
  }
  list(p.value = p, method = method)
}



#TODO add CochranArmitageTest

# @importFrom DescTools CochranArmitageTest
#' @importFrom stats cor.test chisq.test fisher.test
#' @keywords internal
#' @noRd
test.tabular.auto2 = function (x, y) {
  tab = table(x, y)
  if(is.ordered(x) & is.ordered(y)){
    test = cor.test(as.numeric(x), as.numeric(y), method = "spearman", exact = FALSE)
  } else if((is.ordered(x) | is.ordered(y)) & any(dim(tab)==2)){
    test = DescTools::CochranArmitageTest(tab, alternative = "two.sided")
  } else{
    exp = rowSums(tab) %*% t(colSums(tab))/sum(tab)
    if (any(dim(table(x, y)) == 1)) 
      test = list(p.value = NULL, method = NULL)
    else if (all(exp >= 5)) 
      test = suppressWarnings(chisq.test(x, y, correct = FALSE))
    else test = fisher.test(x, y)
  }
  p = test$p.value
  method = test$method
  list(p.value = p, method = method)
}
# nocov end
