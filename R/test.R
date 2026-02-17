
#' Default arguments for calculating and displaying tests in [crosstable()]
#'
#' This is the starting point for refining the testing algorithm used in crosstable. Users can provide their own functions for test.~.
#'
#' @param test_summarize a function of two arguments (continuous variable and grouping variable), used to compare continuous variable. Must return a list of two components: `p.value` and `method`. See [`test_summarize_auto`] or [`test_summarize_linear_contrasts`] for some examples of such functions.
#' @param test_tabular a function of two arguments (two categorical variables), used to test association between two categorical variables.  Must return a list of two components: `p.value` and `method`. See [`test_tabular_auto`] for example.
#' @param test_correlation a function of three arguments (two continuous variables plus the correlation method), used to test association between two continuous variables.  Like `cor.test`, it must return a list of at least `estimate`, `p.value`, and `method`, with also `conf.int` optionally. See [`test_correlation_auto`] for example.
#' @param test_survival a function of one argument (the formula `surv~by`), used to compare survival estimations. Must return a list of two components: `p.value` and `method`. See [`test_survival_logrank`] for example.
#' @param test_display function used to display the test result. See [`display_test`].
#' @param plim number of digits for the p value.
#' @param show_method whether to display the test name (logical).
#'
#' @return A list with test parameters
#'
#' @aliases test_args
#'
#' @seealso [`test_summarize_auto`], [`test_tabular_auto`], [`test_survival_logrank`], [`test_summarize_linear_contrasts`], [`display_test`]
#'
#' @export
#' @author Dan Chaltiel
#'
#'
#' @examples
#' library(dplyr)
#' my_test_args=crosstable_test_args()
#' my_test_args$test_summarize = test_summarize_linear_contrasts
#' iris %>%
#'   mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
#'   crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
crosstable_test_args = function(test_summarize = test_summarize_auto,
                                test_tabular = test_tabular_auto,
                                test_correlation = test_correlation_auto,
                                test_survival = test_survival_logrank,
                                test_display = display_test,
                                plim = 4,
                                show_method = TRUE){
  list(
    test_summarize = test_summarize,
    test_tabular = test_tabular,
    test_correlation = test_correlation,
    test_survival = test_survival,
    test_display = test_display,
    plim = plim,
    show_method = show_method
  )
}





#' Default function to display a test result
#'
#' @param test test
#' @param digits number of digits
#' @param method display method
#' @return a string
#' @importFrom stringr str_squish
#' @export
#' @author Dan Chaltiel
display_test = function(test, digits = 4, method = TRUE) {
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





#' test for contingency table
#'
#' Compute a chisq.test, a chisq.test with correction of continuity
#' or a fisher test as appropriate
#'
#' @param x vector
#' @param y another vector
#' @return a list with two components: p.value and method
#' @importFrom stats chisq.test
#' @export
#' @author Dan Chaltiel, David Hajage
test_tabular_auto = function(x, y) {
  tab = table(x, y)
  null_test = list(p.value=NULL, method=NULL)
  if(any(dim(tab) == 1)) {
    return(null_test)
  }
  exp = rowSums(tab)%*%t(colSums(tab))/sum(tab)
  type = if(all(exp >= 5)) "chisq" else "fisher"

  tryCatch(
    switch(type,
           chisq = chisq.test(tab, correct=FALSE),
           fisher = fisher_test(tab)),
    error=function(e) null_test
  )
}


#' Prevent `fisher.test()` from failing
#' @importFrom stats fisher.test
#' @importFrom stringr str_detect
#' @keywords internal
#' @noRd
#' @source https://stackoverflow.com/q/17052639/3888000
fisher_test = function(x, y, B=getOption("crosstable_fishertest_B", 1e5)){
  tryCatch(
    fisher.test(x, y),
    error=function(e){
      if(!str_detect(e$message, "consider using 'simulate.p.value=TRUE'")) stop(e)
      fisher.test(x, y, simulate.p.value=TRUE, B=B)
    }
  )
}


#' test for mean comparison
#'
#' Compute a oneway.test (with equal or unequal variance) or a
#' kruskal.test as appropriate.
#'
#' @param x vector
#' @param g another vector
#' @return a list with two components: p.value and method
#' @author Dan Chaltiel, David Hajage
# @importFrom nortest ad.test
#' @importFrom stats bartlett.test kruskal.test oneway.test t.test
#' @export
test_summarize_auto = function(x, g) {
  ng = table(g)
  x = as.numeric(x)
  null_test = list(p.value=NULL, method=NULL)

  if (length(ng) <= 1) {
    return(null_test)
  }

  normg = test_normality(x, g)
  k = length(ng)

  if (any(normg < 0.05)) {
    type = if (k == 2) "wilcox" else "kruskal"
  } else {
    bart = bartlett.test(x, g)$p.value
    if (bart < 0.05) {
      type = if (k == 2) "t.unequalvar" else "a.unequalvar"
    } else {
      type = if (k == 2) "t.equalvar" else "a.equalvar"
    }
  }
  test = tryCatch(
    switch(type,
           wilcox = wilcox_test2(x, g),
           kruskal = kruskal.test(x, g),
           t.unequalvar = t.test(x ~ g, var.equal = FALSE),
           t.equalvar = t.test(x ~ g, var.equal = TRUE),
           a.unequalvar = oneway.test(x ~ g, var.equal = FALSE),
           a.equalvar = oneway.test(x ~ g, var.equal = TRUE)),
    error=function(e) null_test
  )

  list(p.value = test$p.value,
       method = test$method)
}




#' test for correlation coefficients
#'
#' @param x vector
#' @param by another vector
#' @param method "pearson", "kendall", or "spearman"
#'
#' @return the correlation test with appropriate method
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom stats cor.test
#' @importFrom stringr str_detect
test_correlation_auto = function(x, by, method) {
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


#' test for survival comparison
#'
#' Compute a logrank test
#'
#' @param formula a formula
#' @return a list with two components: p.value and method
#' @author Dan Chaltiel, David Hajage
#' @export
#' @importFrom rlang check_installed
test_survival_logrank = function(formula) {
  check_installed("survival", reason="for survival data to be described using `crosstable()`.")
  null_test = list(p.value=NULL, method=NULL)
  test = tryCatch(survival::survdiff(formula),
                  error=function(e) null_test)

  list(p.value=test$pvalue, method="Logrank test")
}





#' Test for linear trend across ordered factor with contrasts
#'
#' @param x vector
#' @param y ordered factor
#'
#' @return a list with two components: p.value and method
#' @author Dan Chaltiel
#' @export
#' @importFrom rlang check_installed
#' @importFrom stats lm
#'
#' @examples
#' library(dplyr)
#' my_test_args=crosstable_test_args()
#' my_test_args$test_summarize = test_summarize_linear_contrasts
#' iris %>%
#'   mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
#'   crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
test_summarize_linear_contrasts = function(x, y){
  check_installed("gmodels", reason="for function `test_summarize_linear_contrasts()` to work.")
  x = as.numeric(x)
  stopifnot(is.ordered(y))
  levels_seq = 1:length(levels(y))
  contr = levels_seq - mean(levels_seq)  #centered on 0, step of 1
  m = lm(x ~ y)
  t = gmodels::fit.contrast(m, y, coeff=contr)
  list(p.value=t[,"Pr(>|t|)"], method="Contrast test for linear trend")
}



# Utils -------------------------------------------------------------------


#' Test for normality of `y` in subgroups `g`.
#'
#' TODO auto normality testing may require some more thought...
#'
#' @keywords internal
#' @importFrom stats na.omit shapiro.test
# @importFrom nortest ad.test
#' @noRd
test_normality = function(x, g){
  x=as.numeric(x)
  if (any(table(g) < 50)) {
    normg = tapply(x, g, function(x) {
      if(length(na.omit(x))<3 || length(unique(x))==1) return(0)
      if(length(x)>=5000) return(1)
      shapiro.test(x)$p.value
    })
  } else {
    normg = tapply(x, g, function(x) {
      if(length(unique(x))==1) return(0)
      ad.test(x)$p.value
    })
  }

  normg
}


#' Wilcoxon-MW test handling the "ties" warning
#' @keywords internal
#' @importFrom stats wilcox.test
#' @importFrom stringr str_detect
#' @noRd
wilcox_test2 = function(x, g) {
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
# wilcox_test2(dummy_data2$x_exp, dummy_data2$tmt2)$method
# #n>50, no ties, not exact test
# wilcox.test(dummy_data3$x_exp ~ dummy_data3$tmt2, correct=FALSE, exact=NULL)$method
# wilcox_test2(dummy_data3$x_exp, dummy_data3$tmt2)$method
# #n<50, ties, not exact test + WARNING
# wilcox.test(dummy_data4$x_exp ~ dummy_data4$tmt2, correct=FALSE, exact=NULL)$method
# wilcox_test2(dummy_data4$x_exp, dummy_data4$tmt2)$method
# #n>50, ties, not exact test
# wilcox.test(dummy_data5$x_exp ~ dummy_data5$tmt2, correct=FALSE, exact=NULL)$method
# wilcox_test2(dummy_data5$x_exp, dummy_data5$tmt2)$method
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
# wilcox_test2(dummy_data$x_exp, dummy_data$tmt2)
# wilcox_test2(dummy_data2$x_exp, dummy_data2$tmt2)
# wilcox_test2(dummy_data3$x_exp, dummy_data3$tmt2)
# wilcox_test2(mtcars3$disp, mtcars3$vs)




# DAN ---------------------------------------------------------------------

# nocov start

#' @importFrom stats bartlett.test kruskal.test oneway.test shapiro.test t.test wilcox.test
#' @keywords internal
#' @noRd
test_summarize_auto.dan = function (x, g) {
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

# nocov end


#TODO add CochranArmitageTest

# @importFrom DescTools CochranArmitageTest
#' @importFrom stats chisq.test cor.test
#' @keywords internal
#' @noRd
test_tabular_auto2 = function (x, y) {
  tab = table(x, y)
  if(is.ordered(x) & is.ordered(y)){
    test = cor.test(as.numeric(x), as.numeric(y), method = "spearman", exact = FALSE)
  } else if((is.ordered(x) | is.ordered(y)) & any(dim(tab)==2)){
    # check_installed("DescTools", reason="for function `CochranArmitageTest()` in `test_tabular_auto2()` to work.")
    # test = DescTools::CochranArmitageTest(tab, alternative = "two.sided")
    test = CochranArmitageTest(tab, alternative = "two.sided")
  } else{
    exp = rowSums(tab) %*% t(colSums(tab))/sum(tab)
    if (any(dim(table(x, y)) == 1))
      test = list(p.value = NULL, method = NULL)
    else if (all(exp >= 5))
      test = suppressWarnings(chisq.test(x, y, correct = FALSE))
    else test = fisher_test(x, y)
  }
  p = test$p.value
  method = test$method
  list(p.value = p, method = method)
}
