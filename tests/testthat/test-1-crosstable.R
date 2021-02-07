
# Init --------------------------------------------------------------------

# local_edition(3)
do_save=FALSE
# do_save=TRUE



# Unique checks -----------------------------------------------------------
test_that("Auto-testing is bad and you should feel bad.", {
  expect_warning(crosstable(mtcars2, disp, by=vs, funs=mean, test=T),
                 "Be aware that automatic global testing should only be done in an exploratory context, as it would cause extensive alpha inflation otherwise.")
})

test_that("Deprecation: moystd", {
  expect_warning(crosstable(mtcars2, disp, by=vs, funs=moystd),
                 ".*`moystd\\(\\)` is deprecated.*")
  lifecycle::expect_deprecated(crosstable(mtcars2, disp, by=vs, funs=moystd))
})





# By numeric --------------------------------------------------
test_that("numeric by numeric", {
  x1=crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp)
  expect_equal(dim(x1), c(7,4))
  expect_equal(sum(is.na(x1)), 0)

  x2=crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, test=T)
  expect_equal(dim(x2), c(7,5))
  expect_equal(sum(is.na(x2)), 0)

  crosstable(mtcars3, carb, by=disp, cor_method = "kendall")
  crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, cor_method = "pearson")
  crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, cor_method = "kendall")
  crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp, cor_method = "spearman")

  expect_warning(crosstable(mtcars2, disp, by=hp, funs=mean),
                 "`funs`  and `funs_arg` arguments will not be used if `by` is numeric.")
})

test_that("numeric+factor by numeric: ", {
  expect_warning(crosstable(mtcars3, mpg, cyl, by=disp),
                 "Cannot cross column 'cyl' \\(factor\\) by column 'disp' \\(numeric\\)")
  expect_warning(crosstable(mtcars3, is.numeric, by=disp, effect=T),
                 "Cannot cross column 'surv' \\(Surv\\) by column 'disp' \\(numeric\\)")

  x1=crosstable(mtcars3, where(is.numeric.and.not.surv), by=disp)
  expect_equal(dim(x1), c(7,4))
  expect_equal(sum(is.na(x1)), 0)
})

test_that("difftime is OK: ", {
  x1=crosstable(mtcars3, diff)
  expect_equal(dim(x1), c(4,4))
  expect_equal(sum(is.na(x1)), 0)
  x2=crosstable(mtcars3, diff, by=disp)
  expect_equal(dim(x2), c(1,4))
  expect_equal(sum(is.na(x2)), 0)
  x3=crosstable(mtcars3, diff, by=cyl)
  expect_equal(dim(x3), c(4,7))
  expect_equal(sum(is.na(x3)), 0)
})






# By numeric=factor minimum of unique levels  ---------------------------------------
test_that("by factor if numeric <= 3 levels", {
  x10=crosstable(mtcars, cyl, by=vs, total="both", margin="all")
  x10 %>% as_flextable()
  expect_identical(x10[3,4], "14 (43.75% / 100.00% / 77.78%)")
  expect_equal(dim(x10), c(4,6))
  expect_equal(sum(is.na(x10)), 0)
})


# By Nothing ---------------------------------------------------------
test_that("numeric+factor+surv by nothing", {
  x1=crosstable(mtcars3, c(am,mpg,cyl,surv))
  x1 %>% as_flextable()
  expect_equal(dim(x1), c(38,4))
  expect_equal(sum(is.na(x1)), 1)

  x2=crosstable(mtcars3, c(am,mpg,cyl,surv), times=c(0,100,200,400), followup=TRUE)
  x2 %>% as_flextable()
  expect_equal(dim(x2), c(16,4))
  expect_equal(sum(is.na(x2)), 1)
})

# By factor: showNA --------------------------------------------------------
test_that("numeric+factor+surv by factor: showNA with NA in by", {
  #am=character, mpg=numeric, cyl=numeric-factor, vs=character
  x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="no", times=c(0,100,200,400))
  x1 %>% as_flextable()
  expect_equal(dim(x1), c(14,5))
  expect_equal(sum(is.na(x1)), 1)

  x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="ifany", times=c(0,100,200,400))
  x2 %>% as_flextable()
  expect_equal(dim(x2), c(15,6))
  expect_equal(sum(is.na(x2)), 2)

  x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="always", times=c(0,100,200,400))
  x3 %>% as_flextable()
  expect_equal(dim(x3), c(16,6))
  expect_equal(sum(is.na(x3)), 2)
})

test_that("numeric+factor+surv by factor: showNA without NA in by", {
  #am=character, mpg=numeric, cyl=numeric-factor, vs=character
  x1=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="no", times=c(0,100,200,400))
  x1 %>% as_flextable()
  expect_equal(dim(x1), c(14,5))
  expect_equal(sum(is.na(x1)), 1)

  x2=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="ifany", times=c(0,100,200,400))
  x2 %>% as_flextable()
  expect_equal(dim(x2), c(16,5))
  expect_equal(sum(is.na(x2)), 1)

  x3=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="always", times=c(0,100,200,400))
  x3 %>% as_flextable()
  expect_equal(dim(x3), c(16,6))
  expect_equal(sum(is.na(x3)), 6)
  # expect_equal(sum(is.na(x3)), 1)
})

# By factor: total --------------------------------------------------------
test_that("numeric+factor+surv by factor: total", {
  x4=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="row", times=c(0,100,200,400))
  x4 %>% as_flextable()
  expect_equal(dim(x4), c(15,7))
  expect_equal(sum(is.na(x4)), 3)

  x5=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="col", times=c(0,100,200,400), followup=TRUE)
  x5 %>% as_flextable()
  expect_equal(dim(x5), c(18,6))
  expect_equal(sum(is.na(x5)), 2)

  x6=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", times=c(0,100,200,400), followup=TRUE)
  x6 %>% as_flextable()
  expect_equal(dim(x6), c(18,7))
  expect_equal(sum(is.na(x6)), 3)
})


# By factor: margin -------------------------------------------------------
test_that("numeric+factor+surv by factor: margin", {
  x6=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", times=c(0,100,200,400), followup=TRUE)
  x7=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="row", times=c(0,100,200,400))
  x7 %>% as_flextable()
  expect_identical(x7[1:3,], x6[1:3,])
  expect_identical(x7[4:7,], x6[4:7,])
  expect_identical(x7[8,4], "7 (87.50%)")
  expect_equal(dim(x7), c(17,7))
  expect_equal(sum(is.na(x7)), 3)

  x8=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="col", times=c(0,100,200,400))
  x8 %>% as_flextable()
  expect_false(identical(x8[5:8,], x6[5:8,]))
  expect_identical(x8[8,4], "7 (100.00%)")
  expect_equal(dim(x8), c(17,7))
  expect_equal(sum(is.na(x8)), 3)

  x9=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="all", times=c(0,100,200,400))
  x9 %>% as_flextable()
  expect_false(identical(x9[5:8,], x6[5:8,]))
  expect_identical(x9[8,4], "7 (35.00% / 87.50% / 100.00%)")
  expect_equal(dim(x9), c(17,7))
  expect_equal(sum(is.na(x9)), 3)

  x92=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="none", times=c(0,100,200,400))
  x92 %>% as_flextable()
  expect_false(identical(x92[5:8,], x6[5:8,]))
  expect_identical(x92[8,4], "7")
  expect_equal(dim(x92), c(17,7))
  expect_equal(sum(is.na(x92)), 3)
  
  
  x93=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="none", times=c(0,100,200,400), label=FALSE)
  x93 %>% as_flextable()
  expect_false(identical(x93[5:8,], x6[5:8,]))
  expect_identical(x93[8,4], "7")
  expect_equal(dim(x93), c(17,7))
  expect_equal(sum(is.na(x93)), 3)
})


# By dummy ---------------------------------------------------------
test_that("numeric+factor+surv by dummy", {
  x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy)
  x1 %>% as_flextable()
  expect_equal(dim(x1), c(38,4))
  expect_equal(sum(is.na(x1)), 1)

  x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy2)
  x2 %>% as_flextable()
  expect_equal(dim(x2), c(39,5))
  expect_equal(sum(is.na(x2)), 1)
})



# Functions ---------------------------------------------------------------
test_that("Functions work", {
  x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(meanCI),
                 funs_arg = list(level=0.99))
  x %>% as_flextable()
  expect_equal(dim(x), c(4,6))
  expect_equal(sum(is.na(x)), 0)
})

test_that("Function arguments work", {
  name = "test_funs_arg.Rds"
  x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(meansd, quantile),
                 funs_arg = list(dig=3, probs=c(0.25,0.75)),
                 total=T, showNA="always")
  x %>% as_flextable()
  if(is_testing()){
    expect_equivalent(x, readRDS(paste0("rds/",name)))
  } else {
    if(do_save)
      saveRDS(x, paste0("tests/testthat/rds/",name), version = 2)
    expect_equivalent(x, readRDS(paste0("tests/testthat/rds/",name)))
    waldo::compare(x, readRDS(paste0("tests/testthat/rds/",name)))
  }
  
  #TODO snapshot avec testthat3
})


test_that("One function", {
  bar=function(x, dig=1, ...) {
    return(c("MinMax" = minmax(x, dig=dig, ...), "N_NA" = nna(x)))
  }
  bar2=function(x, dig=1, ...) {
    return(c(minmax(x, dig=dig, ...), "N_NA"=nna(x)))
  }
  bar3=function(x, dig=1, ...) {
    return(c(minmax(x, dig=dig, ...), nna(x)))
  }
  
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=mean)
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs="mean")
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c("My mean" = mean))
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c("My mean" = ~mean(.x, na.rm=TRUE)))
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=cross_summary)
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c(" " = cross_summary))
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=list(" " = cross_summary))
  x = crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c("cross_summary1" = cross_summary, "bar2" = bar))
  expect_true(TRUE)
  
  expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width), 
                            funs=function(y) mean(y, na.rm=TRUE)),
                 "Anonymous functions should be named.")
  expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width), 
                            funs=~mean(.x, na.rm=TRUE)),
                 "Anonymous lambda-functions should be named.")
})


test_that("Multiple functions", {
  #avec un seul nom
  x1 = crosstable(iris2, c(Sepal.Length, Sepal.Width), 
                 funs=c(var, "meannnn"=mean))
  expect_setequal(x1$variable, c("var", "meannnn"))
  
  #avec tous les noms quand il en faut
  x2 = crosstable(iris2, c(Sepal.Length, Sepal.Width),
             funs=c(
               "moy_lambda"=~mean(.x, na.rm=TRUE), 
               "moy_fn"=function(.x){mean(.x, na.rm=TRUE)}, 
               var, 
               "moyenne"=mean
             ))
  
  expect_setequal(x2$variable, 
                  c("moy_lambda", "moy_fn", "var", "moyenne"))
  
  #avec un seul nom
  x3 = expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
             funs=c(
               ~mean(.x, na.rm=TRUE), 
               function(.x){
                 .x=.x+1
                 mean(.x, na.rm=TRUE)
               }, 
               var, 
               "moyenne"=mean
             )))
  
  expect_setequal(x3$variable, 
                  c("~mean(.x, na.rm = TRUE)", 
                    "function(.x){.x = .x + 1...}", 
                    "var", "moyenne"))

  #sans noms
  x4 = expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
             funs=c(
               ~mean(.x, na.rm=TRUE), 
               function(.x){mean(.x, na.rm=TRUE)}, 
               var, 
               mean
             )))
  
  expect_setequal(x4$variable, 
                  c("~mean(.x, na.rm = TRUE)", 
                    "function(.x){mean(.x, na.rm = TRUE)}", 
                    "var", "mean"))
})


test_that("Summary functions", {
  #date
  ct = crosstable(mtcars2, hp_date, date_format="%d/%m/%Y")
  expect_equal(ct[1,4], "22/02/2010 - 02/12/2010")
  
  #only_round
  x = mtcars2 %>% dplyr::transmute(mpg=mpg/100000)
  withr::local_options(crosstable_only_round=NULL)
  ct = crosstable(x, funs_arg=list(dig=2, zero_digits=5))
  expect_equal(ct[1,4], "0.000104 / 0.000339")
  withr::local_options(crosstable_only_round=TRUE)
  ct = crosstable(x, funs_arg=list(dig=2, zero_digits=5))
  expect_equal(ct[1,4], "0 / 0")
})




# Statistical Tests --------------------------------------------------

test_that("Statistical Tests", {
  set.seed(0)
  dummy_data = tibble(x_norm=rnorm(50,0,20), x_exp=rexp(50,60), y=rnorm(50,0,20), tmt2=rep(c("A","B"), 25), tmt3=LETTERS[1:3][sample(3,50,replace=TRUE)])

  ##CATEGORIAL

  # Fisher
  x=crosstable(mtcars3, cyl, by=vs, test=T)
  expect_equal(x$test[1], "p value: 0.0001 \n(Fisher's Exact Test for Count Data)")
  # Chi Square
  x=crosstable(iris, I(Species == "versicolor")~I(Species == "setosa"), test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Pearson's Chi-squared test)")


  ##NUMERIC

  # wilcox (exact=F)
  x=crosstable(mtcars3, disp, by=vs, test=T)
  expect_equal(x$test[1], "p value: 0.0002 \n(Wilcoxon rank sum test)")
  # wilcox (exact=T)
  if(package_version(R.version) >= package_version("4.0")) {
    x=crosstable(dummy_data, x_exp, by=tmt2, test=T)
    expect_equal(x$test[1], "p value: 0.4185 \n(Wilcoxon rank sum exact test)")
  } else {
    x=crosstable(dummy_data, x_exp, by=tmt2, test=T)
    expect_equal(x$test[1], "p value: 0.4185 \n(Wilcoxon rank sum test)")
  }
  # t.equalvar
  x = crosstable(mtcars3, mpg, by=vs, test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Two Sample t-test)")
  # t.unequalvar
  x=crosstable(mtcars3, hp, by=vs, test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Welch Two Sample t-test)")
  # a.unequalvar
  x=crosstable(mtcars3, mpg, by=cyl, test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(One-way analysis of means (not assuming equal variances))")
  # a.equalvar
  x=crosstable(dummy_data, x_norm, by=tmt3, test=T)
  expect_equal(x$test[1], "p value: 0.1626 \n(One-way analysis of means)")
  # kruskal
  x=crosstable(mtcars3, drat, by=cyl, test=T)
  expect_equal(x$test[1], "p value: 0.0017 \n(Kruskal-Wallis rank sum test)")


  ##CORRELATION

  #Pearson
  x=crosstable(mtcars3, mpg, by=disp, cor_method="pearson", test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Pearson's product-moment correlation)")
  #Kendall normal
  x=crosstable(mtcars3, mpg, by=disp, cor_method="kendall", test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Kendall's rank correlation tau, normal approximation)")
  #Kendall exact
  x=crosstable(iris, Petal.Length, by=Sepal.Length, cor_method="kendall", test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Kendall's rank correlation tau, exact test)")
  #Spearman normal
  x=crosstable(mtcars3, mpg, by=disp, cor_method="spearman", test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Spearman's rank correlation rho, normal approximation)")
  #Spearman exact
  x=crosstable(dummy_data, x_exp, by=y, cor_method="spearman", test=T)
  expect_equal(x$test[1], "p value: 0.1860 \n(Spearman's rank correlation rho, exact test)")


  ##SURVIVAL

  #Logrank
  x=crosstable(mtcars3, surv, by=am, test=T)
  expect_equal(x$test[1], "p value: <0.0001 \n(Logrank test)")
})


test_that("Linear contrasts test)", {
  my_test_args=crosstable_test_args()
  my_test_args$test_summarize = test_summarize_linear_contrasts
  x=iris %>%
    mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
    crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
  expect_equal(x$test[1], "p value: <0.0001 \n(Contrast test for linear trend)")
  expect_equal(dim(x), c(4,9))
  expect_equal(sum(is.na(x)), 0)
})

test_that("'Test' can be a variable name", {
  x=crosstable(mtcars3, test, by=vs, test=TRUE)
  expect_equal(x$.id[1], "test")
  expect_equal(dim(x), c(2,7))
})




# Effects --------------------------------------------------

test_that("Effects", {
  set.seed(0)
  dummy_data = tibble(x_norm=rnorm(50,0,20), x_exp=rexp(50,60), y=rnorm(50,0,20), tmt2=rep(c("A","B"), 25), tmt3=LETTERS[1:3][sample(3,50,replace=TRUE)])

  args = crosstable_effect_args()

  ##CATEGORIAL

  #args$effect_tabular = effect_odds_ratio (default)
  x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Odds ratio (Wald CI) (auto, vshaped vs straight): 5.25\n95%CI [0.80 to 34.43]")

  args$effect_tabular = effect_relative_risk
  x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Relative risk (Wald CI) (auto, vshaped vs straight): 2.70\n95%CI [0.94 to 15.21]")

  args$effect_tabular = effect_risk_difference
  x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Risk difference (Wald CI) (auto, vshaped minus straight): 165.82\n95%CI [-10.51 to 379.13]")



  ##NUMERIC

  #args$effect_summarize = diff_mean_auto (default)
  set.seed(1234)
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]")

  set.seed(1234)
  args$effect_summarize = diff_mean_boot
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Difference in means (bootstrap CI) (straight minus vshaped): -190.34\n95%CI [-260.78 to -119.89]")

  set.seed(1234)
  args$effect_summarize = diff_median
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Difference in medians (bootstrap CI) (): -208.90\n95%CI [-293.18 to -124.62]")


  ##SURVIVAL
  x=crosstable(mtcars3, surv, by=cyl3, effect=T, effect_args=args)
  expect_equal(x$effect[1], "Hazard ratio (Wald CI) (NA vs FALSE): 1.54\n95%CI [0.42 to 5.63]")
})

test_that("Effects never fail", {

  set.seed(1234)
  args = crosstable_effect_args()
  can_be_by = function(x) !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")

  expect_warning({
    x=names(mtcars3) %>% set_names() %>% map(~{
      if(!is_testing()) print(.x)
      if(can_be_by(mtcars3[[.x]])) {
        crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
          table %>% as.data.frame()
      }
      expect_equal(1,1, info = .x)
      return(0)
    })
  })

  set.seed(1234)
  args = crosstable_effect_args()
  args$effect_summarize = diff_mean_boot
  args$effect_tabular = effect_relative_risk
  expect_warning({
    x=names(mtcars3) %>% set_names() %>% map(~{
      if(!is_testing()) print(.x)
      if(can_be_by(mtcars3[[.x]])) {
        crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
          table %>% as.data.frame()
      }
      expect_equal(1,1, info = .x)
      return(0)
    })
  })

  set.seed(1234)
  args = crosstable_effect_args()
  args$effect_summarize = diff_median
  args$effect_tabular = effect_risk_difference
  expect_warning({
    x=names(mtcars3) %>% set_names() %>% map(~{
      # print(.x)
      if(can_be_by(mtcars3[[.x]])) {
        crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
          table %>% as.data.frame()
      }
      expect_equal(1,1)
      return(0)
    })
  })
})




# Testing everything --------------------------------------------------
test_that("Testing everything", {
  name = "test_everything.rds"
  set.seed(1234)
  x = expect_warning(
    crosstable(mtcars3, disp+hp+am+surv~vs, margin="all", total="both",
               times=c(0,100,200,400), followup=TRUE, funs_arg = list(dig=9),
               test=T, effect=T),
    "Loglik converged before variable.*")
  x %>% as_flextable()
  if(is_testing()){
    expect_equivalent(x, readRDS(paste0("rds/",name)))
  } else {
    save_name = paste0("tests/testthat/rds/",name)
    if(do_save)
      saveRDS(x, save_name, version = 2)
    expect_equivalent(x, readRDS(save_name))
    waldo::compare(x, readRDS(save_name))
  }
})





# Warnings and Errors -----------------------------------------------------
test_that("Warnings", {
  ct_warns = function() { crosstable(mtcars3, by=vs, times=c(0,100,200,400), test=T, effect=T) }

  w = capture_warnings(ct_warns())
  expect_match(w, ".*Loglik converged before variable.*", all = FALSE)
  expect_match(w, "Could not calculate crosstable effects for variables.*", all = FALSE)
})

test_that('contains both `NA` and "NA"', {
  x=mtcars3
  x$vs[18:20] = "NA"
  expect_warning(crosstable(x, c(vs), by=mpg),
                 'contains both `NA` \\(missing values\\) and "NA" \\(string\\)')
})

test_that("BY class check", {
  #no by survival
  expect_error(crosstable(mtcars3, by=surv, times=c(0,100,200,400)),
               "Crosstable only supports numeric, logical, character or factor `by` columns.*")
  #no by date
  mtcars3$dummy_posix = as.Date(mtcars3$disp, origin="2020-01-01") %>% as.POSIXct
  expect_error(crosstable(mtcars3, by=dummy_posix),
               "Crosstable only supports numeric, logical, character or factor `by` columns.*")
})

test_that("Unnamed ellipsis", {
  expect_error(
    crosstable(mtcars3, foo=vs),
    class = "rlib_error_dots_named"
  )
})



