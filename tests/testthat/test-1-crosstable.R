

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
                 class="crosstable_funs_by_warning")
})

test_that("numeric+factor by numeric: ", {
  expect_warning(crosstable(mtcars3, c(mpg, cyl), by=disp),
                 class="crosstable_wrong_col_class_by_warning")
  expect_warning(crosstable(mtcars3, where(is.numeric), by=disp, effect=T),
                 class="crosstable_wrong_col_class_by_warning")

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
  x10 = as.data.frame(x10)
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
  x6 = as.data.frame(x6)
  x7 = as.data.frame(x7)
  expect_identical(x7[1:3,], x6[1:3,])
  expect_identical(x7[4:7,], x6[4:7,])
  expect_identical(x7[8,4], "7 (87.50%)")
  expect_equal(dim(x7), c(17,7))
  expect_equal(sum(is.na(x7)), 3)

  x8=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="col", times=c(0,100,200,400))
  x8 %>% as_flextable()
  x8 = as.data.frame(x8)
  expect_false(identical(x8[5:8,], x6[5:8,]))
  expect_identical(x8[8,4], "7 (100.00%)")
  expect_equal(dim(x8), c(17,7))
  expect_equal(sum(is.na(x8)), 3)

  x9=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="all", times=c(0,100,200,400))
  x9 %>% as_flextable()
  x9 = as.data.frame(x9)
  expect_false(identical(x9[5:8,], x6[5:8,]))
  expect_identical(x9[8,4], "7 (35.00% / 87.50% / 100.00%)")
  expect_equal(dim(x9), c(17,7))
  expect_equal(sum(is.na(x9)), 3)

  x92=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="none", times=c(0,100,200,400))
  x92 %>% as_flextable()
  x92 = as.data.frame(x92)
  expect_false(identical(x92[5:8,], x6[5:8,]))
  expect_identical(x92[8,4], "7")
  expect_equal(dim(x92), c(17,7))
  expect_equal(sum(is.na(x92)), 3)
  
  
  x93=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="none", times=c(0,100,200,400), label=FALSE)
  x93 %>% as_flextable()
  x93 = as.data.frame(x93)
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


# By multiple -------------------------------------------------------------

test_that("By multiple OK", {
  x1 = crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs))
  x1 %>% as_flextable()
  expect_equal(dim(x1), c(7,27))
  x2 = crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs), showNA=FALSE)
  x2 %>% as_flextable()
  expect_equal(dim(x2), c(7,15))
  x3 = crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs), total=TRUE)
  x3 %>% as_flextable()
  expect_equal(dim(x3), c(8,28))
  x4 = crosstable(mtcars3, c(mpg, vs, cyl), by=c(am, dummy))
  x4 %>% as_flextable()
  expect_equal(dim(x4), c(11,5))
  
  x5 = crosstable(mtcars3, c(mpg, vs, cyl, dummy, surv, hp_date, qsec_posix, diff, cyl3), 
                  by=c(am, gear),
                  total=TRUE, times=c(100,200), followup=TRUE)
  x5 %>% as_flextable()
  expect_equal(dim(x5), c(34,10))
})

test_that("By multiple formula interface", {
  x1 = crosstable(mtcars3, mpg+gear~cyl+I(am=="auto")+vs, total=TRUE)
  x1 %>% as_flextable()
  expect_equal(dim(x1), c(8,28))
  
})


test_that("By multiple warnings", {
  x1 = crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, diff, qsec_posix, hp_date)) %>% 
    # expect_equal(dim(.), c(11,5)) %>% 
    expect_warning2(class="crosstable_multiby_wrong_class_warning")
  attr(x1, "obj") %>% dim() %>% expect_equal(c(11,5))
  
  x2 = crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, dummy_na, dummy_na2)) %>% 
    expect_warning2(class="crosstable_multiby_some_missing_warning")
  attr(x2, "obj") %>% dim() %>% expect_equal(c(11,5))
  
  crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, vs), test=TRUE) %>% 
    expect_warning(class="crosstable_multiby_test_warning")
  crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, vs), effect=TRUE) %>% 
    expect_warning(class="crosstable_multiby_effect_warning")
})

test_that("By multiple errors", {
  #All `by` columns have unsupported classes and were removed
  crosstable(mtcars3, c(mpg, gear, disp, carb, am), 
                  by=c(hp, surv, diff, qsec_posix, hp_date)) %>% 
    expect_error(class="crosstable_multiby_wrong_class_error")
  
  #All `by` columns have missing values only
  crosstable(mtcars3, c(mpg, gear, cyl), by=c(dummy_na, dummy_na2)) %>% 
    expect_error(class="crosstable_by_only_missing_error")
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
  ft = as_flextable(x)
  x = as.data.frame(x)
  expect_snapshot(x)
  expect_snapshot(ft)
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
  
  cross_list = list(
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=mean),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs="mean"),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c("My mean" = mean)),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=cross_summary),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c(" " = cross_summary)),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=list(" " = cross_summary)),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c("first"=function(xx){xx[1]})),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), funs=c("first"=~.x[1]))
  ) %>% map(as.data.frame)
  expect_snapshot(cross_list, error=FALSE)

  expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
                            funs=function(y) mean(y, na.rm=TRUE)),
                 class="crosstable_unnamed_anonymous_warning")
  expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
                            funs=~mean(.x, na.rm=TRUE)),
                 class="crosstable_unnamed_lambda_warning")
})

test_that("One function by", {
  bar=function(x, dig=1, ...) {
    return(c("MinMax" = minmax(x, dig=dig, ...), "N_NA" = nna(x)))
  }
  bar2=function(x, dig=1, ...) {
    return(c(minmax(x, dig=dig, ...), "N_NA"=nna(x)))
  }
  bar3=function(x, dig=1, ...) {
    return(c(minmax(x, dig=dig, ...), nna(x)))
  }
  
  cross_list = list(
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=mean),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs="mean"),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=c("My mean" = mean)),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=cross_summary),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=c(" " = cross_summary)),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=list(" " = cross_summary)),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=c("first"=function(xx){xx[1]})),
    crosstable(iris2, c(Sepal.Length, Sepal.Width), by=Species, funs=c("first"=~.x[1]))
  ) %>% map(as.data.frame)
  expect_snapshot(cross_list, error=FALSE)

  expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
                            funs=function(y) mean(y, na.rm=TRUE)),
                 class="crosstable_unnamed_anonymous_warning")
  expect_warning(crosstable(iris2, c(Sepal.Length, Sepal.Width),
                            funs=~mean(.x, na.rm=TRUE)),
                 class="crosstable_unnamed_lambda_warning")
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
                    "cross_summary1" = cross_summary
                  ))
  
  expect_setequal(x2$variable, 
                  c("moy_lambda", "moy_fn", "var", 
                    paste("cross_summary1", names(cross_summary(1))))
  )
  
  #avec un seul nom
  x3 = crosstable(iris2, c(Sepal.Length, Sepal.Width),
                  funs=c(
                    ~mean(.x, na.rm=TRUE), 
                    function(.x){
                      .x=.x+1
                      mean(.x, na.rm=TRUE)
                    }, 
                    var, 
                    "moyenne"=mean
                  )) %>% 
    expect_warning2(class="crosstable_unnamed_anonymous_warning") %>% 
    expect_warning2(class="crosstable_unnamed_lambda_warning")
  
  expect_setequal(attr(x3, "obj")$variable, 
                  c("~mean(.x, na.rm = TRUE)", 
                    "function(.x){.x = .x + 1...}", 
                    "var", "moyenne"))
  
  
  #sans noms
  x4 = crosstable(iris2, c(Sepal.Length, Sepal.Width),
                  funs=c(
                    ~mean(.x, na.rm=TRUE), 
                    function(.x){mean(.x, na.rm=TRUE)}, 
                    var, 
                    mean
                  )) %>% 
    expect_warning2(class="crosstable_unnamed_anonymous_warning") %>% 
    expect_warning2(class="crosstable_unnamed_lambda_warning")
  
  expect_setequal(attr(x4, "obj")$variable, 
                  c("~mean(.x, na.rm = TRUE)", 
                    "function(.x){mean(.x, na.rm = TRUE)}", 
                    "var", "mean"))
})


test_that("Special summary functions", {
  #date
  ct = crosstable(mtcars2, hp_date, date_format="%d/%m/%Y") %>% as.data.frame()
  expect_equal(ct[1,4], "22/02/2010 - 02/12/2010")
  
  #only_round
  x = mtcars2 %>% dplyr::transmute(mpg=mpg/100000)
  rlang::local_options(crosstable_only_round=NULL)
  ct = crosstable(x, funs_arg=list(dig=2, zero_digits=5)) %>% as.data.frame()
  expect_equal(ct[1,4], "0.000104 / 0.000339")
  rlang::local_options(crosstable_only_round=TRUE)
  ct = crosstable(x, funs_arg=list(dig=2, zero_digits=5)) %>% as.data.frame()
  expect_equal(ct[1,4], "0 / 0")
})


# Warnings ----------------------------------------------------------------


test_that('contains both `NA` and "NA"', {
  x=mtcars3
  x$vs[18:20] = "NA"
  crosstable(x, c(vs), by=mpg) %>% 
    expect_warning(class='crosstable_wrong_col_class_by_warning') %>% 
    expect_warning(class='crosstable_na_char_warning')
})

test_that('Duplicate columns are removed from `cols`', {
  rlang::local_options(crosstable_verbose_duplicate_cols=TRUE)
  crosstable(mtcars3, c(mpg, gear, am, vs), by=c(am, vs)) %>% 
    expect_warning(class="crosstable_duplicate_cols_warning")
})

test_that('Total in rows when by is NULL', {
  crosstable(mtcars2, c(mpg, wt), total="row") %>% 
    expect_warning(class='crosstable_totalrow_bynull')
  crosstable(mtcars2, mpg+wt~1, total="row") %>% 
    expect_warning(class='crosstable_totalrow_bynull')
})

test_that("Deprecation: moystd", {
  lifecycle::expect_deprecated(crosstable(mtcars2, disp, funs=moystd))
})



# Errors ------------------------------------------------------------------


test_that("BY class check", {
  #no by survival
  expect_error(crosstable(mtcars3, by=surv, times=c(0,100,200,400)),
               class="crosstable_wrong_byclass_error")
  #no by date
  mtcars3$dummy_posix = as.Date(mtcars3$disp, origin="2020-01-01") %>% as.POSIXct
  expect_error(crosstable(mtcars3, by=dummy_posix),
               class="crosstable_wrong_byclass_error")
})

test_that("Functions should return scalar", {
  crosstable(mtcars2, c(mpg, wt), by=am, funs=c("square"=function(xx){xx^2})) %>% 
    expect_error(class="crosstable_summary_not_scalar")
  crosstable(mtcars2, c(mpg, wt), by=am, funs=c(" "=function(xx){xx^2})) %>% 
    expect_error(class="crosstable_summary_not_scalar")
})

test_that("Named ellipsis", {
  expect_error(crosstable(mtcars3, foo=vs),
               class="rlib_error_dots_named")
})

