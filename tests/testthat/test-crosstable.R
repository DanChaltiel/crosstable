


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


# Crossing difftime -------------------------------------------------------

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


# Warnings ----------------------------------------------------------------

test_that("Warn: numeric+factor by numeric: ", {
  expect_warning(crosstable(mtcars3, c(mpg, cyl), by=disp),
                 class="crosstable_wrong_col_class_by_warning")
  expect_warning(crosstable(mtcars3, c(mpg, surv), by=disp, effect=T),
                 class="crosstable_wrong_col_class_by_warning")
  
})

test_that('Warn: contains both `NA` and "NA"', {
  x=mtcars3
  x$vs[18:20] = "NA"
  crosstable(x, c(vs), by=mpg) %>% 
    expect_warning(class='crosstable_wrong_col_class_by_warning') %>% 
    expect_warning(class='crosstable_na_char_warning')
})

test_that('Warn: Duplicate columns are removed from `cols`', {
  rlang::local_options(crosstable_verbosity_duplicate_cols="verbose")
  crosstable(mtcars3, c(mpg, gear, am, vs), by=c(am, vs)) %>% 
    expect_warning(class="crosstable_duplicate_cols_warning")
})

test_that('Warn: Total in rows when by is NULL', {
  crosstable(mtcars2, c(mpg, wt), total="row") %>% 
    expect_warning(class='crosstable_totalrow_bynull')
  crosstable(mtcars2, mpg+wt~1, total="row") %>% 
    expect_warning(class='crosstable_totalrow_bynull')
})

test_that("Warn: Deprecation: moystd", {
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
  crosstable(mtcars2, c(mpg, wt), by=am, funs=c("square"=function(xx) xx^2)) %>% 
    expect_error(class="crosstable_summary_not_scalar")
  crosstable(mtcars2, c(mpg, wt), by=am, funs=c(" "=function(xx) xx^2)) %>% 
    expect_error(class="crosstable_summary_not_scalar")
})

test_that("Named ellipsis", {
  expect_error(crosstable(mtcars3, foo=vs),
               class="rlib_error_dots_named")
})

