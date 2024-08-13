


# By Nothing ---------------------------------------------------------
test_that("numeric+factor+surv by nothing", {
  x1=crosstable(mtcars3, c(am,mpg,cyl,surv))
  x1 %>% as_flextable()
  expect_true(is.crosstable(x1))
  expect_equal(dim(x1), c(38,4))
  expect_equal(sum(is.na(x1)), 1)

  x2=crosstable(mtcars3, c(am,mpg,cyl,surv), times=c(0,100,200,400), followup=TRUE)
  x2 %>% as_flextable()
  expect_true(is.crosstable(x2))
  expect_equal(dim(x2), c(16,4))
  expect_equal(sum(is.na(x2)), 1)
})


test_that('Contains both `NA` and "NA"', {
  # "'NA'" is overkill to test
  x = data.frame(a = as_factor(c("zzz", "zzz", "NA", "NA", NA, NA, "aaa", "aaa")),
                 b = rep(1:2, 4))
  x$a %>% levels

  crosstable(x, a) %>%
    pull(variable) %>%
    expect_identical(c("zzz", "aaa", "\"NA\"", "NA"))

  crosstable(x, a, by=b) %>%
    pull(variable) %>%
    expect_identical(c("zzz", "aaa", "\"NA\"", "NA"))
})


# Arguments -----------------------------------------------------------------------------------


test_that('`remove_zero_percent` works', {
  crosstable_options(remove_zero_percent=FALSE, .local=TRUE)
  a = crosstable(mtcars2, cyl, by=vs)
  expect_false(any(a=="0"))
  b = crosstable(mtcars2, cyl, by=vs, remove_zero_percent=TRUE)
  expect_true(any(b=="0"))
})

# Crossing difftime -------------------------------------------------------

test_that("difftime is OK", {
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

test_that("difftime is OK also", {
  set.seed(42)
  x1 = tibble(
    h = rpois(10, 10), m = rpois(10, 30),
    hm = lubridate::hm(paste0(h,":",m)),
    hms = hms::hms(hours=h, minutes=m),
    hms2 = hms::hms(hours=h*20, minutes=m),
  )
  class(x1$hm) #Period
  class(x1$hms)#hms + difftime
  ct = crosstable(x1, starts_with("hm"))
  expect_equal(ct$value[2], "11H 28M 30S [9H 34M 15S;14H 3M 15S]")
})


# Warnings ----------------------------------------------------------------

test_that("Warn: numeric+factor by numeric: ", {
  expect_warning(crosstable(mtcars3, c(mpg, cyl), by=disp),
                 class="crosstable_wrong_col_class_by_warning")
  expect_warning(crosstable(mtcars3, c(mpg, surv), by=disp, effect=T),
                 class="crosstable_wrong_col_class_by_warning")
  expect_warning(crosstable(mtcars3, c(mpg, cyl, surv), by=disp, effect=T),
                 class="crosstable_wrong_col_class_by_warning")

})


test_that('Warn: contains only `NA`', {
  crosstable(mtcars3, dummy_na) %>%
    expect_warning(class='crosstable_all_na_warning') %>%
    expect_warning(class='crosstable_empty_warning')
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
  expect_error(crosstable(mtcars3, vs, by=surv, times=c(0,100,200,400)),
               class="crosstable_wrong_byclass_error")
  #no by date
  mtcars3$dummy_posix = as.Date(mtcars3$disp, origin="2020-01-01") %>% as.POSIXct
  expect_error(crosstable(mtcars3, vs, by=dummy_posix),
               class="crosstable_wrong_byclass_error")
})

test_that("Functions should return scalar", {
  crosstable(mtcars2, c(mpg, wt), by=am, funs=c("square"=function(xx) xx^2)) %>%
    expect_error(class="crosstable_summary_not_scalar")
  crosstable(mtcars2, c(mpg, wt), by=am, funs=c(" "=function(xx) xx^2)) %>%
    expect_error(class="crosstable_summary_not_scalar")
})

test_that("Named ellipsis", {
  expect_snapshot_error(crosstable(mtcars3, foo=vs, bar=am))
})

