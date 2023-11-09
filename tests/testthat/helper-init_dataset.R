

# Options and packages ------------------------------------------------------------------------

Sys.setenv(LANGUAGE = "en")
Sys.setenv(TZ='Europe/Paris')
Sys.setenv(TESTTHAT_NCPUS =4)
options(
  encoding="UTF-8",
  # width = 200,
  # warn=0, #default, stacks
  warn=1, #immediate. =TRUE
  Ncpus=4,
  # warn=2, #error
  # warnPartialMatchArgs=TRUE,
  # warnPartialMatchAttr=TRUE,
  # warnPartialMatchDollar=TRUE,
  stringsAsFactors=FALSE,
  # conflicts.policy="depends.ok",
  dplyr.summarise.inform=FALSE,
  tidyverse.quiet=TRUE,
  tidyselect_verbosity ="verbose",#quiet or verbose
  lifecycle_verbosity="warning", #NULL, "quiet", "warning" or "error"
  # lifecycle_verbosity="verbose",
  testthat.progress.max_fails = 50
)
crosstable_options(verbosity_autotesting="quiet")
# prettycode::prettycode()


#'@source https://stackoverflow.com/a/52066708/3888000
shhh = function(expr) suppressPackageStartupMessages(suppressWarnings(expr))
shhh(library(dplyr))
shhh(library(officer))


# Dataset -------------------------------------------------------------------------------------

set.seed(1234)
mtcars3 = as_tibble(mtcars2)
mtcars3$cyl[1:5] = NA
mtcars3$vs[5:12] = NA
mtcars3$cyl3 = mtcars3$cyl==3
mtcars3$cyl6 = mtcars3$cyl==6
mtcars3$dummy = "dummy"
mtcars3$dummy_na = NA
mtcars3$dummy_na2 = NA
mtcars3$dummy_num_vs = ifelse(mtcars3$vs=="vshaped", 0, stats::rnorm(15))
mtcars3$dummy2 = mtcars3$dummy
mtcars3$dummy2[5:12] = NA
mtcars3$test = stats::rbinom(nrow(mtcars3), 1, 0.5) %>% factor(labels = c("A","B"))
mtcars3$surv = survival::Surv(mtcars3$disp, mtcars3$am=="manual") %>% set_label("Dummy survival (disp/am)")
# mtcars3$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
# mtcars3$my_posix = as.POSIXct(mtcars2$qsec*3600*24 , origin="2010-01-01") %>% set_label("Date+time")
mtcars3$diff = difftime(mtcars3$hp_date, mtcars3$qsec_posix, units="days") %>% set_label("Difftime hp_date-qsec_posix (days)")


# Functions -----------------------------------------------------------------------------------

v = utils::View
iris2names = c(SL="Sepal.Length", SW="Sepal.Width", PL="Petal.Length", PW="Petal.Width", Sp="Species")
iris2_num = iris2 %>% select(-Species)


expect_cross = function(x, xnames, byname, dim, regex){
  # expect=match.arg(expect)
  # if(expect=="nothing"){
  #     x=eval(expr, envir=caller_env())
  # }
  # else if(expect=="silent")
  #     x=expect_silent(expr)
  # else if(expect=="warning")
  #     x=expect_warning(expr, regex)
  # else
  #     x=expect_error(expr, regex)
  expect_s3_class(x, c("data.frame", "crosstable"))
  expect_equal(dim, dim(x))
  expect_equal(byname, unname(attr(x, "by")))

  if(all(xnames %in% names(iris2names)))
    expect_equal(unname(iris2names[xnames]), unique(as.character(x$.id)))
  else
    expect_equal(unname(xnames), unique(x$.id))
}

expect_cross_bak = function(expr, xnames, byname, dim, expect=c("nothing", "silent", "warning", "error"), regex){
  expect=match.arg(expect)
  if(expect=="nothing"){
    x=eval(expr, envir=caller_env())
  }
  else if(expect=="silent")
    x=expect_silent(expr)
  else if(expect=="warning")
    x=expect_warning(expr, regex)
  else
    x=expect_error(expr, regex)
  expect_s3_class(x, c("data.frame", "crosstable"))
  expect_equal(dim, dim(x))
  expect_equal(byname, unname(attr(x, "by")))

  if(all(xnames %in% names(iris2names)))
    expect_equal(unname(iris2names[xnames]), unique(as.character(x$.id)))
  else
    expect_equal(unname(xnames), unique(x$.id))
}

snapshot_review_bg = function(...){
  # brw = function(url) .Call("rs_browseURL", url, PACKAGE="(embedding)")
  brw = Sys.getenv("R_BROWSER")
  callr::r_bg(function() testthat::snapshot_review(...),
              package=TRUE,
              env = c(R_BROWSER = brw))
}

expect_warning2 = function(object, ...) {
  rtn = testthat::expect_warning(object, ...)
  if (inherits(object, "condition")) {
    attr(rtn, "object") = attr(object, "object")
  } else{
    attr(rtn, "object") = object
  }
  rtn
}

compare = function (x, y, x_arg=caller_arg(x), y_arg=caller_arg(y), len_max=Inf, ...) {
  x_arg = stringr::str_trunc(x_arg, width=len_max)
  y_arg = stringr::str_trunc(y_arg, width=len_max)
  waldo::compare(x, y, x_arg=x_arg, y_arg=y_arg, ...)
}


cli::cli_inform(c(v="Initializer {.file helper-init_dataset.R} loaded",
                  i="is_testing={is_testing()}, is_parallel={is_parallel()}"))
