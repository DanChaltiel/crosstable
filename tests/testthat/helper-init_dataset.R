
Sys.setenv(LANG = "en")


options(stringsAsFactors = FALSE)
options(testthat.progress.max_fails = 50)
options(width = 200)

options(crosstable_verbosity_autotesting="quiet")
options(lifecycle_verbosity="warning")
# if(testthat::is_testing()){
# print(is_parallel())
if(FALSE){
    v=View
    # prettycode::prettycode()
}

# options(warn = 2)
# options(warn = 1)
# options(tidyselect_verbosity = "verbose") #quiet or verbose
# options(lifecycle_verbosity = "error") #NULL, "quiet", "warning" or "error"

library(dplyr, warn.conflicts = FALSE)
library(crosstable, warn.conflicts = FALSE)
library(stats, warn.conflicts = FALSE)
library(officer, warn.conflicts = FALSE)
# library(survival, warn.conflicts = FALSE)
# compact=crosstable::compact

set.seed(1234)
mtcars3 = as_tibble(mtcars2)
mtcars3$cyl[1:5] = NA
mtcars3$vs[5:12] = NA
mtcars3$cyl3 = mtcars3$cyl==3
mtcars3$cyl6 = mtcars3$cyl==6
mtcars3$dummy = "dummy"
mtcars3$dummy_na = NA
mtcars3$dummy_na2 = NA
mtcars3$dummy_num_vs = ifelse(mtcars3$vs=="vshaped", 0, rnorm(15))
mtcars3$dummy2 = mtcars3$dummy
mtcars3$dummy2[5:12] = NA
mtcars3$test = rbinom(nrow(mtcars3), 1, 0.5) %>% factor(labels = c("A","B"))
mtcars3$surv = survival::Surv(mtcars3$disp, mtcars3$am=="manual") %>% set_label("Dummy survival (disp/am)")
# mtcars3$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
# mtcars3$my_posix = as.POSIXct(mtcars2$qsec*3600*24 , origin="2010-01-01") %>% set_label("Date+time")
mtcars3$diff = difftime(mtcars3$hp_date, mtcars3$qsec_posix, units="days") %>% set_label("Difftime hp_date-qsec_posix (days)")



# Functions ---------------------------------------------------------------
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




expect_warning2 = function(object, ...) {
    rtn = testthat::expect_warning(object, ...)
    if (inherits(object, "condition")) {
        attr(rtn, "object") = attr(object, "object")
    } else{
        attr(rtn, "object") = object
    }
    rtn
}

print('Helper loaded')