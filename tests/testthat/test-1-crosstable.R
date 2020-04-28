
# Init --------------------------------------------------------------------

Sys.setenv(LANG = "en")
options(warn = 1)
options(stringsAsFactors = FALSE)
options(tidyselect_verbosity = "verbose")

library(survival)
library(dplyr)
# library(Hmisc)

mtcars3 = mtcars2
mtcars3$cyl[1:5] = NA
mtcars3$vs[5:12] = NA
mtcars3$cyl3 = mtcars3$cyl==3
mtcars3$cyl6 = mtcars3$cyl==6
mtcars3$dummy = "dummy"
mtcars3$dummy2 = mtcars3$dummy
mtcars3$dummy2[5:12] = NA
mtcars3$surv = Surv(mtcars3$disp, mtcars3$am=="manual")
Hmisc::label(mtcars3$surv) = "Dummy survival"


do.save=FALSE
# do.save=TRUE



# Unique checks -----------------------------------------------------------
test_that("Auto-testing is bad and you should feel bad.", {
    expect_warning(crosstable(mtcars2, disp, by=vs, funs=mean, test=T),
                   "Be aware that automatic global testing should only be done in an exploratory context, as it would cause extensive alpha inflation otherwise.")
})



# By numeric --------------------------------------------------
test_that("numeric by numeric", {
    x1=crosstable(mtcars3, is.numeric.and.not.surv, by=disp)
    expect_equal(dim(x1), c(6,4))
    expect_equal(sum(is.na(x1)), 0)
    
    x2=crosstable(mtcars3, is.numeric.and.not.surv, by=disp, test=T) 
    expect_equal(dim(x2), c(6,5))
    expect_equal(sum(is.na(x2)), 0)
    
    expect_warning(crosstable(mtcars2, disp, by=hp, funs=mean),
                   "`funs` argument will not be used if `by` is numeric.")
})

test_that("numeric+factor by numeric: ", {
    expect_warning(crosstable(mtcars3, mpg, cyl, by=disp), 
                   "Cannot cross 'cyl' \\(factor\\) by 'disp' \\(numeric\\)")
    expect_warning(crosstable(mtcars3, is.numeric, by=disp, effect=T), 
                   "Cannot cross 'surv' \\(Surv\\) by 'disp' \\(numeric\\)")
    
    x1=crosstable(mtcars3, is.numeric.and.not.surv, by=disp)
    expect_equal(dim(x1), c(6,4))
    expect_equal(sum(is.na(x1)), 0)
})


# By numeric=factor minimum of unique levels  ---------------------------------------
test_that("by factor if numeric <= 3 levels", {
    x10=crosstable(mtcars, cyl, by=vs, total="both", margin="all")
    x10 %>% ctf
    expect_identical(x10[3,4], "14 (43.75% / 100.00% / 77.78%)")
    expect_equal(dim(x10), c(4,6))
    expect_equal(sum(is.na(x10)), 0)
})


# By Nothing ---------------------------------------------------------
test_that("numeric+factor+surv by nothing", {
    x1=crosstable(mtcars3, c(am,mpg,cyl,surv))
    x1 %>% ctf
    expect_equal(dim(x1), c(38,4))
    expect_equal(sum(is.na(x1)), 1)
    
    x2=crosstable(mtcars3, c(am,mpg,cyl,surv), times=c(0,100,200,400), followup=TRUE)
    x2 %>% ctf
    expect_equal(dim(x2), c(16,4))
    expect_equal(sum(is.na(x2)), 1)
})

# By factor: showNA --------------------------------------------------------
test_that("numeric+factor+surv by factor: showNA", {
    x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="no", times=c(0,100,200,400))
    x1 %>% ctf
    expect_equal(dim(x1), c(14,5))
    expect_equal(sum(is.na(x1)), 1)
    
    x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="ifany", times=c(0,100,200,400))
    x2 %>% ctf
    expect_equal(dim(x2), c(15,6))
    expect_equal(sum(is.na(x2)), 2)
    
    x3=crosstable(mtcars2, c(am,mpg,cyl), by=vs, showNA="always")
    x3 %>% ctf
    expect_equal(dim(x3), c(11,6))
    expect_equal(sum(is.na(x3)), 0)
})


# By factor: total --------------------------------------------------------
test_that("numeric+factor+surv by factor: total", {
    x4=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="row", times=c(0,100,200,400))
    x4 %>% ctf
    expect_equal(dim(x4), c(15,7))
    expect_equal(sum(is.na(x4)), 3)
    
    x5=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="col", times=c(0,100,200,400), followup=TRUE)
    x5 %>% ctf
    expect_equal(dim(x5), c(18,6))
    expect_equal(sum(is.na(x5)), 2)
    
    x6=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", times=c(0,100,200,400), followup=TRUE)
    x6 %>% ctf
    expect_equal(dim(x6), c(18,7))
    expect_equal(sum(is.na(x6)), 3)
})


# By factor: margin -------------------------------------------------------
test_that("numeric+factor+surv by factor: margin", {
    x6=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", times=c(0,100,200,400), followup=TRUE)
    x7=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="row", times=c(0,100,200,400))
    x7 %>% ctf
    expect_identical(x7[1:3,], x6[1:3,])
    expect_identical(x7[4:7,], x6[4:7,])
    expect_identical(x7[8,4], "7 (87.50%)")
    expect_equal(dim(x7), c(17,7))
    expect_equal(sum(is.na(x7)), 3)
    
    x8=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="col", times=c(0,100,200,400))
    x8 %>% ctf
    expect_false(identical(x8[5:8,], x6[5:8,]))
    expect_identical(x8[8,4], "7 (100.00%)")
    expect_equal(dim(x8), c(17,7))
    expect_equal(sum(is.na(x8)), 3)
    
    x9=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="all", times=c(0,100,200,400))
    x9 %>% ctf
    expect_false(identical(x9[5:8,], x6[5:8,]))
    expect_identical(x9[8,4], "7 (35.00% / 87.50% / 100.00%)")
    expect_equal(dim(x9), c(17,7))
    expect_equal(sum(is.na(x9)), 3)
    
    x92=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", margin="none", times=c(0,100,200,400))
    x92 %>% ctf
    expect_false(identical(x92[5:8,], x6[5:8,]))
    expect_identical(x92[8,4], "7")
    expect_equal(dim(x92), c(17,7))
    expect_equal(sum(is.na(x92)), 3)
    
})


# By dummy ---------------------------------------------------------
test_that("numeric+factor+surv by dummy", {
    x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy)
    x1 %>% ctf
    expect_equal(dim(x1), c(38,4))
    expect_equal(sum(is.na(x1)), 1)
    
    x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy2)
    x2 %>% ctf
    expect_equal(dim(x2), c(39,5))
    expect_equal(sum(is.na(x2)), 1)
})


# Funs arguments --------------------------------------------------
test_that("Funs arguments", {
    name = "test_funs_arg.Rds"
    x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(moystd,quantile), 
                   funs_arg = list(dig=3, probs=c(0.25,0.75)), 
                   total=T, showNA="always")
    x %>% cross_to_flextable()
    if(is_testing()){
        expect_equivalent(x, readRDS(paste0("rds/",name)))
    } else {
        if(do.save)
            saveRDS(x, paste0("tests/testthat/rds/",name), version = 2)
        expect_equivalent(x, readRDS(paste0("tests/testthat/rds/",name)))
        setdiff(readRDS(paste0("tests/testthat/rds/",name)), x)
    }
})



# Tests (stats) --------------------------------------------------
# test_that("Tests (stats)", {
#     #2sample ttest, fisher, welch, chi2, logrank
#     name = "test_stats.rds"
#     x = crosstable(mtcars3, -c(carb,disp), by=vs, times=c(0,100,200,400), test=T)
#     x %>% cross_to_flextable()
#     if(is_testing()){
#         expect_equivalent(x, readRDS(paste0("rds/",name)))
#     } else {
#         if(do.save)
#             saveRDS(x, paste0("tests/testthat/rds/",name), version = 2)
#         expect_equivalent(x, readRDS(paste0("tests/testthat/rds/",name)))
#         setdiff(readRDS(paste0("tests/testthat/rds/",name)), x)
#     }
# })

test_that("Tests (linear contrasts)", {
    my_test_args=crosstable_test_args()
    my_test_args$test.summarize = test.summarize.contrasts.lin
    x=iris %>%
      mutate(Petal.Width.qt = paste0("Q", ntile(Petal.Width, 5)) %>% ordered()) %>%
      crosstable(Petal.Length ~ Petal.Width.qt, test=TRUE, test_args = my_test_args)
    expect_equal(x$test[1], "p value: <0.0001 \n(Contrast test for linear trend)")
    expect_equal(dim(x), c(4,9))
    expect_equal(sum(is.na(x)), 0)
})

# 
# # Effect --------------------------------------------------
# test_that("Effects", {
#     #mean-diff, welch, bootstrap, OR, HR
#     set.seed(1234)
#     name = "test_effects.rds"
#     x = expect_warning(crosstable(mtcars3, -c(cyl,cyl3,gear), by=vs, times=c(0,100,200,400), effect=T), 
#                        "Loglik converged before variable  2 ; coefficient may be infinite")
#     x %>% cross_to_flextable()
#     if(is_testing()){
#         expect_equivalent(x, readRDS(paste0("rds/",name)))
#     } else {
#         if(do.save)
#             saveRDS(x, paste0("tests/testthat/rds/",name), version = 2)
#         expect_equivalent(x, readRDS(paste0("tests/testthat/rds/",name)))
#         setdiff(readRDS(paste0("tests/testthat/rds/",name)), x)
#     }
# })




# Testing everything --------------------------------------------------
test_that("Testing everything", {
    name = "test_everything.rds"
    set.seed(1234)
    x = expect_warning(crosstable(mtcars3, disp+hp+am+surv~vs, margin="all", total="both", times=c(0,100,200,400), followup=TRUE, 
                   funs_arg = list(dig=9), test=T, effect=T),"cannot compute exact p-value with ties")
    x %>% cross_to_flextable()
    if(is_testing()){
        expect_equivalent(x, readRDS(paste0("rds/",name)))
    } else {
        if(do.save)
            saveRDS(x, paste0("tests/testthat/rds/",name), version = 2)
        expect_equivalent(x, readRDS(paste0("tests/testthat/rds/",name)))
        setdiff(x, readRDS(paste0("tests/testthat/rds/",name)))
    }
})





# Warnings and Errors -----------------------------------------------------
test_that("Warnings", {
    ct_warns = function() { crosstable(mtcars3, by=vs, times=c(0,100,200,400), test=T, effect=T) }
    
    w = capture_warnings(ct_warns())
    expect_match(w, "cannot compute exact p-value with ties", all = FALSE)
    expect_match(w, "Loglik converged before variable  2", all = FALSE)
    expect_match(w, "Could not calculate effect. Is there not 2 groups exactly?", all = FALSE)
})


test_that("Anything by surv = error", {
    expect_error(
        crosstable(mtcars3, by=surv, times=c(0,100,200,400)),
        "Crosstable only supports numeric, logical, character or factor `by` columns.*"
    )
})

test_that("X class check", {
    mtcars3$dummy = as.Date(mtcars3$disp, origin="2020-01-01") %>% as.POSIXct
    expect_error(
        crosstable(mtcars3, dummy, by=vs),
        "Variables of class \\[POSIXct, POSIXt\\] are not supported by crosstable\\."
    )
})

