
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



test_that("Margins", {
    
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