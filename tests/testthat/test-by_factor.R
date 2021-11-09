
# colonnes test : c(am,mpg,cyl,surv)
# am = factor
# mpg = numeric
# cyl = numeric as factor
# surv = survival


# showNA ------------------------------------------------------------------


test_that("showNA with NA in by", {
    expect_true(anyNA(mtcars3$vs))
    expect_snapshot({
        x0=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, times=c(0,100,200,400))
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="no", times=c(0,100,200,400))
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="ifany", times=c(0,100,200,400))
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, showNA="always", times=c(0,100,200,400))
        as.data.frame(x3)
        as_flextable(x3)
    })
})

test_that("showNA without NA in by", {
    expect_false(anyNA(mtcars3$am))
    expect_snapshot({
        x0=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, times=c(0,100,200,400))
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="no", times=c(0,100,200,400))
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="ifany", times=c(0,100,200,400))
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(vs,mpg,cyl,surv), by=am, showNA="always", times=c(0,100,200,400))
        as.data.frame(x3)
        as_flextable(x3)
    }) 
})


# total -------------------------------------------------------------------

test_that("total", {
    expect_snapshot({
        x0=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, times=c(0,100,200,400))
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="none", times=c(0,100,200,400))
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="row", times=c(0,100,200,400))
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="col", times=c(0,100,200,400))
        as.data.frame(x3)
        as_flextable(x3)
        x4=crosstable(mtcars3, c(am,mpg,cyl,surv), by=vs, total="both", times=c(0,100,200,400))
        as.data.frame(x4)
        as_flextable(x4)
    })
})


# Margins -----------------------------------------------------------------

test_that("Margins without totals", {
    
    expect_snapshot({
        x0=crosstable(mtcars3, c(am, cyl), by=vs, total="none")
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="row")
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="col")
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="cell")
        as.data.frame(x3)
        as_flextable(x3)
        x4=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="none")
        as.data.frame(x4)
        as_flextable(x4)
        x5=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin="all")
        as.data.frame(x5)
        as_flextable(x5)
        x6=crosstable(mtcars3, c(am, cyl), by=vs, total="none", margin=1:2)
        as.data.frame(x6)
        as_flextable(x6)
    }) 
})

test_that("Margins with totals", {
    
    expect_snapshot({
        x0=crosstable(mtcars3, c(am, cyl), by=vs, total="both")
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="row")
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="col")
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="cell")
        as.data.frame(x3)
        as_flextable(x3)
        x4=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="none")
        as.data.frame(x4)
        as_flextable(x4)
        x5=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin="all")
        as.data.frame(x5)
        as_flextable(x5)
        x6=crosstable(mtcars3, c(am, cyl), by=vs, total="both", margin=1:2)
        as.data.frame(x6)
        as_flextable(x6)
    }) 
})



# Percent pattern ---------------------------------------------------------


test_that("Percent pattern", {
    
    expect_snapshot({
        x0=crosstable(mtcars3, cyl, 
                      percent_pattern="N={n} \nrow={p_row}, col={p_col}")
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, cyl, total=TRUE, 
                      percent_pattern="N={n} \np[95%CI] = {p_col} [{p_col_inf}; {p_col_sup}]")
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, cyl, showNA="always", 
                      percent_pattern="N={n} \nrow={p_row}, col={p_col}")
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(mpg, vs, cyl), by=c(am, dummy))
        as.data.frame(x3)
        as_flextable(x3)
    }) 
})

# By dummy ---------------------------------------------------------
test_that("By dummy", {
    
    expect_snapshot({
        x0=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy)
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy, showNA=TRUE)
        as.data.frame(x1)
        as_flextable(x1)
    }) 
    expect_snapshot({#dummy with NA
        x2=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy2)
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(am,mpg,cyl,surv), by=dummy2, showNA=FALSE)
        as.data.frame(x3)
        as_flextable(x3)
    }) 
})


# By multiple -------------------------------------------------------------

test_that("By multiple", {
    
    expect_snapshot({
        x0=crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs))
        as.data.frame(x0)
        as_flextable(x0)
        x1=crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs), showNA=FALSE)
        as.data.frame(x1)
        as_flextable(x1)
        x2=crosstable(mtcars3, c(mpg, gear), by=c(cyl, am, vs), total=TRUE)
        as.data.frame(x2)
        as_flextable(x2)
        x3=crosstable(mtcars3, c(mpg, vs, cyl), by=c(am, dummy))
        as.data.frame(x3)
        as_flextable(x3)
        x4=crosstable(mtcars3, c(mpg, vs, cyl, dummy, surv, hp_date, qsec_posix, diff, cyl3), 
                      by=c(am, gear), total=TRUE, 
                      times=c(100,200), followup=TRUE)
        as.data.frame(x4)
        as_flextable(x4)
    }) 
    # expect_equal(dim(x1), c(7,27))
    # expect_equal(dim(x2), c(7,15))
    # expect_equal(dim(x3), c(8,28))
    # expect_equal(dim(x4), c(11,5))
    # expect_equal(dim(x5), c(34,10))
})


test_that("By multiple formula interface", {
    
    expect_snapshot({
        x1 = crosstable(mtcars3, mpg+gear~cyl+I(am=="auto")+vs, total=TRUE)
        x1 %>% as_flextable()
    }) 
    # expect_equal(dim(x1), c(8,28))
    
})


test_that("By multiple warnings", {
    x1 = crosstable(mtcars3, c(mpg, gear, cyl), by=c(am, diff, qsec_posix, hp_date)) %>% 
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