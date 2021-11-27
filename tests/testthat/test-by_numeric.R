
# numeric by numeric ------------------------------------------------------

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




# By numeric=factor minimum of unique levels  ---------------------------------------
test_that("by factor if numeric <= 3 levels", {
    x10 = crosstable(mtcars2, cyl, by=vs, total="both", margin="all")
    x10 %>% as_flextable()
    x10 = as.data.frame(x10)
    
    expect_identical(x10[3,5], "14 (43.75% / 100.00% / 77.78%)")
    expect_equal(dim(x10), c(4,6))
    expect_equal(sum(is.na(x10)), 0)
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
    x = crosstable(mtcars3, c(disp, hp, am), by=vs, funs=c(meansd, quantile),
                   funs_arg = list(dig=3, probs=c(0.25,0.75)),
                   total=T, showNA="always")
    ft = as_flextable(x)
    x = as.data.frame(x)
    expect_snapshot(x)
    expect_snapshot(ft)
})

test_that("One function", {
    bar=function(x, dig=1, ...)  c("MinMax" = minmax(x, dig=dig, ...), "N_NA" = nna(x))
    bar2=function(x, dig=1, ...) c(minmax(x, dig=dig, ...), "N_NA"=nna(x))
    bar3=function(x, dig=1, ...) c(minmax(x, dig=dig, ...), nna(x))
    
    cross_list = list(
        crosstable(iris2, c(Sepal.Length), funs=mean),
        crosstable(iris2, c(Sepal.Length), funs="mean"),
        crosstable(iris2, c(Sepal.Length), funs=c("My mean" = mean)),
        crosstable(iris2, c(Sepal.Length), funs=cross_summary),
        crosstable(iris2, c(Sepal.Length), funs=c(" " = cross_summary)),
        crosstable(iris2, c(Sepal.Length), funs=list(" " = cross_summary)),
        crosstable(iris2, c(Sepal.Length), funs=c("first"=function(xx) xx[1])),
        crosstable(iris2, c(Sepal.Length), funs=c("first"=~.x[1]))
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
    bar=function(x, dig=1, ...)  c("MinMax" = minmax(x, dig=dig, ...), "N_NA" = nna(x))
    bar2=function(x, dig=1, ...) c(minmax(x, dig=dig, ...), "N_NA"=nna(x))
    bar3=function(x, dig=1, ...) c(minmax(x, dig=dig, ...), nna(x))
    
    cross_list = list(
        crosstable(iris2, c(Sepal.Length), by=Species, funs=mean),
        crosstable(iris2, c(Sepal.Length), by=Species, funs="mean"),
        crosstable(iris2, c(Sepal.Length), by=Species, funs=c("My mean" = mean)),
        crosstable(iris2, c(Sepal.Length), by=Species, funs=cross_summary),
        crosstable(iris2, c(Sepal.Length), by=Species, funs=c(" " = cross_summary)),
        crosstable(iris2, c(Sepal.Length), by=Species, funs=list(" " = cross_summary)),
        crosstable(iris2, c(Sepal.Length), by=Species, funs=c("first"=function(xx) xx[1])),
        crosstable(iris2, c(Sepal.Length), by=Species, funs=c("first"=~.x[1]))
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
                        "moy_fn"=function(.x) mean(.x, na.rm=TRUE), 
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
