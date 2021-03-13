
# Statistical Tests --------------------------------------------------

test_that("Statistical Tests", {
    # testthat::skip_on_cran()
    set.seed(0)
    dummy_data = tibble(x_norm=rnorm(50,0,20), x_exp=rexp(50,60), y=rnorm(50,0,20), tmt2=rep(c("A","B"), 25), tmt3=LETTERS[1:3][sample(3,50,replace=TRUE)])
    
    ##CATEGORICAL
    
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


test_that("Testing everything", {
    set.seed(1234)
    x = crosstable(mtcars3, disp+hp+am+surv~vs, margin="all", total="both",
                   times=c(0,100,200,400), followup=TRUE, funs_arg = list(dig=9),
                   test=T, effect=T) %>% 
        expect_warning(class="crosstable_effect_warning")
    ft = as_flextable(x)
    
    expect_snapshot(x)
    expect_snapshot(ft)
})



# Effects --------------------------------------------------

test_that("Effects: categorical variables", {
    set.seed(0)
    args = crosstable_effect_args()
    
    #args$effect_tabular = effect_odds_ratio (default)
    x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
    expect_equal(x$effect[1], "Odds ratio (Wald CI) (auto, vshaped vs straight): 5.25\n95%CI [0.80 to 34.43]")
    
    args$effect_tabular = effect_relative_risk
    x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
    expect_equal(x$effect[1], "Relative risk (Wald CI) (auto, vshaped vs straight): 2.70\n95%CI [0.94 to 15.21]")
    
    args$effect_tabular = effect_risk_difference
    x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
    expect_equal(x$effect[1], "Risk difference (Wald CI) (auto, vshaped minus straight): 165.82\n95%CI [-10.51 to 379.13]")
    
})

test_that("Effects: numeric variables", {
    set.seed(0)
    args = crosstable_effect_args()
    
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
    
    
    
})

test_that("Effects: survival variables", {
    set.seed(0)
    args = crosstable_effect_args()
    
    x=crosstable(mtcars3, surv, by=cyl3, effect=T, effect_args=args)
    expect_equal(x$effect[1], "Hazard ratio (Wald CI) (NA vs FALSE): 1.54\n95%CI [0.42 to 5.63]")
})



test_that("Effects never fail 1", {
    args = crosstable_effect_args()
    can_be_by = function(x) !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")
    
    names(mtcars3) %>% set_names() %>% map(~{
        set.seed(1234)
        if(!is_testing()) print(.x)
        if(can_be_by(mtcars3[[.x]])) {
            expect_snapshot({
                print(.x)
                crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
                    table %>% as.data.frame()
            })
        }
        return(0)
    })
    
})

test_that("Effects never fail 2", {
    args = crosstable_effect_args()
    args$effect_summarize = diff_mean_boot
    args$effect_tabular = effect_relative_risk
    can_be_by = function(x) !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")
    
    names(mtcars3) %>% set_names() %>% map(~{
        set.seed(1234)
        if(!is_testing()) print(.x)
        if(can_be_by(mtcars3[[.x]])) {
            expect_snapshot({
                print(.x)
                crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
                    table %>% as.data.frame()
            })
        }
        return(0)
    })
    
})

test_that("Effects never fail 3", {
    args = crosstable_effect_args()
    args$effect_summarize = diff_median
    args$effect_tabular = effect_risk_difference
    can_be_by = function(x) !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")
    
    names(mtcars3) %>% set_names() %>% map(~{
        if(!is_testing()) print(.x)
        if(can_be_by(mtcars3[[.x]])) {
            expect_snapshot({
                print(.x)
                set.seed(1234)
                crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
                    table %>% as.data.frame()
            })
        }
        return(0)
    })
})


