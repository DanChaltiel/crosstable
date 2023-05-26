

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
    x = crosstable(mtcars3, c(disp,hp,am,surv), by=vs, margin="all", total="both",
                   times=c(70,100,200,400), followup=TRUE, num_digits=0, percent_digits=0,
                   test=TRUE, effect=TRUE)
        # expect_warning(class="crosstable_effect_warning")
    ft = as_flextable(x)

    expect_snapshot(as.data.frame(x))
    expect_snapshot(ft)
})



test_that("Auto-testing is bad and you should feel bad.", {
    rlang::local_options(crosstable_verbosity_autotesting="verbose")
    expect_warning(crosstable(mtcars2, disp, by=vs, funs=mean, test=T),
                   class="crosstable_autotesting_warning")
})

# snapshot_review('1-tests')
