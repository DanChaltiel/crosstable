
test_that("Effects: categorical variables", {
    set.seed(0)
    args = crosstable_effect_args()
    
    # args$effect_tabular = effect_odds_ratio (default)
    x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
    expect_match(x$effect[1], "0.19 [0.02 to 1.11]", fixed=TRUE)
    
    args$effect_tabular = effect_relative_risk
    x = crosstable(mtcars3, am~vs, effect=T, effect_args=args) %>% 
        expect_warning2(class="crosstable_effect_error_warning")
    expect_match(attr(x, "obj")$effect[1], "0.56 [CI error]", fixed=TRUE)
    x = crosstable(mtcars3, am~fct_rev(vs), effect=T, effect_args=args)
    expect_match(x$effect[1], "2.96 [0.94 to 17.27]", fixed=TRUE)
    
    args$effect_tabular = effect_risk_difference
    x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=args)
    expect_match(x$effect[1], "-1.66 [-3.79 to 0.11]", fixed=TRUE)
})#TODO doc effect dire que ref = 1er level des factors

test_that("Effects: numeric variables", {
    args = crosstable_effect_args()
    
    #args$effect_summarize = diff_mean_auto (default)
    set.seed(1234)
    x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
    expect_match(x$effect[1], "Difference in means (bootstrap CI)", fixed=TRUE)
    expect_match(x$effect[1], "190.34 [119.89 to 260.78]", fixed=TRUE)
    
    args$effect_summarize = diff_mean_student
    x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
    expect_match(x$effect[1], "Difference in means (t-test CI)", fixed=TRUE)
    expect_match(x$effect[1], "190.34 [104.14 to 276.54]", fixed=TRUE)
    
    # mtcars3 %>% map(~if(is.numeric(.x)) try(test_normality(.x, mtcars2$vs)))
    x=crosstable(mtcars3, hp, by=vs, effect=T)
    expect_match(x$effect[1], "Difference in means (Welch CI)", fixed=TRUE)
    expect_match(x$effect[1], "102.00 [63.49 to 140.51]", fixed=TRUE)
    
    set.seed(1234)
    args$effect_summarize = diff_mean_boot
    x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
    expect_match(x$effect[1], "Difference in means (bootstrap CI)", fixed=TRUE)
    expect_match(x$effect[1], "190.34 [119.89 to 260.78]", fixed=TRUE)
    
    set.seed(1234)
    args$effect_summarize = diff_median_boot
    x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=args)
    expect_match(x$effect[1], "Difference in medians (bootstrap CI)", fixed=TRUE)
    expect_match(x$effect[1], "208.90 [89.06 to 283.31]", fixed=TRUE)
})

test_that("Effects: survival variables", {
    set.seed(1234)
    x=crosstable(mtcars3, surv, by=cyl6, effect=T)
    expect_match(x$effect[1], "Hazard ratio (Wald CI)", fixed=TRUE)
    expect_match(x$effect[1], "0.66 [0.08 to 5.41]", fixed=TRUE)
    
    x=crosstable(mtcars3, surv, by=am, effect=T) %>% 
        expect_warning(class="crosstable_effect_warning")
})




# Automatic snapshot tests ------------------------------------------------
# They sometimes fail on other OS than Windows due to tiny RNG differences


test_that("Effects never fail 1", {
    skip_on_os(c("mac", "linux", "solaris"))
    rlang::local_options(width = 100)
    args = crosstable_effect_args()
    can_be_by = function(x) !is.numeric(x) && length(unique(x))!=2 && !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")
    
    names(mtcars3) %>% set_names() %>% map(~{
        set.seed(1234)
        if(!is_testing()) print(glue("Effect part 1 - variable={.x}"))
        if(can_be_by(mtcars3[[.x]])) {
            expect_snapshot({
                print(glue("Effect part 1 - variable={.x}"))
                crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
                    table %>% as.data.frame()
            })
        }
        return(0)
    })
    
})

test_that("Effects never fail 2", {
    skip_on_os(c("mac", "linux", "solaris"))
    rlang::local_options(width = 100)
    args = crosstable_effect_args()
    args$effect_summarize = diff_mean_boot
    args$effect_tabular = effect_relative_risk
    can_be_by = function(x) !is.numeric(x) && length(unique(x))!=2 && !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")
    
    names(mtcars3) %>% set_names() %>% map(~{
        set.seed(1234)
        if(!is_testing()) print(glue("Effect part 2 - variable={.x}"))
        if(can_be_by(mtcars3[[.x]])) {
            expect_snapshot({
                print(glue("Effect part 2 - variable={.x}"))
                crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
                    table %>% as.data.frame()
            })
        }
        return(0)
    })
    
})

test_that("Effects never fail 3", {
    skip_on_os(c("mac", "linux", "solaris"))
    rlang::local_options(width = 100)
    args = crosstable_effect_args()
    args$effect_summarize = diff_median_boot
    args$effect_tabular = effect_risk_difference
    can_be_by = function(x) !is.numeric(x) && length(unique(x))!=2 && !is.Surv(x) && !is.date(x) && !all(is.na(x)) && !inherits(x, "difftime")
    
    names(mtcars3) %>% set_names() %>% map(~{
        if(!is_testing()) print(glue("Effect part 3 - variable={.x}"))
        if(can_be_by(mtcars3[[.x]])) {
            expect_snapshot({
                print(glue("Effect part 3 - variable={.x}"))
                set.seed(1234)
                crosstable(mtcars3, by=any_of(.x), effect=T, effect_args=args)$effect %>%
                    table %>% as.data.frame()
            })
        }
        return(0)
    })
})


test_that("Effects Warnings", {
    crosstable(mtcars3, by=vs, times=c(0,100,200,400), effect=T) %>%
        expect_warning("fitted probabilities numerically 0 or 1 occurred") %>%
        expect_warning("fitted probabilities numerically 0 or 1 occurred") %>%
        expect_warning(class="crosstable_effect_other_warning")
})

#snapshot_review('1-effects')