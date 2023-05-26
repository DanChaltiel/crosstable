#snapshot_review('effects')


# Categorical variables ---------------------------------------------------

test_that("Effects: categorical variables", {
  set.seed(0)
  e_args = crosstable_effect_args()

  # e_args$effect_tabular = effect_odds_ratio (default)
  x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=e_args)
  expect_match(x$effect[1], "0.19 [0.02 to 1.11]", fixed=TRUE)

  e_args$effect_tabular = effect_relative_risk
  x = crosstable(mtcars3, am~vs, effect=T, effect_args=e_args) %>%
    expect_warning2(class="crosstable_effect_error_warning")
  expect_match(attr(x, "obj")$effect[1], "0.56 [CI error]", fixed=TRUE)
  x = crosstable(mtcars3, am~fct_rev(vs), effect=T, effect_args=e_args)
  expect_match(x$effect[1], "2.96 [0.94 to 17.27]", fixed=TRUE)

  e_args$effect_tabular = effect_risk_difference
  x=crosstable(mtcars3, am, by=vs, effect=T, effect_args=e_args)
  expect_match(x$effect[1], "-1.66 [-3.79 to 0.11]", fixed=TRUE)
})


# Numeric variables -------------------------------------------------------

test_that("Effects: numeric variables", {
  e_args = crosstable_effect_args()

  #e_args$effect_summarize = diff_mean_auto (default)
  set.seed(1234)
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=e_args)
  expect_match(x$effect[1], "Difference in means (bootstrap CI)", fixed=TRUE)
  expect_match(x$effect[1], "190.34 [119.89 to 260.78]", fixed=TRUE)

  e_args$effect_summarize = diff_mean_student
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=e_args)
  expect_match(x$effect[1], "Difference in means (t-test CI)", fixed=TRUE)
  expect_match(x$effect[1], "190.34 [104.14 to 276.54]", fixed=TRUE)

  # mtcars3 %>% map(~if(is.numeric(.x)) try(test_normality(.x, mtcars2$vs)))
  x=crosstable(mtcars3, hp, by=vs, effect=T)
  expect_match(x$effect[1], "Difference in means (Welch CI)", fixed=TRUE)
  expect_match(x$effect[1], "102.00 [63.49 to 140.51]", fixed=TRUE)

  set.seed(1234)
  e_args$effect_summarize = diff_mean_boot
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=e_args)
  expect_match(x$effect[1], "Difference in means (bootstrap CI)", fixed=TRUE)
  expect_match(x$effect[1], "190.34 [119.89 to 260.78]", fixed=TRUE)

  set.seed(1234)
  e_args$effect_summarize = diff_median_boot
  x=crosstable(mtcars3, disp, by=vs, effect=T, effect_args=e_args)
  expect_match(x$effect[1], "Difference in medians (bootstrap CI)", fixed=TRUE)
  expect_match(x$effect[1], "208.90 [89.06 to 283.31]", fixed=TRUE)
})


# Survival variables ------------------------------------------------------

test_that("Effects: survival variables", {
  set.seed(1234)
  x=crosstable(mtcars3, surv, by=cyl6, effect=T)
  expect_match(x$effect[1], "Hazard ratio (Wald CI)", fixed=TRUE)
  expect_match(x$effect[1], "0.66 [0.08 to 5.41]", fixed=TRUE)

  crosstable(mtcars3, surv, by=am, effect=T) %>%
    expect_warning(class="crosstable_effect_warning")
})


# Survival variables ------------------------------------------------------

test_that("Effects: missing variables", {
  set.seed(1234)
  mtcars3$x = ifelse(is.na(mtcars3$vs), "missing", paste0("cyl", mtcars3$cyl))

  x = crosstable(mtcars3, x, by=vs, effect=T) %>%
    expect_warning2(class="crosstable_effect_warning")

  expect_match(attr(x, "object")$effect[1], "Odds ratio [95% Wald CI]", fixed=TRUE)
  expect_match(attr(x, "object")$effect[1], "missing vs cyl4", fixed=TRUE)
})


# Warnings/Errors ---------------------------------------------------------


test_that("Effects Warnings", {
  set.seed(1234)

  crosstable(mtcars3, cyl, by=c(am, vs), effect=T) %>%
    expect_warning(class="crosstable_multiby_effect_warning")

  crosstable(mtcars3, c(am, vs), by=cyl, effect=T) %>%
    expect_warning(class="crosstable_effect_2groups_warning") #3 groups
  crosstable(mtcars3, c(am, vs), by=dummy, effect=T) %>%
    expect_warning(class="crosstable_effect_2groups_warning") #1 group

  crosstable(mtcars3, by=vs, times=c(0,100,200,400), effect=T) %>%
    expect_warning("fitted probabilities numerically 0 or 1 occurred") %>%
    expect_warning("fitted probabilities numerically 0 or 1 occurred") %>%
    expect_warning("fitted probabilities numerically 0 or 1 occurred") %>%
    expect_warning(class="crosstable_effect_other_warning") %>%
    expect_warning(class="crosstable_all_na_warning")
})

