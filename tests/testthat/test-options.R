

t <- data.frame(x=c('a', 'a', 'b'))
ct <- crosstable(t, cols=c('x'))
gt <- as_gt(ct, generic_labels=list(value="count"))


test_that("No missing options", {
  skip_on_ci()
  skip_on_cran()
  path = test_path("../../R")
  skip_if(!dir.exists(path))

  ignored = c(
    "rec_sep", "rec_max_length", "ct_label_recursion_max", #internal, unused yet
    "crosstable_...", "crosstable_.local", "crosstable_reset"
  )

  missing_options = missing_options_helper(path=path, ignore=ignored)

  #missing options, not handled in crosstable_options()
  missing_options$not_handled %>% expect_identical(character(0))
  # added options, handled in crosstable_options() but never used in the code
  missing_options$not_used %>% expect_identical(character(0))
})


test_that("Options work", {
  local({
    crosstable_reset_options(quiet=TRUE)
    x=crosstable_peek_options()
    expect_length(x, 0)

    #normal func
    crosstable_options(remove_zero_percent=TRUE)
    #legacy
    crosstable_options(crosstable_wrap_id=50)
    #duplicates
    expect_warning(crosstable_options(units="cm", crosstable_units="error",
                                      percent_pattern="{n}", crosstable_percent_pattern="error"),
                   class="crosstable_dupl_option_warning")
    #unknown options
    expect_warning(crosstable_options(foo="bar", bar="foo"),
                   class="crosstable_unknown_option_warning")

    x=crosstable_peek_options()
    expect_mapequal(x, list(crosstable_remove_zero_percent = TRUE, crosstable_wrap_id=50,
                            crosstable_units = "cm", crosstable_percent_pattern = "{n}"))

    #reset
    expect_message(crosstable_reset_options())
    expect_silent(crosstable_reset_options(quiet=TRUE))
    lifecycle::expect_deprecated(crosstable_options(reset=TRUE))
    expect_length(crosstable_peek_options(), 0)

    #default in testing env
    crosstable_options(crosstable_verbosity_autotesting="quiet")
  })
})


test_that("Renaming work", {
  #zero_percent was renamed to remove_zero_percent
  crosstable_options(zero_percent=TRUE, .local=TRUE)
  a=crosstable_peek_options()
  expect_in("crosstable_remove_zero_percent", names(a))
})


test_that("All options work", {
  crosstable_reset_options(quiet=TRUE)
  local_reproducible_output(width = 1000)
  set.seed(1234)
  # withr::deferred_clear()

  ct_noopt = crosstable(mtcars3, c(cyl, carb, qsec_posix, surv), by=vs, test=TRUE, effect=TRUE) %>%
    suppressWarnings()

  crosstable_options(
    remove_zero_percent=TRUE,
    only_round=TRUE,
    verbosity_autotesting="quiet",
    verbosity_duplicate_cols="quiet",
    total="both",
    percent_pattern="{n} ({p_row}, {p_col})",
    percent_digits=1,
    num_digits=1,
    showNA="always",
    label=FALSE,
    # funs=meansd,
    funs=c("Mean (SD)"=meansd),
    funs_arg=list(na.rm=FALSE),
    cor_method="kendall",
    unique_numeric=4,
    date_format="%d/%m/%Y",
    times=c(0,100),
    followup=TRUE,
    test_args = crosstable_test_args(plim=1),
    effect_args = crosstable_effect_args(conf_level=0.7),
    .local=TRUE
  )
  ct_opt = crosstable(mtcars3, c(cyl, carb, qsec_posix, surv), by=vs, test=TRUE, effect=TRUE) %>%
    suppressWarnings()
  
  if (getRversion() >= "4.6") {
    valR4.5 = "p value: 0.0001 \n(Wilcoxon rank sum test)"
    valR4.6 = "p value: 0.0001 \n(Wilcoxon rank sum exact test)"
    if (any(ct_noopt$test[5:8] != valR4.6)) {
      stop("Error in wilcoxon exact test in R>4.6")
    }
    ct_noopt$test[5:8] = valR4.5

    
    valR4.5 = "p value: <0.1 \n(Wilcoxon rank sum test)"
    valR4.6 = "p value: <0.1 \n(Wilcoxon rank sum exact test)"
    if (ct_opt$test[6] != valR4.6) {
      stop("Error in wilcoxon exact test in R>4.6")
    }
    ct_opt$test[6] = valR4.5
  }
  
  expect_snapshot({
    as.data.frame(ct_noopt)
    as.data.frame(ct_opt)
  })


  #as_flextable()
  ft_noopt = as_flextable(ct_opt)
  # local_options(
  crosstable_options(
    wrap_id=30,
    compact_padding=40,
    header_show_n_pattern="{.col}={.n}",
    keep_id=TRUE,
    autofit=FALSE,
    compact=TRUE,
    remove_header_keys=TRUE,
    show_test_name=FALSE,
    padding_v=0,
    header_show_n=TRUE,
    fontsize_body=8,
    fontsize_subheaders=9,
    fontsize_header=10,
    .local=TRUE
  )
  ft_opt = as_flextable(ct_opt)

  expect_snapshot({
    ft_noopt
    ft_opt
  })

  # x = crosstable(mtcars3, c(cyl, carb, qsec_posix, surv), by=vs)
  # print(af(x))


  # expect_snapshot({
  #   x = crosstable(mtcars3, c(cyl, carb, qsec_posix, surv), by=vs)
  #   as.data.frame(x)
  # })
})
