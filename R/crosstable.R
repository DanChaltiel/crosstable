
#' @importFrom utils globalVariables
utils::globalVariables(c(".", "x", "y", "n", "where", "ct", "col_keys",
                         "p_col", ".col_1", ".col_2", "value",
                         ".data", ".env"))

crosstable_caller = rlang::env()


#' Easily describe datasets
#'
#' Generate a descriptive table of all chosen columns, as contingency tables for categorical variables and as calculation summaries for numeric variables. If the `by` argument points to one or several categorical variables, `crosstable` will output a description of all columns for each level. Otherwise, if it points to a numeric variable, `crosstable` will calculate correlation coefficients with all other selected numeric columns. Finally, if it points to a `Surv` object, `crosstable` will describe the survival at different times.\cr
#' \cr
#' Can be formatted as an HTML table using [as_flextable()].
#'
#' @param data A data.frame
#' @param cols <[`tidy-select`][tidyselect::language]> Columns to describe, default to `everything()`. See examples or `vignette("crosstable-selection")` for more details.
#' @param ... Unused. All parameters after this one must be named.
#' @param by The variable to group on. Character or name.
#' @param funs Functions to apply to numeric variables. Default to [cross_summary()].
#' @param funs_arg Additional parameters for `funs`, e.g. `digits` (the number of decimal places) for the default [cross_summary()]. Ultimately, these arguments are passed to [format_fixed()].
#' @param total one of \["none", "row", "column" or "both"] to indicate whether to add total rows and/or columns. Default to `none`.
#' @param percent_pattern Pattern used to describe proportions in categorical data. Syntax uses a [glue::glue()] specification, see the **section** below for more details. Default to `"{n} ({p_col})"` if `by` is null and `"{n} ({p_row})"` if it is not.
#' @param percent_digits Number of digits for percentages.
#' @param num_digits Number of digits for numeric summaries.
#' @param unique_numeric The number of non-missing different levels a variable should have to be considered as numeric.
#' @param showNA Whether to show NA in categorical variables (one of \code{c("ifany", "always", "no")}, like in \code{table()}).
#' @param label Whether to show labels. See [import_labels()] or [set_label()]for how to add labels to the dataset columns.
#' @param cor_method One of `c("pearson", "kendall", "spearman")` to indicate which correlation coefficient is to be used.
#' @param drop_levels Whether to drop unused levels of factor variables. Default to `TRUE`.
#' @param remove_zero_percent Whether to remove proportions when `n==0`. Default to `FALSE`.
#' @param times When using formula with [survival::Surv()] objects, which times to summarize.
#' @param followup When using formula with [survival::Surv()] objects, whether to display follow-up time.
#' @param test Whether to perform tests.
#' @param test_args See \code{\link{crosstable_test_args}} to override default testing behaviour.
#' @param effect Whether to compute a effect measure.
#' @param effect_args See \code{\link{crosstable_effect_args}} to override default behaviour.
#' @param margin Deprecated in favor of `percent_pattern`. One of \["row", "column", "cell", "none", or "all"]. Default to `row`.
#' @param .vars Deprecated in favor of `cols`.
#' @inheritParams format_fixed
#'
#' @section About `percent_pattern`:
#' The `percent_pattern` argument is very powerful but can be difficult to understand at first :
#' * It is usually a single string that uses the glue syntax, where variables are put in curly braces (`{x}`).
#' * Counts are expressed as `{n}`, `{n_row}`, `{n_col}`, and `{n_tot}`, and proportions as `{p_row}`, `{p_col}`, and `{p_cell}`, depending on the margin on which they are calculated.
#' * For each variable, a version including missing values in the total is proposed as `{n_xxx_na}` or `{p_xxx_na}`.
#' * For each proportion, a confidence interval is also calculated using [Wilson score](https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval) and can be expressed as `{p_xxx_inf}` and `{p_xxx_sup}`. See examples for practical applications.
#' * Alternatively, `percent_pattern` can be a list of characters with names `body`, `total_row`, `total_col`, and `total_all` to also control the pattern in other parts of the crosstable than the body.
#'
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate assert_choice assert_count assert_data_frame assert_list assert_logical assert_multi_class makeAssertCollection reportAssertions
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr across any_of cur_column everything intersect mutate n_distinct pull select where
#' @importFrom forcats fct_na_value_to_level
#' @importFrom glue glue
#' @importFrom lifecycle deprecate_stop deprecate_warn deprecated
#' @importFrom purrr map map_chr
#' @importFrom rlang as_function call_args call_match check_dots_unnamed current_env dots_n enquo is_empty is_formula local_options quo_get_expr
#' @importFrom stats model.frame na.omit
#' @importFrom tidyr unite
#'
#' @return A `data.frame`/`tibble` of class `crosstable`
#'
#' @seealso https://danchaltiel.github.io/crosstable/, as_flextable, import_labels
#'
#' @examples
#' #whole table
#' crosstable(iris)
#' crosstable(mtcars)
#' crosstable(mtcars2)
#'
#' #tidyselection, custom functions
#' library(dplyr)
#' crosstable(mtcars2, c(ends_with("t"), starts_with("c")), by=vs,
#'            funs=c(mean, quantile), funs_arg=list(probs=c(.25,.75)))
#'
#' #margin and totals, multiple by
#' crosstable(mtcars2, c(disp, cyl), by=c(am, vs),
#'            margin=c("row", "col"), total = "both")
#'
#' #predicate selection, correlation, effect calculation
#' crosstable(mtcars2, where(is.numeric), by=hp, effect=TRUE)
#'
#' #lambda selection & statistical tests
#' crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, test=TRUE)
#'
#' #Dates
#' mtcars2$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
#' crosstable(mtcars2, my_date, by=vs, date_format="%d/%m/%Y")
#'
#' #Survival data (using formula syntax)
#' library(survival)
#' crosstable(aml, Surv(time, status) ~ x, times=c(0,15,30,150), followup=TRUE)
#'
#' #Patterns
#' crosstable(mtcars2, vs, by=am, percent_digits=0,
#'            percent_pattern="{n} ({p_col} / {p_row})")
#' crosstable(mtcars2, vs, by=am, percent_digits=0,
#'            percent_pattern="N={n} \np[95%CI] = {p_col} [{p_col_inf}; {p_col_sup}]")
#' str_high="n>5"; str_lo="n<=5"
#' crosstable(mtcars2, vs, by=am, percent_digits=0,
#'            percent_pattern="col={p_col}, row={p_row} ({ifelse(n<5, str_lo, str_high)})")
crosstable = function(data, cols=everything(), ..., by=NULL,
                      total = c("none", "row", "column", "both"),
                      percent_pattern = "{n} ({p_row})",
                      percent_digits = 2, num_digits = 1,
                      showNA = c("ifany", "always", "no"), label = TRUE,
                      funs = c(" " = cross_summary), funs_arg=list(),
                      cor_method = c("pearson", "kendall", "spearman"),
                      drop_levels = FALSE, remove_zero_percent=FALSE,
                      unique_numeric = 3, date_format=NULL,
                      times = NULL, followup = FALSE,
                      test = FALSE, test_args = crosstable_test_args(),
                      effect = FALSE, effect_args = crosstable_effect_args(),
                      margin = deprecated(),
                      .vars = deprecated()) {
  debug=list()
  local_options(stringsAsFactors=FALSE)
  crosstable_caller$env = rlang::current_env()

  # Options -------------------------------------------------------------
  missing_percent_pattern = missing(percent_pattern)
  missing_margin = missing(margin)

  if(missing(total)) total = getOption("crosstable_total", 0)
  if(missing(percent_digits)) percent_digits = getOption("crosstable_percent_digits", 2)
  if(missing(showNA)) showNA = getOption("crosstable_showNA", "ifany")
  if(missing(label)) label = getOption("crosstable_label", TRUE)
  if(missing(funs)) funs = getOption("crosstable_funs",  c(" " = cross_summary))
  if(missing(funs_arg)) funs_arg = getOption("crosstable_funs_arg",  list())
  if(missing(cor_method)) cor_method = getOption("crosstable_cor_method",  "pearson")
  if(missing(drop_levels)) drop_levels = getOption("crosstable_drop_levels",  FALSE)
  if(missing(unique_numeric)) unique_numeric = getOption("crosstable_unique_numeric", 3)
  if(missing(date_format)) date_format = getOption("crosstable_date_format",  NULL)
  if(missing(times)) times = getOption("crosstable_times", NULL)
  if(missing(followup)) followup = getOption("crosstable_followup", followup)
  if(missing(test_args)) test_args = getOption("crosstable_test_args", crosstable_test_args())
  if(missing(effect_args)) effect_args = getOption("crosstable_effect_args", crosstable_effect_args())
  if(missing(num_digits)) num_digits = getOption("crosstable_num_digits",  1)
  if(missing_margin) margin = getOption("crosstable_margin",  NULL)

  if(!"dig" %in% names(funs_arg)){
    funs_arg = c(funs_arg, list(dig=num_digits))
    #TODO warning/error si "dig" %in% names(funs_arg) && !missing(num_digits)
  }
  # Arguments checks ----------------------------------------------------

  check_dots_unnamed()
  coll = makeAssertCollection()
  assert_data_frame(data, null.ok=FALSE, add=coll)
  dataCall = deparse(substitute(data))
  byCall = deparse(substitute(by))
  data = as.data.frame(data)
  assert_multi_class(percent_pattern, c("list", "character"), add=coll)
  assert_count(percent_digits, add=coll)
  assert_logical(label, len=1, add=coll)
  assert_logical(followup, len=1, add=coll)
  assert_logical(test, len=1, add=coll)
  assert_logical(effect, len=1, add=coll)
  assert_list(funs_arg, add=coll)
  if(isFALSE(showNA)) showNA="no"
  if(isTRUE(showNA)) showNA="always"
  showNA = match.arg(showNA)
  cor_method = match.arg(cor_method)

  if(is.null(total)) total = 0
  else if(isTRUE(total)) total = 1:2
  else if(is.character(total)) {
    assert_choice(total, c("none", "both", "all", "row", "col", "column"), add=coll)
    totalopts = list(all = 1:2,
                     both = 1:2,
                     row = 1,
                     col = 2,
                     column = 2,
                     none = 0)
    total = unname(unlist(totalopts[total]))
  }
  reportAssertions(coll)

  percent_pattern = validate_percent_pattern(margin, percent_pattern, missing_margin, missing_percent_pattern)
  check_percent_pattern(percent_pattern)

  if(!is.null(date_format)) funs_arg = c(funs_arg, list(date_format=date_format))

  autotesting_verbosity = getOption("crosstable_verbosity_autotesting", "default")
  if(test==T && autotesting_verbosity!="quiet"){
    if(autotesting_verbosity=="verbose") freq = "always"
    else freq = "regularly"
    cli_warn("Be aware that automatic global testing should only be done in an exploratory context, as it would cause extensive alpha inflation otherwise.",
             class="crosstable_autotesting_warning",
             call=current_env(),
             .frequency = freq, .frequency_id="crosstable_global_testing")
  }

  # Deprecations --------------------------------------------------------
  if(!missing(...) && dots_n(...)>0){
    m=call_match(dots_expand=F)
    x=call_args(m)
    colsCall = deparse(x$cols)
    dotsCall = x$... %>% as.list() %>% map(deparse) %>% unlist()
    dotsCallp = dotsCall %>% paste(collapse =", ")
    badcall = paste(deparse(m$cols), dotsCallp, sep=", ")
    goodcall = c(colsCall, dotsCall) %>% map(call_vars) %>% unlist() %>%
      unique() %>% paste(collapse =", ")
    bad = glue("Bad : `crosstable({dataCall}, {badcall}, ...)`")
    good = glue("Good: `crosstable({dataCall}, c({goodcall}), ...)`")
    deprecate_warn("0.2.0", "crosstable(...=)", "crosstable(cols=)",
                   details=c(x=bad,v=good))
  }
  if(!missing(.vars)){
    deprecate_stop("0.2.0", "crosstable(.vars=)", "crosstable(cols=)")
  }


  # # Logic handle --------------------------------------------------------

  cols_is_formula = enquo(cols) %>% quo_get_expr() %>% is_formula()
  cols_is_lambda = cols_is_formula && is_one_sided(cols)

  if(cols_is_formula && !cols_is_lambda){
    debug$interface="formula"
    f=cols
    if(!is_empty(byCall) && byCall!="NULL"){
      cli_abort(c("{.arg by} cannot be used together with the formula interface.
                  Please include it in the formula or use another syntax.",
                  i="formula = {format(f)}",
                  i="by = {byCall}"),
                class="crosstable_formula_by_error")
    }
    data_x = model.frame(f[-3], data, na.action = NULL)
    data_y = model.frame(f[-2], data, na.action = NULL)
  } else {
    debug$interface="quosure"
    if(cols_is_lambda) cols=as_function(cols)
    data_x = data %>% select({{cols}}, ...) %>% as.data.frame()
    data_y = data %>% select({{by}}) %>% as.data.frame()
  }
  byname = names(data_y)

  duplicate_cols = intersect(names(data_y), names(data_x))
  verbosity_duplicate_cols = getOption("crosstable_verbosity_duplicate_cols", "default")
  if(length(duplicate_cols)>0 && verbosity_duplicate_cols=="verbose"){
    cli_warn(c("Some columns were selected in `by` and in `cols` and were removed from the latter.",
               i="Columns automatically removed from `cols`: {.code {duplicate_cols}}"),
             class="crosstable_duplicate_cols_warning",
             call=current_env())
  }

  verbosity_na_cols = getOption("crosstable_verbosity_na_cols", "quiet")
  na_cols_x = data_x %>% select(where(~all(is.na(.x) | is_blank(.x)))) %>% names()
  if(length(na_cols_x)>0 && verbosity_na_cols=="verbose"){
    cli_warn(c('Cannot describe column{?s} {.var {na_cols_x}} as {?it/they} contain{?s/} only missing values/blank.'),
             class = "crosstable_all_na_warning",
             call = crosstable_caller$env)
  }

  na_cols_y = data_y %>% select(where(~all(is.na(.x) | is_blank(.x)))) %>% names()
  if(length(na_cols_y)>0){
    cli_warn(c('Cannot use {.var {na_cols_y}} as `by` column{?s} as {?it/they} contain{?s/} only missing/blank values.'),
             class = "crosstable_all_na_by_warning",
             call = crosstable_caller$env)
  }

  data_x = select(data_x, -any_of(c(duplicate_cols, na_cols_x)))
  data_y = select(data_y, -any_of(c(na_cols_y)))
  ncol_x = if(is.null(data_x)) 0 else ncol(data_x)
  ncol_y = if(is.null(data_y)) 0 else ncol(data_y)

  if(missing_percent_pattern && is.null(margin)) {
    one_col_dummy = ncol_y==1 && length(unique(data_y[[1]]))==1
    default = if(one_col_dummy||ncol_y==0) "{n} ({p_col})" else "{n} ({p_row})"
    percent_pattern$body = getOption("crosstable_percent_pattern", default)
  }

  # Unique Numerics ---------------------------------------------------------
  if(ncol_x>0){
    data_x = data_x %>% mutate(
      across(where(is.logical),
             ~ .x %>% as.character() %>% set_label(get_label(.x))),
      across(where(~is.numeric.and.not.surv(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric),
             ~{
               levels = na.omit(unique(mixedsort(.x)))
               .x = factor(.x, labels=levels) %>% set_label(get_label(.x))
               class(.x) = c("unique_numeric", class(.x))
               .x
             }),
    )
  }

  if(ncol_y>0){
    data_y = data_y %>% mutate(
      across(where(is.logical),
             ~ .x %>% as.character() %>% set_label(get_label(.x))),
      across(where(~is.numeric.and.not.surv(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric),
             ~{
               levels = na.omit(unique(mixedsort(.x)))
               .x = factor(.x, labels=levels) %>% set_label(get_label(.x))
               class(.x) = c("unique_numeric", class(.x))
               .x
             })
    )
  }

  # Return checks -------------------------------------------------------
  if(ncol_x==0) {
    cli_warn("Variable selection in crosstable ended with no variable to describe",
             class="crosstable_empty_warning",
             call=current_env())
    rtn=data.frame()
    class(rtn) = c("crosstable", "data.frame")
    attr(rtn, "debug") = debug
    return(rtn)
  }

  ## No BY ----
  if(ncol_y==0) {
    test=effect=FALSE
    data_y=NULL

    if(identical(total, 1)){
      cli_warn("`crosstable()` cannot add total in rows if `by` is NULL",
               class="crosstable_totalrow_bynull",
               call=current_env())
    }
  }

  ## BY one ----
  if(ncol_y==1){
    y_var = data_y[[1]]
    if(!is.numeric.and.not.surv(y_var) && !is.character.or.factor(y_var)){
      cli_abort(c("Crosstable only supports numeric, logical, character or factor `by` columns.",
                  i="`by` is pointing to the column {.code {names(data_y)}} {.cls {class(y_var)}}"),
                class="crosstable_wrong_byclass_error")
    }
    if(is.numeric(y_var)){
      tmp=funs_arg[!names(funs_arg) %in% c("dig", "date_format")]
      if(!identical(funs, c(` `=cross_summary)) || length(tmp)>0){
        cli_warn("`funs` and `funs_arg` arguments will not be used if `by` is numeric.",
                 class="crosstable_funs_by_warning",
                 call=current_env())
      }
    }
  }

  ## multi BY ----
  if(ncol_y>1) {

    #supported classes
    data_y2 = data_y %>% select(where(~is.logical(.x)||is.character.or.factor(.x)))
    nameclass_diff = setdiff(paste_nameclasses(data_y), paste_nameclasses(data_y2))
    if(length(nameclass_diff)>0){
      message = "Crosstable only supports logical, character or factor `by` columns (multiple)."
      if(ncol(data_y2)==0){
        cli_abort(c(message,
                    i="All columns were problematic:",
                    i="{.code {nameclass_diff}}"),
                  class="crosstable_multiby_wrong_class_error",
                  call=current_env())
      } else {
        cli_warn(c(message,
                   i="Columns automatically removed from `by`:",
                   i="{.code {nameclass_diff}}"),
                 class="crosstable_multiby_wrong_class_warning",
                 call=current_env())
      }
    }
    data_y = data_y2


    #tests and effects
    if(test==TRUE) {
      cli_warn("Cannot perform tests with multiple `by` strata.",
               class="crosstable_multiby_test_warning",
               call=current_env())
      test=FALSE
    }
    if(effect==TRUE) {
      cli_warn("Cannot compute effects with multiple `by` strata.",
               class="crosstable_multiby_effect_warning",
               call=current_env())
      effect=FALSE
    }
  }


  x_class = map_chr(data_x, ~paste_classes(.x))
  y_class = map_chr(data_y, ~paste_classes(.x))
  multiby = !is.null(data_y) && ncol_y>1

  # Function call -------------------------------------------------------
  by_levels = map(data_y, ~{
    if(is.numeric(.x)) NULL
    else if(is.factor(.x) && anyNA(.x)) levels(fct_na_value_to_level(.x, "NA"))
    else if(is.factor(.x)) levels(.x)
    else sort(unique(as.character(.x)), na.last=TRUE)
  })
  if(showNA=="no") by_levels = map(by_levels, ~.x[!is.na(.x)])
  funs = parse_funs(funs)
  if(multiby){
    data_y_lvl = expand.grid(by_levels, stringsAsFactors=FALSE) %>%
      mutate(across(everything(), ~paste(cur_column(), .x, sep="="))) %>%
      unite("y", sep = " & ") %>% pull()

    data_y2 = data_y %>%
      mutate(across(everything(), ~paste(cur_column(), .x, sep="="))) %>%
      unite(col="y", sep=" & ") %>%
      mutate(y=factor(y, levels=data_y_lvl))

    rtn = cross_by(data_x=data_x, data_y=data_y2, funs=funs, funs_arg=funs_arg,
                   percent_pattern=percent_pattern, total=total,
                   percent_digits=percent_digits, showNA=showNA, drop_levels=drop_levels,
                   remove_zero_percent=remove_zero_percent,
                   cor_method=cor_method, times=times, followup=followup,
                   test=test, test_args=test_args,
                   effect=effect, effect_args=effect_args, label=label)

    class(rtn) = c("crosstable_multiby", "crosstable", "tbl_df", "tbl", "data.frame")

  } else {
    data_y2 = data_y
    rtn = cross_by(data_x=data_x, data_y=data_y, funs=funs, funs_arg=funs_arg,
                   percent_pattern=percent_pattern, percent_digits=percent_digits,
                   total=total, showNA=showNA, drop_levels=drop_levels,
                   remove_zero_percent=remove_zero_percent,
                   cor_method=cor_method, times=times, followup=followup, test=test, test_args=test_args,
                   effect=effect, effect_args=effect_args, label=label)
    class(rtn) = c("crosstable", "tbl_df", "tbl", "data.frame")
  }

  # Attributes and return -----------------------------------------------
  debug$x_class = x_class
  debug$y_class = y_class
  attr(rtn, "debug") = debug
  attr(rtn, "N") = nrow(data)
  attr(rtn, "showNA") = showNA
  attr(rtn, "variables") = names(data_x)
  attr(rtn, "has_test") = test
  attr(rtn, "has_effect") = effect
  attr(rtn, "has_total") = total
  attr(rtn, "has_label") = label
  if(ncol_y==0){
    attr(rtn, "by") = NULL
    attr(rtn, "by_label") = NULL
    attr(rtn, "by_table") = NULL
    attr(rtn, "by_levels") = NULL
  } else {
    attr(rtn, "by") = byname
    attr(rtn, "by_label") = get_label(data_y)
    attr(rtn, "by_table") = table(data_y2)
    attr(rtn, "by_levels") = by_levels
  }
  return(rtn)
}
