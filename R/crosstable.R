
utils::globalVariables(c("x", "y", "ct", "col_keys"))

#' Easily describe datasets
#' 
#' Generate a descriptive table of all chosen columns, as contingency tables for categorical variables and as calculation summaries for numeric variables. If the `by` argument points to one or several categorical variables, `crosstable` will output a description of all columns for each level. Otherwise, if it points to a numeric variable, `crosstable` will calculate correlation coefficients with all other selected numeric columns. Finally, if it points to a `Surv` object, `crosstable` will describe the survival at different times.\cr
#' \cr
#' Can be formatted as an HTML table using [as_flextable()].
#' 
#' @param data A data.frame
#' @param cols The variables to describe. Can be a character or name vector, a tidyselect helper, a (lambda) function that returns a logical, or a formula. See examples or `vignette("crosstable-selection")` for more details.
#' @param ... Unused. All parameters after this one must be named.
#' @param by The variable to group on. Character or name.
#' @param funs Functions to apply to numeric variables. Default to [cross_summary()].
#' @param funs_arg Additional parameters for `funs`, e.g. `digits` (the number of decimal places) for the default [cross_summary()]. Ultimately, these arguments are passed to [format_fixed()].
#' @param total one of \["none", "row", "column" or "both"] to indicate whether to add total rows and/or columns. Default to `none`.
#' @param percent_pattern Pattern used to describe proportions in categorical data. Use the [glue::glue()] syntax with any variable in `c("n", "p_row", "p_coll", "p_cell")`. Default to `"{n} ({p_col})"` if `by` is null and `"{n} ({p_row})"` if it is not.
#' @param percent_digits Number of digits for percentages
#' @param unique_numeric The number of non-missing different levels a variable should have to be considered as numeric
#' @param showNA Whether to show NA in categorical variables (one of \code{c("ifany", "always", "no")}, like in \code{table()})
#' @param label Whether to show labels. See [import_labels()] or [set_label()]for how to add labels to the dataset columns.
#' @param cor_method One of `c("pearson", "kendall", "spearman")` to indicate which correlation coefficient is to be used.
#' @param times When using formula with [survival::Surv()] objects, which times to summarize
#' @param followup When using formula with [survival::Surv()] objects, whether to display follow-up time
#' @param test Whether to perform tests
#' @param test_args See \code{\link{crosstable_test_args}} to override default testing behaviour.
#' @param effect Whether to compute a effect measure
#' @param effect_args See \code{\link{crosstable_effect_args}} to override default behaviour.
#' @param margin Deprecated in favor of `percent_pattern`. One of \["row", "column", "cell", "none", or "all"]. Default to `row`.
#' @param .vars Deprecated
#' @inheritParams format_fixed
#' 
#' @author Dan Chaltiel
#' @export
#' @importFrom checkmate makeAssertCollection reportAssertions assert_data_frame assert_count assert_string assert_logical assert_list assert_subset assert_choice
#' @importFrom rlang quos enquos enquo expr quo_is_null is_null is_quosures is_formula is_string is_empty is_lambda as_function set_env quo_squash caller_env warn abort quo_is_missing
#' @importFrom tidyselect vars_select eval_select everything any_of 
#' @importFrom dplyr select mutate_if n_distinct across
#' @importFrom purrr map map_lgl map_chr map_dfc pmap_dfr
#' @importFrom stringr str_detect str_split
#' @importFrom glue glue
#' @importFrom ellipsis check_dots_unnamed
#' @importFrom lifecycle deprecated is_present deprecate_warn deprecate_stop
#' @importFrom stats model.frame
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
#'            funs=c(mean, quantile), funs_arg = list(probs=c(.25,.75)))
#' 
#' #margin and totals, multiple by
#' crosstable(mtcars2, c(disp, cyl), by=c(am, vs), 
#'            margin=c("row", "col"), total = "both")
#' 
#' #predicate selection, correlation, testing
#' crosstable(mtcars2, where(is.numeric), by=hp, test=TRUE)
#' 
#' #lambda selection & effect calculation
#' crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, effect=TRUE)
#' 
#' #Dates
#' mtcars2$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
#' crosstable(mtcars2, my_date, by=vs, date_format="%d/%m/%Y")
#' 
#' #Survival data (using formula syntax)
#' library(survival)
#' crosstable(aml, Surv(time, status) ~ x,times=c(0,15,30,150), followup=TRUE)
crosstable = function(data, cols=NULL, ..., by=NULL, 
                      total = c("none", "row", "column", "both"),
                      percent_pattern = "{n} ({p_row})", percent_digits = 2, 
                      showNA = c("ifany", "always", "no"), label = TRUE, 
                      funs = c(" " = cross_summary), funs_arg=list(), 
                      cor_method = c("pearson", "kendall", "spearman"), 
                      unique_numeric = 3, date_format=NULL, 
                      times = NULL, followup = FALSE, 
                      test = FALSE, test_args = crosstable_test_args(), 
                      effect = FALSE, effect_args = crosstable_effect_args(), 
                      margin = c("row", "column", "cell", "none", "all"), 
                      .vars) {
    debug=list()
    byname = vars_select(names(data), !!!enquos(by))
    
    # Arguments checks ----------------------------------------------------
    check_dots_unnamed()
    
    coll = makeAssertCollection()    
    assert_data_frame(data, null.ok=FALSE, add=coll)
    dataCall = deparse(substitute(data))
    data = as.data.frame(data)
    assert_string(percent_pattern, add=coll)
    assert_count(percent_digits, add=coll)
    assert_logical(label, add=coll)
    assert_logical(followup, add=coll)
    assert_logical(test, add=coll)
    assert_logical(effect, add=coll)
    assert_list(funs_arg, add=coll)
    if(isFALSE(showNA)) showNA="no"
    if(isTRUE(showNA)) showNA="always"
    showNA = match.arg(showNA)
    cor_method = match.arg(cor_method)

    if(!missing(margin)){
        if(length(margin)>3){
            abort(c("Margin should be of max length 3", 
                    i=glue("margin={paste0(margin, collapse=', ')}")), 
                  class="XXX") #TODO TODO!
        }
        if(missing(percent_pattern)) {
            percent_pattern = get_percent_pattern(margin)
        } else {
            warn(c("Argument `margin` is ignored if `percent_pattern` is set.", 
                   i=glue("`margin`={margin}"), 
                   i=glue("`percent_pattern`={percent_pattern}")), #TODO warning
                 class="xxxx")
        }
    }
    
    if(missing(total) || is.null(total)) total = 0
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
    
    if(!is.null(date_format)) funs_arg = c(funs_arg, list(date_format=date_format))
    
    autotesting_verbosity = getOption("crosstable_verbosity_autotesting", "default")
    if(test==T && autotesting_verbosity!="quiet"){
        if(autotesting_verbosity=="verbose") freq = "always"
        else freq = "regularly"
        warn("Be aware that automatic global testing should only be done in an exploratory context, as it would cause extensive alpha inflation otherwise.", 
             class="crosstable_autotesting_warning",
             .frequency = freq, .frequency_id="crosstable_global_testing")
    }
    
    # Deprecations --------------------------------------------------------
    if (!missing(...)) {
        dotsCall = as.character(substitute(list(...))[-1L]) %>% paste(collapse=", ")
        colsCall = as.character(substitute(cols)) %>% paste(collapse=", ")
        bad = glue("`crosstable({dataCall}, {colsCall}, {dotsCall}, ...)`")
        good = glue("`crosstable({dataCall}, c({colsCall}, {dotsCall}), ...)`")
        deprecate_warn("0.2.0", "crosstable(...=)", "crosstable(cols=)", 
                       details=glue("Instead of {bad}, write {good}"))
    }
    if (!missing(.vars)) {
        deprecate_stop("0.2.0", "crosstable(.vars=)", "crosstable(cols=)")
        vardots= c(enquos(.vars), enquos(...))
    }
    
    # Logic handle --------------------------------------------------------
    if(!exists("vardots"))
        vardots= c(enquos(cols), enquos(...))
    
    is_form = tryCatch(suppressWarnings(is_formula(cols)),error=function(e) FALSE)
    is_lamb = tryCatch(suppressWarnings(is_lambda(as_function(cols))), error=function(e) FALSE)
    
    if(is_form && !is_lamb){
        debug$interface="formula"
        if(length(enquos(...))>0) {
            abort("You cannot use additional arguments through ellipsis (`...`) when using formulas with crosstable. Please include them in the formula or use another syntax.", 
                  class="crosstable_formula_ellipsis_error")
        }
        if(!is_empty(byname)){
            abort(c("`by` argument cannot be used with the formula interface. Please include it in the formula or use another syntax.",
                    i=paste("formula = ", format(cols)),
                    i=paste("by = ", paste(as.character(byname), collapse=", "))), 
                  class="crosstable_formula_by_error")
        }
        data_x = model.frame(cols[-3], data, na.action = NULL)
        data_y = model.frame(cols[-2], data, na.action = NULL)
        byname = names(data_y)
    } else {
        debug$interface="quosure"
        if(vardots %>% map_lgl(quo_is_null) %>% all) 
            vardots=quos(everything())
        
        target_env = caller_env()
        vardots2=vardots %>% 
            map(quo_squash) %>% 
            map(function(.f){
                try({attr(.f, ".Environment") = target_env}, silent = TRUE)
                if (!is_quosures(.f) && is_formula(.f) && length(.f) <= 2 && is_lambda(as_function(.f))){
                    .f = as_function(.f)
                }
                set_env(enquo(.f), target_env)
            }) 
        xloc = eval_select(expr(c(!!!vardots2)), data = data)
        data_x = data %>% select(any_of(xloc)) %>% as.data.frame()
        data_y = data %>% select(any_of(byname)) %>% as.data.frame()
    }
    
    one_col_dummy = ncol(data_y)==1 && length(unique(data_y[[1]]))==1
    if(missing(percent_pattern) && length(byname)==0 || one_col_dummy) {
        percent_pattern = "{n} ({p_col})"
    }
    
    
    duplicate_cols = intersect(byname, names(data_x))
    
    verbose_duplicate_cols = getOption("crosstable_verbose_duplicate_cols", FALSE)
    if(length(duplicate_cols)>0 && verbose_duplicate_cols){
        warn(c("Some columns were selected in `by` and in `cols` and were removed from the latter.", 
               i=glue("Columns automatically removed from `cols`: [{x}]", 
                      x=paste(duplicate_cols, collapse=", "))), 
             class="crosstable_duplicate_cols_warning")
    }
    
    data_x = select(data_x, -any_of(byname))
    ncol_x = if(is.null(data_x)) 0 else ncol(data_x)
    ncol_y = if(is.null(data_y)) 0 else ncol(data_y)
    
    # Data-management -----------------------------------------------------
    if(ncol_x>0){
        data_x = data_x %>% mutate(
            across(where(is.logical),
                   ~ .x %>% as.character() %>% set_label(get_label(.x))),
            across(where(~is.numeric.and.not.surv(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric),
                   ~{
                       .x = as.character(.x) %>% set_label(get_label(.x))
                       class(.x) = c("unique_numeric", "character")
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
                       .x = as.character(.x) %>% set_label(get_label(.x))
                       class(.x) = c("unique_numeric", "character")
                       .x
                   })
        )
    }
    # Return checks -------------------------------------------------------
    
    if(ncol_x==0) {
        warn("Variable selection in crosstable ended with no variable to describe",
             class="crosstable_empty_warning")
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
            warn("Crosstable() cannot add total in rows if `by` is NULL",
                 class="crosstable_totalrow_bynull")
        } 
    }
    
    ## At least 1 BY ----
    if(ncol_y>0 && all(is.na(data_y))){
        abort(glue("`by` columns ({names}) contains only missing values", 
                   s=if(ncol(data_y)>1) "s" else "", 
                   names=paste(names(data_y), collapse=", ")),
              class="crosstable_by_only_missing_error")
    }
    
    ## BY one ----
    if(ncol_y==1){
        y_var = data_y[[1]]
        if(!is.numeric.and.not.surv(y_var) && !is.character.or.factor(y_var)){
            abort(c("Crosstable only supports numeric, logical, character or factor `by` columns.",
                    i=glue("`by` was pointing to the column '{y}' ({yy})",
                           y=names(data_y), yy=paste_classes(y_var))),
                  class="crosstable_wrong_byclass_error")
        }
        if(is.numeric(y_var)){
            if(!identical(funs, c(` `=cross_summary)) || length(funs_arg)>0){
                warn("`funs` and `funs_arg` arguments will not be used if `by` is numeric.",
                     class="crosstable_funs_by_warning")
            }
        }
    }
    
    ## multi BY ----
    if(ncol_y>1) {
        
        #Missing values
        na_cols = purrr::keep(data_y, ~all(is.na(.x))) %>% names()
        if(all(is.na(data_y))){
            stop("This should never happen, contact the developper. Code=13547")
        } else if(length(na_cols)>0){
            warn(c("Some `by` columns contains only missing values and were removed.",
                   i=glue("Automatically removed columns: [{x}]",
                          x=paste(na_cols, collapse=", "))),
                 class="crosstable_multiby_some_missing_warning")
            data_y = select(data_y, -any_of(na_cols))
        }
        
        
        #supported classes
        data_y2 = map_dfc(data_y, ~{if(!is.logical(.x)&&!is.character.or.factor(.x)) NULL else .x})
        nameclass_diff = setdiff(paste_nameclasses(data_y), paste_nameclasses(data_y2))
        if(length(nameclass_diff)>0){
            message = "Crosstable only supports logical, character or factor `by` columns (multiple)."
            if(ncol(data_y2)==0){
                abort(c(message,
                        i="All columns were automatically removed from `by`:",
                        i=glue("[{x}]",x=paste(nameclass_diff, collapse=", "))),
                      class="crosstable_multiby_wrong_class_error")
            } else {
                warn(c(message,
                       i="Columns automatically removed from `by`:",
                       i=glue("[{x}]",x=paste(nameclass_diff, collapse=", "))),
                     class="crosstable_multiby_wrong_class_warning")
            }
        }
        data_y = data_y2
        
        
        #tests and effects
        if(test==TRUE) {
            warn("Cannot perform tests with multiple `by` strata.",
                 class="crosstable_multiby_test_warning")
            test=FALSE
        }
        if(effect==TRUE) {
            warn("Cannot compute effects with multiple `by` strata.",
                 class="crosstable_multiby_effect_warning")
            effect=FALSE
        }
    }
    
    
    x_class = map_chr(data_x, ~paste_classes(.x))
    y_class = map_chr(data_y, ~paste_classes(.x))
    multiby = !is.null(data_y) && ncol(data_y)>1
    
    # Function call -------------------------------------------------------
    by_levels = data_y %>% map(~{if(is.numeric(.x)) NULL else sort(unique(as.character(.x)), na.last=TRUE)})
    if(showNA=="no") by_levels = map(by_levels, ~.x[!is.na(.x)])
    funs = parse_funs(funs)
    if(multiby){
        data_y_lvl = expand.grid(by_levels, stringsAsFactors=FALSE) %>%
            imap_dfr(~ paste(.y, .x, sep="=")) %>%
            unite(col="y", sep=" & ") %>% pull()
        
        data_y2 = data_y %>%
            imap_dfr(~ paste(.y, .x, sep="=")) %>%
            unite(col="y", sep=" & ") %>%
            mutate(y=factor(y, levels=data_y_lvl))
        
        rtn = cross_by(data_x=data_x, data_y=data_y2, funs=funs, funs_arg=funs_arg,
                       percent_pattern=percent_pattern, total=total, 
                       percent_digits=percent_digits, showNA=showNA,
                       cor_method=cor_method, times=times, followup=followup, test=test, test_args=test_args,
                       effect=effect, effect_args=effect_args, label=label)
        
        class(rtn) = c("crosstable_multiby", "crosstable", "tbl_df", "tbl", "data.frame")
        
    } else {
        rtn = cross_by(data_x=data_x, data_y=data_y, funs=funs, funs_arg=funs_arg,
                       percent_pattern=percent_pattern, percent_digits=percent_digits, 
                       total=total, showNA=showNA,
                       cor_method=cor_method, times=times, followup=followup, test=test, test_args=test_args,
                       effect=effect, effect_args=effect_args, label=label)
        class(rtn) = c("crosstable", "tbl_df", "tbl", "data.frame")
    }
    
    # Attributes and return -----------------------------------------------
    debug$x_class = x_class
    debug$y_class = y_class
    attr(rtn, "debug") = debug
    attr(rtn, "showNA") = showNA
    attr(rtn, "variables") = names(data_x)
    attr(rtn, "has_test") = test
    attr(rtn, "has_effect") = effect
    attr(rtn, "has_total") = total
    attr(rtn, "has_label") = label
    if(is_null(byname) || is_empty(byname)){
        attr(rtn, "by") = NULL
        attr(rtn, "by_label") = NULL
        attr(rtn, "by_levels") = NULL
    } else {
        attr(rtn, "by") = byname
        attr(rtn, "by_label") = get_label(data_y)
        attr(rtn, "by_levels") = by_levels
    }
    return(rtn)
}


