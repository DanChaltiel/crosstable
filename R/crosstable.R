
#' Easily describe datasets
#' 
#' Generate a descriptive table of all chosen columns, as contingency tables for categorical variables and as calculation summaries for numeric variables. If the `by` argument points to a categorical variable, `crosstable` will output a description of all columns for every level. Else, if it points to a numeric variable, `crosstable` will calculate correlation coefficients with all other selected numeric columns. Finally, if it points to a `Surv` object, `crosstable` will describe the survival at different times.
#' 
#' Can be formatted as an HTML table using [as_flextable()].
#' 
#' @param data a data.frame
#' @param .vars the variables to describe. Can be a character or name vector, a tidyselect helper, a (lambda) function that returns a logical, or a formula. See examples or `vignette("crosstable-selection")` for more details.
#' @param ... more variables to describe. Cannot be a lambda function nor a formula.
#' @param by the variable to group on. Character or name.
#' @param funs functions to apply to numeric variables. Default to [cross_summary].
#' @param funs_arg additionnal parameters for `funs`, e.g. `digits` (the number of decimal places) for the default [cross_summary]. Ultimately, these arguments are passed to [format_fixed].
#' @param total one of \["none", "row", "column" or "both"] to indicate whether to add total rows and/or columns. Default to `none`.
#' @param margin one of \["row", "column", "cell", "none" or "all"] to indicate which proportions should be computed in frequency tables. Default to `row`.
#' @param percent_digits number of digits for percentages
#' @param showNA whether to show NA in categorial variables (one of \code{c("ifany", "always", "no")}, like in \code{table()})
#' @param label whether to show labels. See [import_labels] or [set_label]for how to add labels to the dataset columns.
#' @param cor_method one of \["pearson", "kendall", or "spearman"] to indicate which correlation coefficient is to be used.
#' @param test whether to perform tests
#' @param test_args See \code{\link{crosstable_test_args}} to override default testing behaviour.
#' @param effect whether to compute a effect measure
#' @param effect_args See \code{\link{crosstable_effect_args}} to override default behaviour.
#' @param times when using formula with [survival::Surv()] objects, which times to summarize
#' @param followup when using formula with [survival::Surv()] objects, whether to display follow-up time
#' @param unique_numeric the number of non-missing different levels a variable should have to be considered as numeric
#' @inheritParams format_fixed
#' 
#' @export
#' @importFrom checkmate makeAssertCollection assertDataFrame assertCount assertLogical assertList assertSubset assertChoice reportAssertions
#' @importFrom rlang quos enquos enquo expr quo_is_null is_null is_quosures is_formula is_string is_empty is_lambda as_function set_env quo_squash caller_env warn abort 
#' @importFrom tidyselect vars_select eval_select everything any_of
#' @importFrom dplyr select mutate_if n_distinct
#' @importFrom purrr map map_lgl map_chr
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom ellipsis check_dots_unnamed
#' @importFrom stats model.frame
#' 
#' @seealso as_flextable, import_labels
#' 
#' @examples
#' #whole table
#' crosstable(iris)
#' crosstable(mtcars)
#' crosstable(mtcars2)
#' 
#' #tidyselection, custom functions
#' library(dplyr)
#' crosstable(mtcars2, ends_with("t"), starts_with("c"), by=vs, 
#'            funs=c(mean, quantile), funs_arg = list(probs=c(.25,.75)))
#' 
#' #margin and totals
#' crosstable(mtcars2, disp, vs, by=am, 
#'            margin=c("row", "col"), total = "both")
#' 
#' #predicate selection, correlation, testing
#' crosstable(mtcars2, where(is.numeric), by=hp, test=TRUE)
#' 
#' #lambda selection, effect calculation
#' crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, effect=TRUE)
#' 
#' #Dates
#' mtcars2$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
#' crosstable(mtcars2, my_date, by=vs, date_format="%d/%m/%Y")
#' 
#' #Survival data (using formula UI)
#' library(survival)
#' crosstable(aml, Surv(time, status) ~ x,times=c(0,15,30,150), followup=TRUE)
crosstable = function(data, .vars=NULL, ..., by=NULL, 
                      total = c("none", "row", "column", "both"),
                      margin = c("row", "column", "cell", "none", "all"), 
                      percent_digits = 2, showNA = c("ifany", "always", "no"), label = TRUE, 
                      funs = c(" " = cross_summary), funs_arg=list(), 
                      cor_method = c("pearson", "kendall", "spearman"), 
                      test = FALSE, test_args = crosstable_test_args(), 
                      unique_numeric = 3, date_format=NULL, 
                      effect = FALSE, effect_args = crosstable_effect_args(), 
                      times = NULL, followup = FALSE) {
    debug=list()
    # Arguments checks ****************************************************
    if(TRUE){
        check_dots_unnamed()
        
        coll = makeAssertCollection()    
        assertDataFrame(data, null.ok=FALSE, add=coll)
        assertCount(percent_digits, add=coll)
        assertLogical(label, add=coll)
        assertList(funs_arg, add=coll)
        showNA = match.arg(showNA)
        cor_method = match.arg(cor_method)
        funs = clear_funs(funs)
        
        
        
        if (missing(margin)) margin = "row"
        if (isTRUE(margin)) margin = c("row", "col")
        if (is.character(margin)) {
            assertSubset(margin, c("all", "row", "col", "column", "cell", "none"), add=coll)
            if(is.null(margin)) {
                margin=0:2 #defaulting 
            } else {
                marginopts = list(all = 0:2,
                                  row = 1,
                                  col = 2,
                                  column = 2,
                                  cell = 0,
                                  none=-1)
                margin = unname(unlist(marginopts[margin]))
            }
        }
        
        if (missing(total)) total = "none"
        if (isTRUE(total)) total = "both"
        if (is.character(total)) {
            assertChoice(total, c("none", "both", "all", "row", "col", "column"), add=coll)
            if(is.null(total)) {
                total=0 #defaulting
            } else {
                totalopts = list(all = 1:2,
                                 both = 1:2,
                                 row = 1,
                                 col = 2,
                                 column = 2,
                                 none = 0)
                total = unname(unlist(totalopts[total]))
            }
        }
        reportAssertions(coll)
        
        if(!is.null(date_format)) funs_arg = c(funs_arg, list(date_format=date_format))
    }
    
    
    if(test==T){
        warning_once("Be aware that automatic global testing should only be done in an exploratory context, as it would cause extensive alpha inflation otherwise.")
    }
    
    # Logic handle ********************************************************
    byname = vars_select(names(data), !!!enquos(by))
    vardots= c(enquos(.vars), enquos(...))
    
    is_form = tryCatch(suppressWarnings(is_formula(.vars)),error=function(e) FALSE)
    is_lamb = tryCatch(suppressWarnings(is_lambda(as_function(.vars))), error=function(e) FALSE)
    
    if(is_form && !is_lamb){
        debug$interface="formula"
        if(length(enquos(...))>0) 
            abort("You cannot use additional arguments through `...` when using formulas with crosstable. Please include them in the formula or use another syntax.")
        if(!is_empty(byname))
            abort(c("`by` argument is ignored when using formula. Please include it in the formula or use another syntax.",
                   i=paste("formula = ", format(.vars)),
                   i=paste("by = ", paste(as.character(byname), collapse=", "))))
        
        data_y = model.frame(.vars[-2], data, na.action = NULL)
        byname = names(data_y)
        data_x = model.frame(.vars[-3], data, na.action = NULL) %>% 
            select(-any_of(byname))
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
        data_y = data %>% select(any_of(byname)) %>% as.data.frame
        xloc=eval_select(expr(c(!!!vardots2)), data = data)
        data_x = data %>% select(any_of(xloc),-any_of(byname)) %>% as.data.frame
    }
    
    
    
    # Data-management *****************************************************
    # browser()
    data_x = data_x %>% 
        mutate_if(is.logical, 
                  ~.x %>% as.character() %>% set_label(get_label(.x))) %>% 
        mutate_if(~is.numeric(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric, 
                  ~{
                      .x = .x %>% as.character() %>% set_label(get_label(.x))
                      class(.x) = c("character", "unique_numeric")
                      .x
                  })
    data_y = data_y %>% 
        mutate_if(is.logical, 
                  ~.x %>% as.character() %>% set_label(get_label(.x))) %>% 
        mutate_if(~is.numeric(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric, 
                  ~.x %>% as.character() %>% set_label(get_label(.x)))
    
    
    
    # Return checks *******************************************************
    if(ncol(data_y)>1) stop("Crosstable does not support multiple `by` columns.")
    if(ncol(data_y)==0) {
        test=effect=FALSE
        data_y=NULL
    }
    
    if(ncol(data_x)==0) {
        warn(c("Variable selection in crosstable ended with no variable to describe"))
        rtn=data.frame()
        class(rtn) = c("crosstable", "data.frame")
        attr(rtn, "debug") = debug
        return(rtn)
    }
    
    if(!is.null(data_y) && !is.numeric.and.not.surv(data_y[[1]]) && !is.character.or.factor(data_y[[1]])){
        abort(c("Crosstable only supports numeric, logical, character or factor `by` columns.",
                i=glue("`by` was pointing to the column '{y}' ({yy})", 
                       y=names(data_y[1]), yy=class(remove_label(data_y[[1]]))[1])) #TODO paste collapse
        )
    }
    
    x_class = map_chr(data_x, ~paste0(class(remove_label(.x)), collapse=', '))
    y_class = class(data_y[[1]])

    # Function call *******************************************************
    
    rtn = cross_by(data_x=data_x, data_y=data_y, funs=funs, funs_arg=funs_arg,
                   margin=margin, total=total, percent_digits=percent_digits, showNA=showNA,
                   cor_method=cor_method, times=times, followup=followup, test=test, test_args=test_args,
                   effect=effect, effect_args=effect_args, label=label)
        
    
    
    # Attributes and return ***********************************************
    class(rtn) = c("crosstable", "data.frame")
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
        attr(rtn, "by_levels") = if(is.numeric(data_y[[1]])) NULL else unique(as.character(data_y[[1]]))
    }
    return(rtn)
}


