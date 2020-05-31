
#' Easily describe datasets
#' 
#' @param data a data.frame
#' @param .vars the variables to describe. Can be a character or name vector, a tidyselect helper, a (lambda) function that returns a logical, or a formula. See examples or `vignette("crosstable-selection")` for more details.
#' @param ... more variables to describe. Cannot be a lambda function nor a formula.
#' @param by the variable to group on. Character or name.
#' @param funs functions to apply to numeric variables. Default to \code{\link{cross_summary}}.
#' @param funs_arg additionnal parameters for \code{funs}. With \code{cross_summary}, you can set \code{dig}, the number of decimal places.
#' @param margin one of \["row", "column", "cell", "none" or "all"] to indicate which proportions should be computed in frequency tables. Default to `row`.
#' @param total one of \["none", "row", "column" or "both"] to indicate whether to add margins. Default to `none`.
#' @param percent_digits number of digits for percentages
#' @param showNA whether to show NA in factors (one of \code{c("ifany", "always", "no")}, like in \code{table()})
#' @param label whether to show labels. Use either [Hmisc::label()], [expss::var_lab()] or \code{expss::apply_labels} to add labels to the dataset columns.
#' @param test whether to perform tests
#' @param test_args See \code{\link{crosstable_test_args}} to override default testing behaviour.
#' @param cor_method one of \["pearson", "kendall", or "spearman"] to indicate which correlation coefficient is to be used.
#' @param effect whether to compute a effect measure
#' @param effect_args See \code{\link{crosstable_effect_args}} to override default behaviour.
#' @param times when using formula with [survival::Surv()] objects, which times to summarize
#' @param followup when using formula with [survival::Surv()] objects, whether to display follow-up time
#' @param unique_numeric the number of non-missing different levels a variable should have to be considered as numeric
#'
#' @export
#' @import checkmate rlang
#' @importFrom tidyselect vars_select eval_select everything any_of
#' @importFrom dplyr select mutate_if n_distinct
#' @importFrom checkmate makeAssertCollection
#' @importFrom purrr map map_lgl map_chr
#' @importFrom stringr str_detect
#' @importFrom expss unlab set_var_lab var_lab apply_labels
#' @importFrom glue glue
#' @importFrom ellipsis check_dots_unnamed
#' @importFrom stats model.frame
#' 
#' @examples
#' #whole table
#' crosstable(iris)
#' crosstable(mtcars)
#' crosstable(mtcars2)
#' 
#' #tidyselection, custom functions
#' library(tidyverse)
#' crosstable(mtcars2, ends_with("t"), starts_with("c"), by=vs, 
#'            funs=c(mean, quantile), funs_arg = list(probs=c(.25,.75)))
#' 
#' #margin and totals
#' crosstable(mtcars2, disp, vs, by=am, 
#'            margin=c("row", "col"), total = "both")
#' 
#' #predicate selection, correlation, testing
#' crosstable(mtcars2, is.numeric, by=hp, test=TRUE)
#' 
#' #lambda selection, effect calculation
#' crosstable(mtcars2, ~is.numeric(.x) && mean(.x)>50, by=vs, effect=TRUE)
#' 
#' 
#' #Survival data (using formula UI)
#' library(survival)
#' crosstable(aml, Surv(time, status) ~ x,times=c(0,15,30,150), followup=TRUE)
crosstable = function(data, .vars=NULL, ..., by=NULL, 
                      margin = c("row", "column", "cell", "none", "all"), 
                      total = c("none", "row", "column", "both"),
                      percent_digits = 2, showNA = c("ifany", "always", "no"), label = TRUE, 
                      funs = c(" " = cross_summary), funs_arg=list(), 
                      test = FALSE, test_args = crosstable_test_args(), 
                      cor_method = c("pearson", "kendall", "spearman"), 
                      unique_numeric = 3,
                      effect = FALSE, effect_args = crosstable_effect_args(), 
                      times = NULL, followup = FALSE) {
    debug=list()
    # Arguments checks ****************************************************
    if(TRUE){
        check_dots_unnamed()
        coll = makeAssertCollection()    
        assertDataFrame(data, null.ok=TRUE, add=coll)
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
            abort("You cannot use additional arguments through `...` when using formulas with crosstable. Please include them in the formula or use another pattern.")
        if(!is_empty(byname))
            abort(c("`by` argument is ignored when using formula. Please include it in the formula or use another pattern.",
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
    data_x = data_x %>% 
        mutate_if(is.logical, 
                  ~.x %>% as.character() %>% set_var_lab(var_lab(.x))) %>% 
        mutate_if(~is.numeric(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric, 
                  ~.x %>% as.character() %>% set_var_lab(var_lab(.x)))
    data_y = data_y %>% 
        mutate_if(is.logical, 
                  ~.x %>% as.character() %>% set_var_lab(var_lab(.x))) %>% 
        mutate_if(~is.numeric(.x) && n_distinct(.x, na.rm=TRUE)<=unique_numeric, 
                  ~.x %>% as.character() %>% set_var_lab(var_lab(.x)))
    
    
    
    # Return checks *******************************************************
    if(ncol(data_y)>1) stop("Crosstable does not support multiple `by` columns.")
    if(ncol(data_y)==0) {
        test=FALSE;effect=FALSE
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
                       y=names(data_y[1]), yy=class(unlab(data_y[[1]]))[1]))
        )
    }
    
    x_class = map_chr(data_x, ~paste0(class(unlab(.x)), collapse=', '))
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
        attr(rtn, "by_label") = var_lab(data_y)
        attr(rtn, "by_levels") = if(is.numeric(data_y[[1]])) NULL else unique(as.character(data_y[[1]]))
    }
    return(rtn)
}



