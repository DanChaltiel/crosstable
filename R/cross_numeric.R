
#' @keywords internal
#' @importFrom expss unlab
#' @importFrom glue glue
#' @importFrom rlang :=
#' @importFrom dplyr rename select everything .data
#' @noRd
cross_numeric = function(data_x, data_y, funs, funs_arg, showNA, total,
                         label, cor_digits, cor_method,  test, test_args, effect, effect_args) {
    stopifnot(ncol(data_x)==1 && (is.null(data_y) || ncol(data_y)==1))
    stopifnot(is.numeric(data_x[[1]]))
    stopifnot(is.character(funs))
    
    x_name = get_label(data_x, label)
    y_name = get_label(data_y, label)

    if(is.null(data_y)){
        rtn = summarize_numeric_single(data_x[[1]],funs,funs_arg)
    } else if(is.character.or.factor(data_y[[1]])) {
        rtn = summarize_numeric_factor(data_x[[1]], data_y[[1]], funs, funs_arg, showNA, total, 
                                       cor_digits, cor_method,  test, test_args, 
                                       effect, effect_args)
    } else if(is.numeric.and.not.surv(data_y[[1]])){
        if(!identical(funs,c(` `="cross_summary")))
            warning("`funs` argument will not be used if `by` is numeric.")
        rtn = summarize_numeric_numeric(data_x[[1]], data_y[[1]], method=cor_method, 
                                        digits=cor_digits, test=test, test_args) %>% 
            rename(!!y_name:=.data$value)
    } else {
        return(NULL)
    }
    
    rtn = rtn %>% 
        mutate(.id=names(data_x), label=x_name) %>% 
        select(.data$.id, .data$label, everything()) %>% 
        mutate_all(as.character)
        
    rtn
}

