
#' @keywords internal
#' @importFrom glue glue
#' @importFrom rlang :=
#' @importFrom dplyr rename select everything .data
#' @importFrom checkmate assert assert_numeric assert_character
#' @noRd
cross_numeric = function(data_x, data_y, funs, funs_arg, showNA, total,
                         label, cor_digits, cor_method,  test, test_args, effect, effect_args) {
    assert(ncol(data_x)==1)
    assert(is.null(data_y) || ncol(data_y)==1)
    assert_numeric(data_x[[1]])
    
    if(label){
        x_name = get_label(data_x)
        y_name = get_label(data_y)
    } else {
        x_name = names(data_x)
        y_name = names(data_y)
    }

    if(is.null(data_y)){
        rtn = summarize_numeric_single(data_x[[1]], funs, funs_arg)
    } else if(is.character.or.factor(data_y[[1]])) {
        rtn = summarize_numeric_factor(data_x[[1]], data_y[[1]], funs, funs_arg, showNA, total, 
                                       cor_digits, cor_method,  test, test_args, 
                                       effect, effect_args)
    } else if(!is.date(data_x[[1]]) && is.numeric.and.not.surv(data_y[[1]])){
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

